import { createReadStream } from "node:fs";

// EyeLink writes its own configuration and calibration messages; they are not
// experiment events. Messages starting with "!" (such as "!MODE" and "!V") are
// also the tracker's or Data Viewer's.
export const systemMessage =
  /^(?:!|ELCL_|(?:GAZE_COORDS|DISPLAY_COORDS|THRESHOLDS|RECCFG|ELCLCFG|CAMERA_LENS_FOCAL_LENGTH|TRACKER_TIME|DRIFTCORRECT|CALIBRATION|VALIDATE|PRESCALER|VPRESCALER|PUPIL_DATA_TYPE|FRAMERATE)\b)/;
// eyeris::epoch() turns {name} into a regular expression group without
// escaping the rest of the pattern, so suggestions avoid these characters.
const special = /[\\.+*?^$()[\]{}|]/;
const reserved = new Set([
  "block",
  "eye",
  "hz",
  "matched_event",
  "matching_pattern",
  "template",
  "text",
  "text_unique",
  "time",
  "time_orig",
  "timebin",
  "type",
]);
const maxShapes = 2000;
const maxValues = 25;

// Summarize a recording's messages by shape: the text with each run of digits
// replaced by "#", how often it occurs, and the values seen in each position.
// As eyelinker reads them for eyeris, only messages between a START line and
// its END are kept, and the text is everything after the timestamp.
export async function summarizeMessages(file) {
  const shapes = {};
  let carry = "";
  const add = (line) => {
    const space = line.indexOf(" ", 4);
    if (space < 0) return;
    const text = line.slice(space + 1).replace(/\r$/, "");
    if (!text.trim() || systemMessage.test(text)) return;
    const shape = text.replace(/\d+/g, "#");
    let entry = shapes[shape];
    if (!entry) {
      if (Object.keys(shapes).length >= maxShapes) return;
      entry = shapes[shape] = { count: 0, values: [], example: text };
    }
    entry.count += 1;
    (text.match(/\d+/g) ?? []).forEach((value, i) => {
      const values = (entry.values[i] ??= []);
      if (values.length < maxValues && !values.includes(value))
        values.push(value);
    });
  };
  // Messages are interleaved with millions of samples; search the text for
  // message and recording boundary lines rather than splitting every line.
  const lines = /(?:^|\n)(MSG|START|END)[\t ][^\n]*/g;
  let recording = false;
  for await (const chunk of createReadStream(file, { encoding: "latin1" })) {
    const text = carry + chunk;
    const end = text.lastIndexOf("\n");
    const complete = end < 0 ? "" : text.slice(0, end);
    carry = end < 0 ? text : text.slice(end + 1);
    lines.lastIndex = 0;
    for (let match; (match = lines.exec(complete));) {
      if (match[1] === "START") recording = true;
      else if (match[1] === "END") recording = false;
      else if (recording) add(match[0].replace(/^\n/, ""));
    }
  }
  if (recording && /^MSG[\t ]/.test(carry)) add(carry);
  return shapes;
}

function placeholderName(before, used, fallback) {
  const word = /([A-Za-z]+)[^A-Za-z]*$/.exec(before)?.[1]?.toLowerCase() ?? "";
  let name = /trial/.test(word) ? "trial" : word || fallback;
  if (reserved.has(name)) name = `${name}_value`;
  let unique = name;
  for (let n = 2; used.has(unique); n++) unique = `${name}${n}`;
  used.add(unique);
  return `{${unique}}`;
}

// Infer eyeris event patterns from the message summaries of one or more
// recordings: digits that vary become placeholders ({trial} when there is one),
// and a last word that varies among otherwise identical messages becomes one
// too (STIM face, STIM house: STIM {stim}). Values that never vary stay
// literal. Returns the patterns that match more than one message, most
// frequent first.
export function inferPatterns(summaries) {
  const shapes = new Map();
  for (const summary of summaries)
    for (const [shape, entry] of Object.entries(summary)) {
      const merged = shapes.get(shape) ?? {
        count: 0,
        recordings: 0,
        values: [],
        example: entry.example,
      };
      merged.count += entry.count;
      merged.recordings += 1;
      entry.values.forEach((values, i) => {
        const all = (merged.values[i] ??= []);
        for (const v of values)
          if (all.length < maxValues && !all.includes(v)) all.push(v);
      });
      shapes.set(shape, merged);
    }
  // Fill in each shape's digits: literal when constant, else a placeholder.
  const patterns = [];
  for (const [shape, entry] of shapes) {
    const varying = entry.values.filter((v) => v.length > 1).length;
    const used = new Set();
    let slot = 0;
    let pattern = "";
    for (const part of shape.split(/(#)/)) {
      if (part !== "#") {
        pattern += part;
        continue;
      }
      const values = entry.values[slot++] ?? [];
      pattern +=
        values.length === 1
          ? values[0]
          : varying === 1
            ? (used.add("trial"), "{trial}")
            : placeholderName(pattern, used, `n${slot}`);
    }
    patterns.push({ ...entry, pattern, placeholders: used.size });
  }
  // Merge messages that differ only in their last word.
  const groups = new Map();
  for (const p of patterns) {
    const words = p.pattern.split(" ");
    if (words.length < 2 || p.placeholders) continue;
    const key = words.slice(0, -1).join(" ");
    groups.set(key, [...(groups.get(key) ?? []), p]);
  }
  for (const [key, members] of groups) {
    if (members.length < 2 || members.length > 12) continue;
    const used = new Set();
    patterns.push({
      pattern: `${key} ${placeholderName(key, used, "value")}`,
      count: members.reduce((n, m) => n + m.count, 0),
      recordings: Math.max(...members.map((m) => m.recordings)),
      example: members[0].example,
      placeholders: 1,
    });
  }
  return (
    patterns
      .filter((p) => p.count > 1 || p.placeholders)
      // eyeris reads *, { and } as pattern syntax, even in a single message.
      .filter((p) =>
        p.placeholders
          ? !special.test(p.pattern.replace(/\{\w+\}/g, ""))
          : !/[*{}]/.test(p.pattern),
      )
      .sort((a, b) => b.count - a.count || a.pattern.localeCompare(b.pattern))
      .slice(0, 40)
      .map(({ pattern, count, recordings, example }) => ({
        pattern,
        count,
        recordings,
        example,
      }))
  );
}
