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
// Event names that differ only in a digit (RTCLR1_S, RTCLR2_S) are different
// events, up to this many; more are numbered events (TRIAL_1 ... TRIAL_80).
const maxNames = 12;
// Summaries are cached in the project; older versions are read again.
export const summaryVersion = 2;
const filename = /\.[A-Za-z][A-Za-z0-9]{1,4}$/;

// Summarize a recording's messages by shape: the text with each run of digits
// replaced by "#", how often it occurs, where it first occurs, and the values
// seen in each position. When a message of several words starts with a name
// containing digits, the names are counted too (heads). As eyelinker reads them
// for eyeris, only messages between a START line and its END are kept, and the
// text is everything after the timestamp.
export async function summarizeMessages(file) {
  const shapes = {};
  let carry = "";
  let order = 0;
  const add = (line) => {
    const space = line.indexOf(" ", 4);
    if (space < 0) return;
    const text = line.slice(space + 1).replace(/\r$/, "");
    if (!text.trim() || systemMessage.test(text)) return;
    const shape = text.replace(/\d+/g, "#");
    let entry = shapes[shape];
    if (!entry) {
      if (Object.keys(shapes).length >= maxShapes) return;
      entry = shapes[shape] = {
        count: 0,
        values: [],
        example: text,
        first: order,
      };
    }
    order += 1;
    entry.count += 1;
    (text.match(/\d+/g) ?? []).forEach((value, i) => {
      const values = (entry.values[i] ??= []);
      if (values.length < maxValues && !values.includes(value))
        values.push(value);
    });
    const head = text.slice(0, text.indexOf(" "));
    if (text.includes(" ") && /[A-Za-z]/.test(head) && /\d/.test(head)) {
      const heads = (entry.heads ??= {});
      if (heads[head]) heads[head][0] += 1;
      else if (Object.keys(heads).length < maxValues)
        heads[head] = [1, order - 1];
      else entry.overflow = true;
    }
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

// A placeholder name that is not one of eyeris' own columns and not used yet.
function uniqueName(name, used) {
  if (reserved.has(name)) name = `${name}_value`;
  let unique = name;
  for (let n = 2; used.has(unique); n++) unique = `${name}${n}`;
  used.add(unique);
  return unique;
}
// Name a placeholder after the last word before it: {trial} after a word
// containing "trial", {stim} after STIM, and so on.
function placeholderName(before, used, fallback) {
  const word = /([A-Za-z]+)[^A-Za-z]*$/.exec(before)?.[1]?.toLowerCase() ?? "";
  return uniqueName(/trial/.test(word) ? "trial" : word || fallback, used);
}

// Infer eyeris event patterns from the message summaries of one or more
// recordings. Messages are compared word by word:
// - a word whose digits change becomes a placeholder. The whole word does when
//   it is a number or a file name, or when what is left of it holds characters
//   eyeris would read as a regular expression (FIX_POSTTRIG 71.jpg becomes
//   FIX_POSTTRIG {stim}); otherwise only its digits do (PROBE_START_{trial}).
// - digits that never change stay as they are (TRIAL_RESULT 1), and so do the
//   names of events that differ only in a digit (RTCLR1_S {stim} and
//   RTCLR2_S {stim} stay apart).
// - messages that differ in one word after the first are grouped (STIM face,
//   STIM house: STIM {stim}).
// Placeholders are {stim} for file names, {trial} when nothing else varies,
// and otherwise named after the word before them. Returns the patterns that
// match more than one message, in the order they first occur.
export function inferPatterns(summaries) {
  const shapes = new Map();
  for (const summary of summaries)
    for (const [shape, entry] of Object.entries(summary)) {
      const merged = shapes.get(shape) ?? {
        count: 0,
        recordings: 0,
        values: [],
        example: entry.example,
        first: Infinity,
        heads: new Map(),
        overflow: false,
      };
      merged.count += entry.count;
      merged.recordings += 1;
      merged.first = Math.min(merged.first, entry.first ?? Infinity);
      entry.values.forEach((values, i) => {
        const all = (merged.values[i] ??= []);
        for (const v of values)
          if (all.length < maxValues && !all.includes(v)) all.push(v);
      });
      for (const [head, [count, first]] of Object.entries(entry.heads ?? {})) {
        const h = merged.heads.get(head) ?? {
          count: 0,
          recordings: 0,
          first: Infinity,
        };
        h.count += count;
        h.recordings += 1;
        h.first = Math.min(h.first, first);
        merged.heads.set(head, h);
      }
      merged.overflow ||= !!entry.overflow;
      shapes.set(shape, merged);
    }
  // Events whose names differ only in a digit are separate events.
  const entries = [];
  for (const [shape, entry] of shapes) {
    const space = shape.indexOf(" ");
    const slots = (shape.slice(0, space).match(/#/g) ?? []).length;
    if (
      space > 0 &&
      slots &&
      entry.heads.size &&
      entry.heads.size <= maxNames &&
      !entry.overflow
    )
      for (const [head, h] of entry.heads)
        entries.push({
          shape: head + shape.slice(space),
          values: entry.values.slice(slots),
          count: h.count,
          recordings: h.recordings,
          first: h.first,
          example: head + entry.example.slice(entry.example.indexOf(" ")),
        });
    else entries.push({ shape, ...entry });
  }
  // Fill in each word: literal when its digits never change, else a
  // placeholder for the whole word or for its digits.
  const patterns = new Map();
  const add = (p) => {
    const pattern = p.words.map((w) => w.text).join(" ");
    const same = patterns.get(pattern);
    if (!same) patterns.set(pattern, { ...p, pattern });
    else {
      same.count += p.count;
      same.recordings = Math.max(same.recordings, p.recordings);
      if (p.first < same.first)
        Object.assign(same, { first: p.first, example: p.example });
    }
  };
  for (const e of entries) {
    const varying = e.values.filter((v) => v.length > 1).length;
    const used = new Set();
    const words = [];
    let slot = 0;
    for (const token of e.shape.split(" ")) {
      const count = (token.match(/#/g) ?? []).length;
      const values = e.values.slice(slot, slot + count);
      slot += count;
      const before = words.map((w) => w.text).join(" ");
      let k = 0;
      // Each word keeps its placeholder names and the text around them, since
      // a message's own text may contain braces.
      if (!values.some((v) => v.length > 1)) {
        const text = token.replace(/#/g, () => values[k++]?.[0] ?? "#");
        words.push({ text, kind: "literal", names: [], literal: text });
      } else if (
        token === "#" ||
        filename.test(token) ||
        special.test(token.replace(/#/g, ""))
      ) {
        const name = filename.test(token)
          ? uniqueName("stim", used)
          : varying === 1
            ? uniqueName("trial", used)
            : placeholderName(before, used, `n${slot}`);
        words.push({
          text: `{${name}}`,
          kind: "whole",
          names: [name],
          literal: "",
        });
      } else {
        const word = { text: "", kind: "digits", names: [], literal: "" };
        for (const part of token.split(/(#)/)) {
          const v = part === "#" ? values[k++] : null;
          if (!v || v.length === 1) {
            word.text += v ? v[0] : part;
            word.literal += v ? v[0] : part;
            continue;
          }
          const name =
            varying === 1
              ? uniqueName("trial", used)
              : placeholderName(`${before} ${word.text}`, used, `n${slot}`);
          word.text += `{${name}}`;
          word.names.push(name);
        }
        words.push(word);
      }
    }
    add({ ...e, words });
  }
  // Group messages that differ in one word after the first.
  const groups = new Map();
  for (const p of patterns.values())
    p.words.forEach((w, i) => {
      if (i === 0 || w.kind === "digits") return;
      const key = `${i}\u0000${p.words.map((x, j) => (j === i ? "\u0000" : x.text)).join(" ")}`;
      groups.set(key, [...(groups.get(key) ?? []), p]);
    });
  const subsumed = new Set();
  for (const [key, members] of groups) {
    const i = Number(key.slice(0, key.indexOf("\u0000")));
    const texts = new Set(members.map((m) => m.words[i].text));
    if (texts.size < 2) continue;
    const words = members[0].words;
    const used = new Set(words.flatMap((w, j) => (j === i ? [] : w.names)));
    const whole = members.find((m) => m.words[i].kind === "whole");
    const name = whole
      ? whole.words[i].names[0]
      : members.some((m) => filename.test(m.words[i].text))
        ? uniqueName("stim", used)
        : placeholderName(
            words
              .slice(0, i)
              .map((w) => w.text)
              .join(" "),
            used,
            "value",
          );
    const grouped = words.map((w, j) =>
      j === i
        ? { text: `{${name}}`, kind: "whole", names: [name], literal: "" }
        : w,
    );
    const earliest = members.reduce((a, b) => (b.first < a.first ? b : a));
    const total = {
      words: grouped,
      count: members.reduce((n, m) => n + m.count, 0),
      recordings: Math.max(...members.map((m) => m.recordings)),
      first: earliest.first,
      example: earliest.example,
    };
    const pattern = grouped.map((w) => w.text).join(" ");
    // A placeholder already there now also counts the words it stands for.
    if (patterns.has(pattern)) Object.assign(patterns.get(pattern), total);
    else patterns.set(pattern, { ...total, pattern });
    // Many single words are listed only as their group.
    if (texts.size > 6)
      for (const m of members)
        if (m.words[i].kind === "literal") subsumed.add(m.pattern);
  }
  return (
    [...patterns.values()]
      .filter((p) => !subsumed.has(p.pattern))
      .map((p) => ({
        ...p,
        placeholders: p.words.reduce((n, w) => n + w.names.length, 0),
        literal: p.words.map((w) => w.literal).join(" "),
      }))
      .filter((p) => p.count > 1 || p.placeholders)
      // eyeris reads *, { and } as pattern syntax, even in a single message,
      // and the rest of a pattern with placeholders as a regular expression.
      .filter((p) =>
        p.placeholders ? !special.test(p.literal) : !/[*{}]/.test(p.pattern),
      )
      .sort(
        (a, b) =>
          a.first - b.first ||
          b.count - a.count ||
          a.pattern.localeCompare(b.pattern),
      )
      .slice(0, 40)
      .map(({ pattern, count, recordings, example }) => ({
        pattern,
        count,
        recordings,
        example,
      }))
  );
}
