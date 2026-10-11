import { createReadStream } from "node:fs";

// Which eyes a recording has: "left", "right" or "both", or "unknown" with the
// reason. eyeris::load_asc() takes this from eyelinker::read.asc(), which reads
// it from the last line of the whole file whose first field is SAMPLES, or the
// last EVENTS line when there is none: binocular when the line lists both
// LEFT and RIGHT, otherwise the one eye it lists.
export async function recordedEyes(file, { signal } = {}) {
  const last = {};
  // Configuration lines are a handful among millions of samples; search the
  // text for them rather than splitting every line.
  const lines = /(?:^|\n)(SAMPLES|EVENTS)(?=[\t\n\f\r ]|$)[^\n]*/g;
  const find = (text) => {
    lines.lastIndex = 0;
    for (let match; (match = lines.exec(text));) last[match[1]] = match[0];
  };
  let carry = "";
  try {
    for await (const chunk of createReadStream(file, {
      encoding: "latin1",
      highWaterMark: 1 << 20,
      signal,
    })) {
      const text = carry + chunk;
      const end = text.lastIndexOf("\n");
      find(end < 0 ? "" : text.slice(0, end));
      carry = end < 0 ? text : text.slice(end + 1);
    }
  } catch (error) {
    return { eyes: "unknown", error: error.message };
  }
  find(carry);
  const keyword = last.SAMPLES ? "SAMPLES" : "EVENTS";
  const config = last[keyword];
  if (!config)
    return {
      eyes: "unknown",
      error: "No EyeLink SAMPLES or EVENTS line was found.",
    };
  const left = config.includes("\tLEFT");
  const right = config.includes("\tRIGHT");
  if (left && right) return { eyes: "both" };
  if (left) return { eyes: "left" };
  if (right) return { eyes: "right" };
  return {
    eyes: "unknown",
    error: `The last ${keyword} line lists neither the left nor the right eye.`,
  };
}
