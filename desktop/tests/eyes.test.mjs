import { resolveRscript } from "../electron/rscript.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { recordedEyes } from "../electron/eyes.mjs";

// R code is given as lines, passed as one line for Rscript on Windows.
const rscript = (code, ...args) =>
  execFileSync(resolveRscript(), ["-e", [code].flat().join(" "), ...args], {
    encoding: "utf8",
  });
const demo = () => rscript("cat(eyeris::eyelink_asc_demo_dataset())").trim();
const binocular = path.resolve("..", "inst", "extdata", "binocular.asc");
const lines = async (file) =>
  (await readFile(file, "latin1")).replace(/\r?\n$/, "").split(/\r?\n/);
// What eyelinker::read.asc() reads for eyeris: "unknown" when it cannot read
// the file or the file lists neither eye.
const eyelinkerEyes = (files) =>
  JSON.parse(
    rscript(
      [
        "eyes <- function(f) tryCatch({",
        "info <- suppressWarnings(eyelinker::read.asc(f))$info;",
        'if (isTRUE(info$left) && isTRUE(info$right)) "both"',
        'else if (isTRUE(info$left)) "left"',
        'else if (isTRUE(info$right)) "right"',
        'else "unknown"',
        '}, error = function(e) "unknown");',
        'cat(jsonlite::toJSON(vapply(commandArgs(TRUE), eyes, "")))',
      ],
      ...files,
    ),
  );

test("eyes are those eyeris::load_asc finds: one eye or both", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-eyes-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  // The left eye alone, from the binocular recording without its right eye.
  const left = path.join(dir, "left.asc");
  await writeFile(
    left,
    (await lines(binocular))
      .filter((l) => !/^[SE](?:FIX|SACC|BLINK) R/.test(l))
      .map((l) => {
        if (/^(?:START|EVENTS|SAMPLES)\s/.test(l))
          return l.replace("\tLEFT\tRIGHT", "\tLEFT");
        if (!/^\d/.test(l)) return l;
        const [time, x, y, pupil, , , , input] = l.split("\t");
        return [time, x, y, pupil, input, "..."].join("\t");
      })
      .join("\n") + "\n",
    "latin1",
  );
  const files = [demo(), binocular, left];
  // As the app read them before, with eyeris itself.
  const expected = JSON.parse(
    rscript(
      [
        "eyes <- function(f) {",
        "x <- suppressWarnings(suppressMessages(",
        'eyeris::load_asc(f, binocular_mode = "both", verbose = FALSE)));',
        'if (!is.null(x$left) && !is.null(x$right)) return("both");',
        'if (isTRUE(x$info$left[1])) return("left");',
        'if (isTRUE(x$info$right[1])) return("right");',
        '"unknown"',
        "};",
        'cat(jsonlite::toJSON(vapply(commandArgs(TRUE), eyes, "")))',
      ],
      ...files,
    ),
  );
  assert.deepEqual(expected, ["right", "both", "left"]);
  const found = await Promise.all(files.map((f) => recordedEyes(f)));
  assert.deepEqual(
    found,
    expected.map((eyes) => ({ eyes })),
  );
});

test("the last SAMPLES line decides, as eyelinker reads it, across chunks and line endings", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-eyes-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const recording = await lines(demo());
  const start = recording.findIndex((l) => l.startsWith("START"));
  const end = recording.findIndex((l) => l.startsWith("END"));
  const header = recording.slice(0, start);
  const block = recording.slice(start, end + 1);
  const files = {};
  // A right-eye block, then a left-eye block whose SAMPLES line, with a CRLF
  // ending, crosses the reader's first 1 MiB chunk. A later EVENTS line, and
  // lines that only contain the word, do not count.
  const before = [...header, ...block].join("\n") + "\n";
  const leftBlock = block.map((l) =>
    /^(?:START|EVENTS|SAMPLES)\s/.test(l) ? l.replace("\tRIGHT", "\tLEFT") : l,
  );
  const samples = leftBlock.findIndex((l) => l.startsWith("SAMPLES"));
  leftBlock[samples] += "\r";
  const offset =
    before.length + leftBlock.slice(0, samples).join("\n").length + 1;
  // A long message moves the SAMPLES line to 4 bytes before 1 MiB.
  leftBlock.splice(
    samples,
    0,
    `MSG\t11242177 ${"x".repeat(2 ** 20 - 4 - offset - 14)}`,
  );
  files.blocks = [
    before,
    leftBlock.join("\n"),
    "\nEVENTS\tGAZE\tLEFT\tRIGHT\tRATE\t1000.00\tTRACKING\tCR\tFILTER\t2",
    "\nSAMPLESX\tGAZE\tRIGHT",
    "\n SAMPLES\tGAZE\tRIGHT",
    "\nMSG\t11355259 SAMPLES\tGAZE\tRIGHT\n",
  ].join("");
  // Without SAMPLES lines, the last EVENTS line decides, here the file's last
  // line, without a line ending.
  files.events = [
    ...[...header, ...block].filter((l) => !l.startsWith("SAMPLES")),
    "EVENTS\tGAZE\tLEFT\tRATE\t1000.00",
  ].join("\n");
  // Neither eye is listed.
  files.neither = [...header, ...block]
    .map((l) =>
      /^(?:START|EVENTS|SAMPLES)\s/.test(l) ? l.replace("\tRIGHT", "") : l,
    )
    .join("\n");
  // No configuration lines at all.
  files.none = [...header, ...block]
    .filter((l) => !/^(?:EVENTS|SAMPLES)\s/.test(l))
    .join("\n");
  const paths = [];
  for (const [name, text] of Object.entries(files)) {
    paths.push(path.join(dir, `${name}.asc`));
    await writeFile(paths.at(-1), text, "latin1");
  }
  const found = await Promise.all(paths.map((f) => recordedEyes(f)));
  assert.deepEqual(
    found.map((r) => r.eyes),
    eyelinkerEyes(paths),
  );
  assert.deepEqual(found, [
    { eyes: "left" },
    { eyes: "left" },
    {
      eyes: "unknown",
      error: "The last SAMPLES line lists neither the left nor the right eye.",
    },
    { eyes: "unknown", error: "No EyeLink SAMPLES or EVENTS line was found." },
  ]);
  assert.equal(files.blocks.indexOf("SAMPLES\tGAZE\tLEFT"), 2 ** 20 - 4);
});

test("a file that cannot be read is unknown, with the reason", async () => {
  const result = await recordedEyes(
    path.join(tmpdir(), "eyeris-missing", "recording.asc"),
  );
  assert.equal(result.eyes, "unknown");
  assert.match(result.error, /ENOENT/);
});
