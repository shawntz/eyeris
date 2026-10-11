import { resolveRscript } from "../electron/rscript.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import {
  summarizeMessages,
  inferPatterns,
  systemMessage,
} from "../electron/events.mjs";

const demo = () =>
  execFileSync(
    resolveRscript(),
    ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
    { encoding: "utf8" },
  ).trim();

test("messages are read as eyelinker reads them, across chunks and line endings", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-messages-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  // Insert messages, including CRLF endings and an EyeLink time offset, among
  // the demo recording's samples so they cross the reader's chunks, and after
  // its END, where eyelinker (and so eyeris) ignores them.
  const lines = (await readFile(demo(), "utf8")).split("\n");
  const samples = lines.findIndex((l) => /^\d/.test(l));
  const extra = [];
  for (let i = 1; i <= 40; i++)
    extra.push(
      `MSG\t${11300000 + i} STIM ${["face", "house", "car"][i % 3]}`,
      `MSG\t${11300000 + i} -12 BLOCK_${1 + (i % 2)}_TRIAL_${i}\r`,
    );
  lines.splice(samples + 500, 0, ...extra.slice(0, 40));
  lines.splice(samples + 30000, 0, ...extra.slice(40));
  const file = path.join(dir, "recording.asc");
  await writeFile(file, lines.join("\n"));
  const summary = await summarizeMessages(file);
  const texts = JSON.parse(
    execFileSync(
      resolveRscript(),
      [
        "-e",
        "cat(jsonlite::toJSON(eyelinker::read.asc(commandArgs(TRUE)[1], samples = FALSE)$msg$text))",
        file,
      ],
      { encoding: "utf8" },
    ),
  );
  const expected = {};
  for (const text of texts.map((t) => t.replace(/\r$/, "")))
    if (!systemMessage.test(text)) {
      const shape = text.replace(/\d+/g, "#");
      expected[shape] = (expected[shape] ?? 0) + 1;
    }
  assert.deepEqual(
    Object.fromEntries(Object.entries(summary).map(([k, v]) => [k, v.count])),
    expected,
  );
  assert.equal(summary["-# BLOCK_#_TRIAL_#"].count, 20);
  assert.equal(summary["ELCL_PROC CENTROID (#)"], undefined);
  assert.equal(summary["STIM face"].example, "STIM face");
});

// A minimal EyeLink ASC recording of the given messages, one per sample.
async function recording(dir, name, messages) {
  const lines = [
    "** CONVERTED FROM test.edf",
    "START\t1000 \tRIGHT\tSAMPLES\tEVENTS",
    "SAMPLES\tGAZE\tRIGHT\tRATE\t1000.00\tTRACKING\tCR\tFILTER\t2",
  ];
  messages.forEach((text, i) =>
    lines.push(
      `MSG\t${1001 + i} ${text}`,
      `${1001 + i}\t512.0\t384.0\t1200.0\t...`,
    ),
  );
  lines.push(`END\t${1001 + messages.length} \tSAMPLES\tEVENTS`);
  const file = path.join(dir, name);
  await writeFile(file, lines.join("\n") + "\n");
  return summarizeMessages(file);
}

test("event patterns name what varies and keep what does not", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-infer-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const trials = [1, 2, 3];
  const first = await recording(dir, "first.asc", [
    ...trials.map((n) => `TRIALID ${n}`),
    ...trials.map((n) => `PROBE_START_${n}`),
    ...trials.map((n) => `BLOCK_${n % 2}_TRIAL_${n}`),
    ...["face", "house", "car", "face", "house", "car"].map((s) => `STIM ${s}`),
    ...trials.map(() => "TRIAL_RESULT 1"),
    ...trials.map((n) => `RESP (${n})`),
    ...trials.map((n) => `ONSET ${n}.png left`),
    "MARK.A",
    "MARK.A",
    "{weird}",
    "{weird}",
    "EXP_START",
  ]);
  const patterns = inferPatterns([first]).map((p) => p.pattern);
  for (const expected of [
    "TRIALID {trial}",
    "PROBE_START_{trial}",
    // "block" is a column eyeris already uses.
    "BLOCK_{block_value}_TRIAL_{trial}",
    "STIM {stim}",
    "TRIAL_RESULT 1",
    // The whole word varies, parentheses and all, so eyeris matches it.
    "RESP {trial}",
    // A file name is a stimulus, extension and all.
    "ONSET {stim} left",
    "MARK.A",
  ])
    assert.ok(patterns.includes(expected), expected);
  // Not matched literally by eyeris, or seen only once.
  for (const excluded of ["RESP ({trial})", "{weird}", "EXP_START"])
    assert.ok(!patterns.includes(excluded), excluded);
  // A few grouped words are also offered one by one.
  for (const word of ["face", "house", "car"])
    assert.ok(patterns.includes(`STIM ${word}`), word);
  // A value constant in each recording but different between them varies.
  const second = await recording(
    dir,
    "second.asc",
    trials.map(() => "TRIAL_RESULT 0"),
  );
  const merged = inferPatterns([first, second]);
  const result = merged.find((p) => p.pattern === "TRIAL_RESULT {trial}");
  assert.deepEqual(
    { count: result.count, recordings: result.recordings },
    { count: 6, recordings: 2 },
  );
  // Many different words are offered only as their group.
  const many = await recording(
    dir,
    "many.asc",
    Array.from({ length: 10 }, (_, i) => `IMAGE ${"abcdefghij"[i]}x`),
  );
  const images = inferPatterns([many]).map((p) => p.pattern);
  assert.deepEqual(images, ["IMAGE {image}"]);
});

test("a run's events are suggested in order, with stimulus files as placeholders", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-run-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  // Shaped like a real run: each trial's events name its stimulus file, some
  // events happen only in some trials, and event names differ by a digit.
  const summaries = [];
  for (const [run, stims] of [
    [1, [260, 187, 310, 227, 71]],
    [2, [120, 33, 260, 9, 187]],
  ]) {
    const messages = [];
    stims.forEach((n, i) => {
      const stim = `${n}.jpg`;
      messages.push(
        `TRIALID ${(run - 1) * 5 + i + 79}`,
        `FIX_PRESTIM ${stim}`,
        `RTCLR1_S ${stim}`,
        `RTCLR1_E ${stim}`,
      );
      if (i % 2 === 0) messages.push(`RTCLR2_S ${stim}`, `RTCLR2_E ${stim}`);
      if (i % 3) messages.push(`TRIG_S ${stim}`, `TRIG_E ${stim}`);
      else messages.push(`DUDTRIG ${stim}`);
      messages.push(
        `FIX_POSTTRIG ${stim}`,
        `PROBE_S ${stim}`,
        `PROBE_E ${stim}`,
        `TRIAL_RESULT ${stim}`,
      );
    });
    messages.push("end_run");
    summaries.push(await recording(dir, `run-${run}.asc`, messages));
  }
  assert.deepEqual(
    inferPatterns(summaries).map((p) => [p.pattern, p.count]),
    [
      ["TRIALID {trial}", 10],
      ["FIX_PRESTIM {stim}", 10],
      ["RTCLR1_S {stim}", 10],
      ["RTCLR1_E {stim}", 10],
      ["RTCLR2_S {stim}", 6],
      ["RTCLR2_E {stim}", 6],
      ["DUDTRIG {stim}", 4],
      ["FIX_POSTTRIG {stim}", 10],
      ["PROBE_S {stim}", 10],
      ["PROBE_E {stim}", 10],
      ["TRIAL_RESULT {stim}", 10],
      ["TRIG_S {stim}", 6],
      ["TRIG_E {stim}", 6],
      ["end_run", 2],
    ],
  );
  const probe = inferPatterns(summaries).find(
    (p) => p.pattern === "FIX_POSTTRIG {stim}",
  );
  assert.equal(probe.example, "FIX_POSTTRIG 260.jpg");
  // Numbered event names, beyond a handful, are a placeholder instead.
  const numbered = await recording(
    dir,
    "numbered.asc",
    Array.from({ length: 20 }, (_, i) => `TRIAL${i + 1} START`),
  );
  assert.deepEqual(
    inferPatterns([numbered]).map((p) => p.pattern),
    ["TRIAL{trial} START"],
  );
});

test("patterns come from a sample of recordings across subjects, read once", async (t) => {
  const { Project } = await import("../electron/project.mjs");
  const { Pipeline } = await import("../electron/pipeline.mjs");
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-patterns-"));
  const project = await Project.open(path.join(dir, "P.eyeris"), null, true);
  const pipeline = new Pipeline(project);
  t.after(async () => {
    pipeline.dispose();
    project.close();
    await rm(dir, { recursive: true, force: true });
  });
  const asc = demo();
  // Two subjects with six runs each: eight recordings are read, four of each.
  for (const subject of ["001", "002"]) {
    pipeline.addSubject(subject);
    await pipeline.addRecording(
      { subject, session: "01", task: "memory" },
      Array(6).fill(asc),
    );
  }
  const ids = pipeline.snapshot().recordings.map((r) => r.id);
  let result = pipeline.eventPatterns(ids);
  assert.deepEqual(
    { read: result.read, pending: result.pending, total: result.total },
    { read: 0, pending: 8, total: 12 },
  );
  await pipeline.readingMessages;
  result = pipeline.eventPatterns(ids);
  assert.deepEqual(
    { read: result.read, pending: result.pending },
    { read: 8, pending: 0 },
  );
  const probe = result.patterns.find(
    (p) => p.pattern === "PROBE_START_{trial}",
  );
  assert.deepEqual(
    {
      count: probe.count,
      recordings: probe.recordings,
      example: probe.example,
    },
    { count: 40, recordings: 8, example: "PROBE_START_22" },
  );
  const sampled = project.db
    .prepare(
      "SELECT r.subject, COUNT(*) AS n FROM recording_messages m JOIN recordings r ON r.id = m.recording_id GROUP BY r.subject",
    )
    .all()
    .map((row) => [row.subject, row.n]);
  assert.deepEqual(sampled, [
    ["001", 4],
    ["002", 4],
  ]);
  // A replaced file is read again.
  const first = pipeline.snapshot().recordings[0];
  project.db
    .prepare("UPDATE recordings SET file=? WHERE id=?")
    .run(`${first.file}.copy`, first.id);
  assert.equal(pipeline.eventPatterns(ids).pending, 1);
  await pipeline.readingMessages;
  assert.equal(pipeline.eventPatterns([]).total, 0);
  // Summaries cached by earlier versions are read again.
  const second = pipeline.snapshot().recordings[1];
  project.db
    .prepare("UPDATE recording_messages SET summary=? WHERE recording_id=?")
    .run(
      JSON.stringify({ "PROBE_START_#": { count: 5, values: [["1"]] } }),
      second.id,
    );
  assert.equal(pipeline.eventPatterns(ids).pending, 1);
  await pipeline.readingMessages;
  assert.equal(pipeline.eventPatterns(ids).pending, 0);
  assert.equal(
    JSON.parse(
      project.db
        .prepare("SELECT summary FROM recording_messages WHERE recording_id=?")
        .get(second.id).summary,
    ).version,
    2,
  );
});
