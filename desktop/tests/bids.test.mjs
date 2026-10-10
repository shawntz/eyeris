import { test } from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  mkdir,
  readFile,
  readdir,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { Project } from "../electron/project.mjs";
import { Pipeline } from "../electron/pipeline.mjs";
import { scanBids } from "../electron/bids.mjs";

// The start of a macOS AppleDouble file ("._" plus a name), which Finder writes
// beside each file copied to a drive without native metadata support.
const appleDouble = Buffer.concat([
  Buffer.from([0x00, 0x05, 0x16, 0x07, 0x00, 0x02, 0x00, 0x00]),
  Buffer.alloc(120),
]);

async function write(root, file, content = file) {
  await mkdir(path.dirname(path.join(root, file)), { recursive: true });
  await writeFile(path.join(root, file), content);
}
async function open(t, name) {
  const dir = await mkdtemp(path.join(tmpdir(), `eyeris-${name}-`));
  const project = await Project.open(
    path.join(dir, "Study.eyeris"),
    null,
    true,
  );
  const pipeline = new Pipeline(project);
  t.after(async () => {
    pipeline.cancelImport();
    await pipeline.importing?.finished;
    project.close();
    await rm(dir, { recursive: true, force: true });
  });
  return { dir, project, pipeline };
}

test("a BIDS dataset imports every subject's ASC runs in one step", async (t) => {
  const { dir, project, pipeline } = await open(t, "bids");
  const root = path.join(dir, "dataset");
  for (const file of [
    "sub-001/ses-01/eye/sub-001_ses-01_task-memory_run-1_eye.asc",
    "sub-001/ses-01/eye/sub-001_ses-01_task-memory_run-2_eye.asc",
    "sub-001/ses-02/eye/sub-001_ses-02_task-memory_eye.asc",
    "sub-001/ses-01/eye/sub-001_ses-01_task-memory_run-1_rec-b_eye.asc",
    "sub-002/eye/sub-002_task-memory_eye.asc",
    "sub-002/eye/sub-002_task-rest_run-1_eye.ASC",
    "sub-002/eye/notes.txt",
    "sub-002/beh/sub-002_task-memory_beh.tsv",
    "sub-003/eye/sub-003_eye.asc",
    "sub-004/ses-01/eye/sub-005_ses-01_task-memory_eye.asc",
    "sub-004/ses-01/eye/sub-004_ses-02_task-memory_eye.asc",
    "derivatives/sub-009/eye/sub-009_task-memory_eye.asc",
  ])
    await write(root, file);
  await write(root, "dataset_description.json", "{}");
  // Hidden files, such as AppleDouble metadata beside each recording, are
  // neither imported nor listed as skipped.
  for (const file of [
    "sub-001/ses-01/eye/._sub-001_ses-01_task-memory_run-1_eye.asc",
    "sub-002/eye/._sub-002_task-memory_eye.asc",
    "sub-002/eye/.DS_Store",
  ])
    await write(root, file, appleDouble);
  // A run already in the project is never replaced.
  pipeline.addSubject("002");
  await write(dir, "earlier.asc");
  await pipeline.addRecording(
    { subject: "002", session: "01", task: "rest", run: "1" },
    path.join(dir, "earlier.asc"),
  );

  const started = await pipeline.importBids(root);
  assert.equal(started.importing.total, 4);
  await assert.rejects(
    () => pipeline.start([started.recordings[0].id], {}),
    /BIDS import/,
  );
  await assert.rejects(() => pipeline.importBids(root), /current import/);
  await pipeline.importing.finished;
  const state = pipeline.snapshot();
  assert.equal(state.importing, null);
  assert.deepEqual(
    state.subjects.map((s) => s.id),
    ["001", "002"],
  );
  assert.deepEqual(
    state.recordings.map((r) => [r.subject, r.session, r.task, r.run, r.name]),
    [
      ["001", "01", "memory", "01", "sub-001_ses-01_task-memory_run-1_eye.asc"],
      ["001", "01", "memory", "02", "sub-001_ses-01_task-memory_run-2_eye.asc"],
      ["001", "02", "memory", "01", "sub-001_ses-02_task-memory_eye.asc"],
      ["002", "01", "memory", "01", "sub-002_task-memory_eye.asc"],
      ["002", "01", "rest", "01", "earlier.asc"],
    ],
  );
  for (const r of state.recordings.slice(0, 4))
    assert.match(
      await readFile(path.join(project.directory, r.file), "utf8"),
      new RegExp(`${r.name}$`),
    );
  const { lastImport } = state;
  assert.equal(lastImport.added, 4);
  assert.equal(lastImport.subjects, 2);
  assert.equal(lastImport.error, null);
  assert.deepEqual(
    Object.fromEntries(lastImport.skipped.map((f) => [f.file, f.reason])),
    {
      [path.join("sub-003", "eye", "sub-003_eye.asc")]:
        "the filename has no task- entity",
      [path.join(
        "sub-004",
        "ses-01",
        "eye",
        "sub-005_ses-01_task-memory_eye.asc",
      )]: "the filename's sub-005 does not match its folder",
      [path.join(
        "sub-004",
        "ses-01",
        "eye",
        "sub-004_ses-02_task-memory_eye.asc",
      )]: "the filename's ses-02 does not match its folder",
      [path.join("sub-002", "eye", "sub-002_task-rest_run-1_eye.ASC")]:
        "sub-002 already has run 01 for ses-01 task-rest",
      [path.join(
        "sub-001",
        "ses-01",
        "eye",
        "sub-001_ses-01_task-memory_run-1_rec-b_eye.asc",
      )]:
        `${path.join("sub-001", "ses-01", "eye", "sub-001_ses-01_task-memory_run-1_eye.asc")} is also run 01 of sub-001 ses-01 task-memory`,
    },
  );

  // Importing the dataset again adds only new recordings.
  await write(root, "sub-002/eye/sub-002_task-memory_run-2_eye.asc");
  await pipeline.importBids(root);
  await pipeline.importing.finished;
  assert.equal(pipeline.snapshot().lastImport.added, 1);
  assert.equal(pipeline.snapshot().recordings.at(-2).run, "02");

  // A single subject folder can be imported on its own.
  const single = await scanBids(path.join(root, "sub-001"));
  // Duplicate runs are rejected when the import is planned, not when scanning.
  assert.equal(single.recordings.length, 4);
  assert.ok(single.recordings.every((r) => r.subject === "001"));
});

test("a cancelled BIDS import keeps completed recordings and no partial copies", async (t) => {
  const { dir, project, pipeline } = await open(t, "bids-cancel");
  const root = path.join(dir, "dataset");
  const large = Buffer.alloc(20e6, 1);
  for (const run of [1, 2, 3])
    await write(
      root,
      `sub-001/eye/sub-001_task-memory_run-${run}_eye.asc`,
      large,
    );
  await assert.rejects(
    () => pipeline.importBids(path.join(dir, "missing")),
    /ENOENT/,
  );
  await mkdir(path.join(dir, "empty"));
  await assert.rejects(
    () => pipeline.importBids(path.join(dir, "empty")),
    /No EyeLink/,
  );
  const started = await pipeline.importBids(root);
  assert.equal(started.importing.totalBytes, 60e6);
  pipeline.cancelImport();
  await pipeline.importing.finished;
  const { lastImport, recordings } = pipeline.snapshot();
  assert.equal(lastImport.cancelled, true);
  assert.equal(lastImport.error, null);
  assert.equal(recordings.length, lastImport.added);
  const copied = await readdir(path.join(project.directory, "sourcedata"), {
    recursive: true,
  });
  assert.equal(
    copied.filter((f) => f.endsWith(".asc")).length,
    recordings.length,
    "an interrupted copy is removed",
  );
});

test("re-importing replaces recordings that were macOS metadata files", async (t) => {
  const { dir, project, pipeline } = await open(t, "bids-appledouble");
  const db = project.db;
  // The state left by 0.4.1, which imported ._ files and skipped the real ones.
  pipeline.addSubject("01");
  const add = async (id, run, name, content) => {
    const file = path.join(
      "sourcedata/sub-01/ses-enc/task-clamp",
      `run-${run}`,
      name,
    );
    await write(project.directory, file, content);
    db.prepare(
      "INSERT INTO recordings(id, subject, session, task, run, name, file, created_at) VALUES (?, '01', 'enc', 'clamp', ?, ?, ?, 'earlier')",
    ).run(id, run, name, file);
    return file;
  };
  const stale = await add(
    "stale",
    "01",
    "._sub-01_ses-enc_task-clamp_run-01_eyetrack.asc",
    appleDouble,
  );
  // A real recording whose name happens to start with "._" is kept.
  await add("named", "03", "._named.asc", "** CONVERTED FROM EDF");
  db.prepare(
    "INSERT INTO jobs(id, recording_id, status, phase, config, started_at) VALUES ('job', 'stale', 'failed', 'failed', '{}', 'earlier')",
  ).run();
  db.prepare("INSERT INTO job_recordings VALUES ('job', 'stale')").run();
  const root = path.join(dir, "dataset");
  for (const run of ["01", "02", "03"]) {
    const name = `sub-01_ses-enc_task-clamp_run-${run}_eyetrack.asc`;
    await write(root, `sub-01/ses-enc/eye/${name}`, `recording ${run}`);
    await write(root, `sub-01/ses-enc/eye/._${name}`, appleDouble);
  }
  await pipeline.importBids(root);
  await pipeline.importing?.finished;
  let state = pipeline.snapshot();
  assert.equal(state.lastImport.added, 1);
  assert.equal(state.lastImport.replaced, 1);
  assert.deepEqual(
    state.lastImport.skipped.map((f) => f.reason),
    ["sub-01 already has run 03 for ses-enc task-clamp"],
  );
  const runs = Object.fromEntries(state.recordings.map((r) => [r.run, r]));
  assert.equal(runs["01"].id, "stale", "the recording keeps its ID");
  assert.equal(
    runs["01"].name,
    "sub-01_ses-enc_task-clamp_run-01_eyetrack.asc",
  );
  assert.equal(
    await readFile(path.join(project.directory, runs["01"].file), "utf8"),
    "recording 01",
  );
  await assert.rejects(() => readFile(path.join(project.directory, stale)), {
    code: "ENOENT",
  });
  assert.deepEqual(state.jobs[0].recordings, ["stale"], "history is kept");
  assert.equal(
    runs["02"].name,
    "sub-01_ses-enc_task-clamp_run-02_eyetrack.asc",
  );
  assert.equal(runs["03"].id, "named");
  // Importing again changes nothing.
  await pipeline.importBids(root);
  await pipeline.importing?.finished;
  state = pipeline.snapshot();
  assert.equal(state.lastImport.added + state.lastImport.replaced, 0);
  assert.equal(state.recordings.length, 3);
  // A metadata file cannot be added by hand either.
  await assert.rejects(
    () =>
      pipeline.addRecording(
        { subject: "01", session: "enc", task: "clamp" },
        path.join(
          root,
          "sub-01/ses-enc/eye/._sub-01_ses-enc_task-clamp_run-02_eyetrack.asc",
        ),
      ),
    /macOS metadata file/,
  );
});
