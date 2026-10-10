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
  assert.equal(single.recordings.length, 3);
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
