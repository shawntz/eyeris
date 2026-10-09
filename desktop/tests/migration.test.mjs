import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { DatabaseSync } from "node:sqlite";
import { Project } from "../electron/project.mjs";
import { Pipeline } from "../electron/pipeline.mjs";

// The schema written by desktop 0.3.x, which allowed one ASC per subject,
// session and task.
function createVersionOne(directory) {
  const db = new DatabaseSync(path.join(directory, "review.sqlite"));
  db.exec(`PRAGMA foreign_keys=ON;
    CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
    CREATE TABLE sources (id TEXT PRIMARY KEY, name TEXT NOT NULL, participant TEXT NOT NULL, imported_at TEXT NOT NULL);
    CREATE TABLE epochs (
      id TEXT PRIMARY KEY, source_id TEXT NOT NULL REFERENCES sources(id), participant TEXT NOT NULL,
      event TEXT NOT NULL, trial TEXT NOT NULL, label TEXT NOT NULL, block TEXT NOT NULL, eye TEXT NOT NULL,
      ordinal INTEGER NOT NULL, missing REAL NOT NULL, meta TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'unreviewed' CHECK(status IN ('unreviewed','keep','exclude')),
      reason TEXT NOT NULL DEFAULT '', reviewer TEXT NOT NULL DEFAULT '', stage TEXT NOT NULL DEFAULT '', updated_at TEXT
    );
    CREATE TABLE actions (
      seq INTEGER PRIMARY KEY AUTOINCREMENT, epoch_id TEXT NOT NULL REFERENCES epochs(id),
      kind TEXT NOT NULL, before_json TEXT NOT NULL, after_json TEXT NOT NULL, at TEXT NOT NULL, undone INTEGER NOT NULL DEFAULT 0
    );
    CREATE TABLE subjects(id TEXT PRIMARY KEY, created_at TEXT NOT NULL);
    CREATE TABLE recordings(id TEXT PRIMARY KEY, subject TEXT NOT NULL REFERENCES subjects(id), session TEXT NOT NULL, task TEXT NOT NULL, name TEXT NOT NULL, file TEXT NOT NULL, created_at TEXT NOT NULL, UNIQUE(subject,session,task));
    CREATE TABLE jobs(id TEXT PRIMARY KEY, recording_id TEXT NOT NULL REFERENCES recordings(id), status TEXT NOT NULL, phase TEXT NOT NULL, config TEXT NOT NULL, started_at TEXT NOT NULL, finished_at TEXT, error TEXT, outputs TEXT NOT NULL DEFAULT '[]');
    INSERT INTO metadata VALUES ('schema', '1');
    INSERT INTO subjects VALUES ('001', '2026-10-01T00:00:00.000Z');
    INSERT INTO recordings VALUES ('rec', '001', '01', 'memory', 'memory.asc', 'sourcedata/sub-001/ses-01/task-memory/memory.asc', '2026-10-01T00:00:00.000Z');
    INSERT INTO jobs VALUES ('job', 'rec', 'completed', 'completed', '{}', '2026-10-01T00:00:00.000Z', '2026-10-01T00:01:00.000Z', NULL, '[]');
    INSERT INTO sources VALUES ('one', 'sub-001_ses-01_task-memory.rds', '001', '2026-10-01T00:01:00.000Z');
    INSERT INTO sources VALUES ('two', 'sub-002_task-rest_run-4.rds', '002', '2026-10-01T00:02:00.000Z');`);
  const epoch = db.prepare(
    "INSERT INTO epochs (id, source_id, participant, event, trial, label, block, eye, ordinal, missing, meta, status) VALUES (?, ?, ?, 'E', '1', 'epoch_probe', ?, 'main', 1, 0, ?, ?)",
  );
  const meta = (block) =>
    JSON.stringify({ eye: "main", label: "epoch_probe", block });
  // A recording with two blocks: eyeris numbers them as runs 01 and 02.
  epoch.run("a", "one", "001", "block_1", meta("block_1"), "keep");
  epoch.run("b", "one", "001", "block_2", meta("block_2"), "unreviewed");
  // One block in a file named for run 4: bidsify labels it run-04.
  epoch.run("c", "two", "002", "block_1", meta("block_1"), "exclude");
  db.close();
}

test("projects from earlier versions gain run labels and several runs per task", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-migration-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const directory = path.join(dir, "Old.eyeris");
  await mkdir(directory);
  createVersionOne(directory);
  let project = await Project.open(directory, null);
  t.after(() => project?.close());
  assert.deepEqual(
    project.db
      .prepare("SELECT id, session, task, run, status FROM epochs ORDER BY id")
      .all()
      .map((e) => ({ ...e })),
    [
      { id: "a", session: "01", task: "memory", run: "01", status: "keep" },
      {
        id: "b",
        session: "01",
        task: "memory",
        run: "02",
        status: "unreviewed",
      },
      { id: "c", session: "", task: "rest", run: "04", status: "exclude" },
    ],
  );
  assert.deepEqual(project.summary().runs, ["01", "02", "04"]);
  assert.equal(project.list({ run: "02" }).rows[0].id, "b");
  const pipeline = new Pipeline(project);
  let state = pipeline.snapshot();
  assert.equal(state.recordings[0].run, "", "earlier recordings keep blocks");
  assert.deepEqual(state.jobs[0].recordings, ["rec"]);
  assert.equal(project.db.prepare("PRAGMA foreign_keys").get().foreign_keys, 1);
  assert.throws(
    () =>
      project.db
        .prepare(
          "INSERT INTO jobs(id, recording_id, status, phase, config, started_at) VALUES ('x', 'missing', 'a', 'a', '{}', 'now')",
        )
        .run(),
    /FOREIGN KEY/,
    "the rebuilt table is still referenced by jobs",
  );
  // The earlier ASC holds at least run 01, so the next run is 02.
  const asc = path.join(dir, "run2.asc");
  await writeFile(asc, "not processed in this test");
  state = await pipeline.addRecording(
    { subject: "001", session: "01", task: "memory" },
    asc,
  );
  assert.deepEqual(
    state.recordings.map((r) => r.run),
    ["", "02"],
  );
  project.close();
  // Reopening an upgraded project is a no-op.
  project = await Project.open(directory, null);
  new Pipeline(project);
  assert.equal(
    project.db.prepare("SELECT value FROM metadata WHERE key='schema'").get()
      .value,
    "2",
  );
  assert.equal(project.list().total, 3);
  project.db.prepare("UPDATE metadata SET value='3' WHERE key='schema'").run();
  project.close();
  project = null;
  await assert.rejects(
    () => Project.open(directory, null),
    /newer version of eyeris/,
  );
});
