import { resolveRscript } from "../electron/rscript.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  mkdir,
  copyFile,
  rm,
  readdir,
  readFile,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { Project } from "../electron/project.mjs";
import { RWorker } from "../electron/r-worker.mjs";
import { Pipeline } from "../electron/pipeline.mjs";

test(
  "ASC → full glassbox → epochs → BIDS, review, saved provenance and reruns",
  { timeout: 180000 },
  async (t) => {
    const dir = await mkdtemp(path.join(tmpdir(), "eyeris-pipeline-"));
    const worker = new RWorker();
    const project = await Project.open(
      path.join(dir, "Study.eyeris"),
      worker,
      true,
    );
    const pipeline = new Pipeline(project);
    t.after(async () => {
      pipeline.disposed = true;
      pipeline.active?.child.kill();
      worker.close();
      project.close();
      await rm(dir, { recursive: true, force: true });
    });
    const asc = execFileSync(
      resolveRscript(),
      ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
      { encoding: "utf8" },
    ).trim();
    assert.throws(() => pipeline.addSubject("../bad"), /letters/);
    pipeline.addSubject("001");
    const state = await pipeline.addRecording(
      { subject: "001", session: "01", task: "memory" },
      asc,
    );
    const recording = state.recordings[0];
    assert.equal(recording.run, "01");
    assert.ok(
      (await readFile(path.join(project.directory, recording.file))).length > 0,
    );
    await assert.rejects(
      () =>
        pipeline.addRecording(
          { subject: "001", session: "01", task: "memory", run: "1" },
          asc,
        ),
      /already has run 01/,
    );
    await assert.rejects(
      () =>
        pipeline.addRecording(
          { subject: "001", session: "01", task: "memory", run: "x" },
          asc,
        ),
      /whole numbers/,
    );
    const settings = {
      glassbox: {
        load_asc: { block: "auto", binocular_mode: "average" },
        lpfilt: { plot_freqz: false },
      },
      epoch: {
        events: "PROBE_START_{trial}",
        limits: [-1, 2],
        label: "probe",
        baseline: false,
      },
      report: true,
      database: true,
    };
    await pipeline.start(recording.id, settings);
    await pipeline.active.done;
    let job = pipeline.snapshot().jobs[0];
    assert.equal(job.status, "completed", job.error);
    assert.ok(project.summary().counts.total > 0);
    const run = path.join(project.directory, "processing", job.id);
    const rds = path.join(run, "sub-001_ses-01_task-memory_run-01.rds");
    assert.ok((await readFile(rds)).length > 0);
    const runtime = JSON.parse(
      await readFile(path.join(run, "runtime.json"), "utf8"),
    );
    assert.equal(runtime.eyeris, "3.3.0");
    const outputs = JSON.parse(job.outputs);
    assert.ok(outputs.some((p) => p.endsWith(".html")));
    assert.ok(outputs.some((p) => p.endsWith(".csv")));
    assert.ok(outputs.some((p) => p.endsWith(".eyerisdb")));
    for (const file of outputs)
      assert.ok(
        (await readFile(path.join(project.directory, "bids", file))).length >=
          0,
      );
    const epoch = project.list().rows[0];
    assert.equal(epoch.run, "01");
    assert.equal(epoch.session, "01");
    assert.equal(epoch.task, "memory");
    assert.equal(epoch.meta.blocks, 1);
    assert.ok((await project.trace(epoch.id, "final")).samples > 0);
    const oldContent = await readFile(
      path.join(
        project.directory,
        "bids",
        outputs.find((p) => p.endsWith(".csv")),
      ),
    );
    await pipeline.start(recording.id, {
      ...settings,
      report: false,
      database: false,
      glassbox: { ...settings.glassbox, zscore: false },
    });
    await pipeline.active.done;
    job = pipeline.snapshot().jobs[0];
    assert.equal(job.status, "completed", job.error);
    assert.deepEqual(
      await readFile(
        path.join(
          project.directory,
          "bids",
          outputs.find((p) => p.endsWith(".csv")),
        ),
      ),
      oldContent,
      "reruns cannot overwrite published data",
    );
    assert.equal(pipeline.snapshot().jobs.length, 2);
    assert.throws(
      () =>
        pipeline.validate({
          ...settings,
          glassbox: {
            downsample: { target_fs: 100 },
            bin: { bins_per_second: 10 },
          },
        }),
      /not both/,
    );
    await pipeline.start(recording.id, { ...settings, report: false });
    const pending = pipeline.active.done;
    pipeline.cancel();
    await pending;
    assert.equal(pipeline.snapshot().jobs[0].status, "cancelled");
    const malformed = path.join(dir, "bad.asc");
    await writeFile(malformed, "not an EyeLink recording");
    await pipeline.addRecording(
      { subject: "001", session: "02", task: "memory" },
      malformed,
    );
    const bad = pipeline.snapshot().recordings.at(-1);
    await pipeline.start(bad.id, {
      ...settings,
      report: false,
      database: false,
    });
    await pipeline.active.done;
    assert.equal(pipeline.snapshot().jobs[0].status, "failed");
  },
);

test(
  "several ASC runs of one session are processed together and reviewed at once",
  { timeout: 180000 },
  async (t) => {
    const dir = await mkdtemp(path.join(tmpdir(), "eyeris-runs-"));
    const worker = new RWorker();
    const project = await Project.open(
      path.join(dir, "Runs.eyeris"),
      worker,
      true,
    );
    const pipeline = new Pipeline(project);
    t.after(async () => {
      pipeline.disposed = true;
      pipeline.active?.child?.kill();
      worker.close();
      project.close();
      await rm(dir, { recursive: true, force: true });
    });
    const demo = execFileSync(
      resolveRscript(),
      ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
      { encoding: "utf8" },
    ).trim();
    // Both runs use the same filename, from separate folders.
    const files = [];
    for (const folder of ["second", "first"]) {
      await mkdir(path.join(dir, folder));
      files.push(path.join(dir, folder, "memory.asc"));
      await copyFile(demo, files.at(-1));
    }
    pipeline.addSubject("001");
    let state = await pipeline.addRecording(
      { subject: "001", session: "01", task: "memory" },
      files,
    );
    assert.deepEqual(
      state.recordings.map((r) => r.run),
      ["01", "02"],
    );
    assert.notEqual(state.recordings[0].file, state.recordings[1].file);
    // New runs continue after the existing ones unless a first run is given.
    state = await pipeline.addRecording(
      { subject: "001", session: "01", task: "memory" },
      files[0],
    );
    assert.equal(state.recordings.at(-1).run, "03");
    state = await pipeline.addRecording(
      { subject: "001", session: "01", task: "memory", run: "7" },
      files[0],
    );
    assert.equal(state.recordings.at(-1).run, "07");
    await assert.rejects(
      () =>
        pipeline.addRecording(
          { subject: "001", session: "01", task: "memory", run: "2" },
          files,
        ),
      /already has run 02, run 03/,
    );
    assert.equal(pipeline.snapshot().recordings.length, 4);
    // A file left by an interrupted add, with no recording, is replaced.
    const orphan = path.join(
      project.directory,
      "sourcedata/sub-001/ses-01/task-memory/run-08/memory.asc",
    );
    await mkdir(path.dirname(orphan), { recursive: true });
    await writeFile(orphan, "partial copy");
    const replaced = await pipeline.addRecording(
      { subject: "001", session: "01", task: "memory", run: "8" },
      files[0],
    );
    const used = replaced.recordings.at(-1);
    assert.equal(used.run, "08");
    assert.deepEqual(await readFile(orphan), await readFile(demo));
    // A recording's file is never replaced, even if its run number changed.
    project.db
      .prepare("UPDATE recordings SET run='09' WHERE id=?")
      .run(used.id);
    await assert.rejects(
      () =>
        pipeline.addRecording(
          { subject: "001", session: "01", task: "memory", run: "8" },
          files[1],
        ),
      { code: "EEXIST" },
    );
    assert.deepEqual(await readFile(orphan), await readFile(demo));
    project.db.prepare("DELETE FROM recordings WHERE id=?").run(used.id);
    const [first, second] = state.recordings;
    const settings = {
      glassbox: {
        load_asc: { block: "auto", binocular_mode: "average" },
        lpfilt: { plot_freqz: false },
      },
      epoch: {
        events: "PROBE_START_{trial}",
        limits: [-1, 2],
        label: "probe",
        baseline: false,
      },
      report: true,
      database: true,
    };
    await pipeline.start([second.id, first.id], settings);
    assert.deepEqual(pipeline.active.recordings, [first.id, second.id]);
    await pipeline.active.done;
    const job = pipeline.snapshot().jobs[0];
    assert.equal(job.status, "completed", job.error);
    assert.deepEqual(job.recordings, [first.id, second.id]);
    const config = JSON.parse(
      await readFile(
        path.join(project.directory, "processing", job.id, "config.json"),
        "utf8",
      ),
    );
    assert.deepEqual(
      config.recordings.map((r) => [r.run, path.basename(r.output)]),
      [
        ["01", "sub-001_ses-01_task-memory_run-01.rds"],
        ["02", "sub-001_ses-01_task-memory_run-02.rds"],
      ],
    );
    // One BIDS folder holds both runs, so the shared session report and
    // database are published with them rather than conflicting.
    const outputs = JSON.parse(job.outputs);
    for (const run of ["run-01", "run-02"])
      assert.ok(
        outputs.some((p) => p.includes(`_${run}_desc-timeseries.csv`)),
        run,
      );
    assert.ok(outputs.some((p) => p.endsWith("sub-001_task-memory.html")));
    assert.ok(outputs.some((p) => p.endsWith(".eyerisdb")));
    const summary = project.summary();
    assert.deepEqual(summary.runs, ["01", "02"]);
    assert.equal(summary.sources.length, 2);
    const all = project.list({ participant: "001" });
    assert.equal(all.total, summary.counts.total);
    assert.deepEqual(
      [...new Set(all.rows.map((e) => e.run))],
      ["01", "02"],
      "runs are reviewed together in run order",
    );
    const runTwo = project.list({ participant: "001", run: "02" });
    assert.equal(runTwo.total * 2, all.total);
    assert.ok(runTwo.rows.every((e) => e.session === "01"));
    assert.ok((await project.trace(runTwo.rows[0].id, "final")).samples > 0);
    // An ASC without a run number (from an earlier version) numbers its own
    // blocks as runs and cannot share a BIDS folder with other runs.
    project.db
      .prepare("UPDATE recordings SET run='' WHERE id=?")
      .run(state.recordings.at(-1).id);
    await assert.rejects(
      () => pipeline.start([first.id, state.recordings.at(-1).id], settings),
      /no run number/,
    );
    await assert.rejects(() => pipeline.start([], settings), /Select/);
    assert.equal(pipeline.active, null);
  },
);
