import { resolveRscript } from "../electron/rscript.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, readdir, readFile, writeFile } from "node:fs/promises";
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
    assert.ok(
      (await readFile(path.join(project.directory, recording.file))).length > 0,
    );
    await assert.rejects(
      () =>
        pipeline.addRecording(
          { subject: "001", session: "01", task: "memory" },
          asc,
        ),
      /already has/,
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
    const rds = path.join(run, "sub-001_ses-01_task-memory.rds");
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
