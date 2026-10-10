import { resolveRscript } from "../electron/rscript.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";
import { access, mkdtemp, readdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { Project } from "../electron/project.mjs";
import { RWorker } from "../electron/r-worker.mjs";
import { Pipeline } from "../electron/pipeline.mjs";

const exists = (file) =>
  access(file).then(
    () => true,
    () => false,
  );
const tables = (database) =>
  execFileSync(
    resolveRscript(),
    [
      "-e",
      'con <- DBI::dbConnect(duckdb::duckdb(), commandArgs(TRUE)[1], read_only = TRUE); cat(DBI::dbListTables(con), sep = "\\n"); DBI::dbDisconnect(con, shutdown = TRUE)',
      database,
    ],
    { encoding: "utf8" },
  )
    .split("\n")
    .filter(Boolean);

test(
  "removing a session takes out its recordings, results and reviews, and nothing else",
  { timeout: 240000 },
  async (t) => {
    const dir = await mkdtemp(path.join(tmpdir(), "eyeris-sessions-"));
    const worker = new RWorker();
    const project = await Project.open(
      path.join(dir, "Study.eyeris"),
      worker,
      true,
    );
    const pipeline = new Pipeline(project, { parallel: 2 });
    t.after(async () => {
      pipeline.dispose();
      worker.close();
      project.close();
      await rm(dir, { recursive: true, force: true });
    });
    const asc = execFileSync(
      resolveRscript(),
      ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
      { encoding: "utf8" },
    ).trim();
    // sub-001 has both sessions; sub-002 only the encoding session.
    const id = {};
    for (const [subject, session] of [
      ["001", "enc"],
      ["001", "ret"],
      ["002", "enc"],
    ]) {
      if (!pipeline.snapshot().subjects.some((s) => s.id === subject))
        pipeline.addSubject(subject);
      const state = await pipeline.addRecording(
        { subject, session, task: "memory" },
        asc,
      );
      id[`${subject}/${session}`] = state.recordings.find(
        (r) => r.subject === subject && r.session === session,
      ).id;
    }
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
      report: false,
      database: true,
    };
    // sub-001's job processes both of its sessions; its first recording is
    // the encoding session's.
    await pipeline.enqueue(
      [[id["001/enc"], id["001/ret"]], [id["002/enc"]]],
      settings,
    );
    await assert.rejects(
      () => pipeline.removeSession("enc"),
      /Wait for processing/,
    );
    await pipeline.idle();
    let state = pipeline.snapshot();
    for (const job of state.jobs)
      assert.equal(job.status, "completed", job.error);
    const shared = state.jobs.find((j) => j.recordings.length === 2);
    const only = state.jobs.find((j) => j.recordings.length === 1);
    assert.equal(shared.recording_id, id["001/enc"]);
    const database = path.join(
      project.directory,
      "bids",
      "derivatives",
      "eyeris.eyerisdb",
    );
    const before = tables(database);
    for (const run of ["001_enc", "001_ret", "002_enc"])
      assert.ok(before.some((name) => name.includes(`_${run}_memory_run01`)));

    // Review some epochs of each session, and stop reviewing at one of the
    // session to remove.
    const epochs = (session) =>
      project.list({}).rows.filter((e) => e.session === session);
    const decide = (epoch, status) =>
      project.decision({
        id: epoch.id,
        status,
        stage: "final",
        reviewer: "Tester",
        reason: "",
      });
    decide(epochs("enc")[0], "exclude");
    decide(epochs("ret")[0], "keep");
    project.saveReviewPosition({ epochId: epochs("enc")[1].id, filters: {} });
    for (const session of ["enc", "ret"])
      project.db
        .prepare("INSERT INTO behavior VALUES (?,?,?,?,?,?,?)")
        .run("beh.tsv", "001", session, "memory", "01", 0, "{}");
    const encEpochs = epochs("enc").length;
    const retEpochs = epochs("ret").length;
    assert.ok(encEpochs > 0 && retEpochs > 0);

    assert.throws(() => pipeline.sessionRemoval("pre"), /no ses-pre/);
    assert.deepEqual(pipeline.sessionRemoval("enc"), {
      session: "enc",
      recordings: 2,
      subjects: 2,
      emptied: ["002"],
      jobs: 1,
      epochs: encEpochs,
      reviewed: 1,
    });
    const encSources = project
      .summary()
      .sources.filter((s) => s.name.includes("ses-enc"))
      .map((s) => s.id);
    assert.equal(encSources.length, 2);
    const copies = state.recordings.map((r) => r.file);

    state = await pipeline.removeSession("enc");
    assert.deepEqual(
      state.recordings.map((r) => r.id),
      [id["001/ret"]],
    );
    assert.deepEqual(
      state.subjects.map((s) => s.id),
      ["001"],
    );
    // The job of only removed recordings is gone; the shared job keeps the
    // other session.
    assert.deepEqual(
      state.jobs.map((j) => [j.id, j.recording_id, j.recordings]),
      [[shared.id, id["001/ret"], [id["001/ret"]]]],
    );
    assert.deepEqual(state.lastRemoval && { ...state.lastRemoval, id: "" }, {
      id: "",
      session: "enc",
      recordings: 2,
      subjects: 1,
      leftover: [],
    });
    // Review data of the other session is untouched.
    const summary = project.summary();
    assert.deepEqual(summary.participants, ["001"]);
    assert.equal(summary.counts.total, retEpochs);
    assert.equal(summary.counts.keep, 1);
    assert.equal(summary.counts.exclude, 0);
    assert.ok(summary.sources.every((s) => !encSources.includes(s.id)));
    assert.equal(project.reviewPosition(), null);
    assert.equal(project.undo().id, epochs("ret")[0].id);
    assert.deepEqual(
      project.db
        .prepare("SELECT session FROM behavior")
        .all()
        .map((r) => r.session),
      ["ret"],
    );
    // Files of the removed session are deleted; the rest are kept.
    const at = (...parts) => path.join(project.directory, ...parts);
    assert.deepEqual(
      await Promise.all(copies.map((file) => exists(at(file)))),
      copies.map((file) => file === state.recordings[0].file),
    );
    assert.equal(await exists(at("sourcedata", "sub-002")), true);
    assert.equal(await exists(at("sourcedata", "sub-002", "ses-enc")), false);
    for (const [subject, session, kept] of [
      ["001", "enc", false],
      ["001", "ret", true],
      ["002", "enc", false],
    ])
      assert.equal(
        await exists(
          at("bids", "derivatives", `sub-${subject}`, `ses-${session}`),
        ),
        kept,
        `sub-${subject} ses-${session}`,
      );
    assert.equal(await exists(at("processing", only.id)), false);
    assert.equal(await exists(at("processing", shared.id)), true);
    const sources = await readdir(at("sources"));
    assert.ok(encSources.every((s) => !sources.includes(`${s}.rds`)));
    assert.equal(sources.filter((f) => f.endsWith(".rds")).length, 1);
    const after = tables(database);
    assert.ok(after.length > 0);
    assert.ok(after.every((name) => !/_enc_memory_/.test(name)));
    assert.deepEqual(
      after,
      before.filter((name) => !/_enc_memory_/.test(name)),
    );
    await assert.rejects(() => pipeline.removeSession("enc"), /no ses-enc/);
  },
);
