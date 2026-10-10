import { resolveRscript } from "../electron/rscript.mjs";
import { test } from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  mkdir,
  readFile,
  readdir,
  copyFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { Project, AUTO_REVIEWER } from "../electron/project.mjs";
import { RWorker } from "../electron/r-worker.mjs";
import packageMetadata from "../package.json" with { type: "json" };

const root = fileURLToPath(new URL("../", import.meta.url));
test("R-backed review: identity, traces, decisions, persistence, and lossless export", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-review-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  project = await Project.open(path.join(dir, "review.eyeris"), worker, true);
  const first = path.join(dir, "sub-001_task-memory.rds");
  assert.equal((await project.importFile(first)).count, 3);
  assert.equal((await project.importFile(first)).duplicate, true);
  assert.equal(
    (await project.importFile(path.join(dir, "sub-002_task-memory.rds"))).count,
    3,
  );
  assert.equal(
    (await project.importFile(path.join(dir, "sub-003_task-memory.rds"))).count,
    9,
  );
  assert.equal(project.summary().counts.total, 15);
  await assert.rejects(
    () => project.importFile(path.join(dir, "invalid.rds")),
    /epoched eyeris/,
  );
  assert.equal(
    project.summary().counts.total,
    15,
    "invalid import does not partly commit",
  );
  const rows = project.list({ participant: "001" }).rows;
  assert.equal(rows.length, 3);
  assert.notEqual(
    rows[0].id,
    rows[1].id,
    "repeated events and trial numbers remain distinct",
  );
  assert.equal(project.list({ search: "' OR 1=1 --" }).total, 0);
  assert.equal(project.list({ stage: "pupil_raw" }).total, 15);
  assert.equal(project.list({ stage: "nonexistent" }).total, 0);
  const trace = await project.trace(rows[0].id, "final");
  assert.equal(trace.stage, "pupil_raw_lpfilt");
  assert.equal(trace.samples, 12000);
  assert.ok(trace.time.length < 12000, "reduce plotting payload");
  assert.ok(trace.signal.includes(null), "preserve missing samples");
  assert.ok(trace.signal.includes(9500), "preserve narrow artifact");
  assert.ok(Math.abs(trace.missing - 100 / 12000) < 1e-14);
  const zoom = await project.trace(rows[0].id, "final", [0.74, 0.76]);
  assert.ok(zoom.time.length < 200);
  assert.ok(zoom.signal.includes(9500), "zoom uses full-resolution source");
  await assert.rejects(
    () => project.trace(rows[0].id, "nonexistent"),
    /selected stage/,
  );
  assert.throws(
    () => project.decision({ id: rows[0].id, status: "bad" }),
    /status/,
  );
  project.decision({
    id: rows[0].id,
    status: "keep",
    stage: "final",
    reviewer: "Tester",
    reason: "Clear",
  });
  project.decision({
    id: rows[1].id,
    status: "exclude",
    stage: "pupil_raw",
    reviewer: "Tester",
    reason: 'Artifact, "spike"\nconfirmed',
  });
  assert.equal(project.list({ status: "exclude" }).total, 1);
  assert.equal(project.undo().status, "unreviewed");
  project.decision({
    id: rows[1].id,
    status: "exclude",
    stage: "pupil_raw",
    reviewer: "Tester",
    reason: 'Artifact, "spike"\nconfirmed',
  });
  const savedIds = rows.map((e) => e.id);
  project.close();
  project = await Project.open(path.join(dir, "review.eyeris"), worker);
  assert.deepEqual(
    project.list({ participant: "001" }).rows.map((e) => e.id),
    savedIds,
  );
  assert.deepEqual(project.summary().counts, {
    total: 15,
    keep: 1,
    exclude: 1,
    unreviewed: 13,
  });
  // Moving the original file has no effect on the immutable imported copy.
  await rm(first);
  assert.equal((await project.trace(rows[0].id, "final")).samples, 12000);
  const exportParent = path.join(dir, "exports");
  await mkdir(exportParent);
  const result = await project.export(exportParent);
  const manifest = JSON.parse(
    await readFile(path.join(result.directory, "manifest.json"), "utf8"),
  );
  assert.equal(
    manifest.application,
    `${packageMetadata.name}/${packageMetadata.version}`,
  );
  assert.equal(manifest.decisions.length, 15);
  assert.equal(manifest.history.length, 4);
  assert.equal(manifest.history[2].kind, "undo");
  assert.equal(
    manifest.decisions.find((e) => e.id === rows[1].id).stage,
    "pupil_raw",
  );
  const script = `args <- commandArgs(TRUE); source <- readRDS(args[1]); target <- args[2]; source_df <- source$epoch_probe$block_1; for (i in 1:3) { status <- c('retained','excluded','unreviewed')[i]; files <- list.files(file.path(target, status, 'sub-001'), pattern='[.]rds$', full.names=TRUE); stopifnot(length(files)==1); actual <- readRDS(files[1]); expected <- source_df[seq.int((i-1)*12000+1,i*12000),]; rownames(actual)<-NULL; rownames(expected)<-NULL; stopifnot(identical(actual[,names(expected)], expected), nrow(actual)==12000, length(unique(actual$.review_epoch_id))==1) }; cat('Lossless partitions verified')`;
  const sourceId = rows[0].source_id;
  const verify = path.join(dir, "verify.R");
  // Direct R comparison includes original precision, missing values, all stages and metadata.
  await writeFile(verify, script);
  const output = execFileSync(
    resolveRscript(),
    [
      verify,
      path.join(project.directory, "sources", `${sourceId}.rds`),
      result.directory,
    ],
    { encoding: "utf8" },
  );
  assert.match(output, /Lossless partitions verified/);
  // Tables are named for their subject, task, run and epoch label.
  for (const file of [
    "retained/sub-001/sub-001_task-memory_run-01_epoch-probe.csv",
    "excluded/sub-001/sub-001_task-memory_run-01_epoch-probe.rds",
    "unreviewed/sub-003/sub-003_task-memory_run-07_epoch-probe.csv",
    "unreviewed/sub-003/sub-003_task-memory_run-01_epoch-second.csv",
  ])
    assert.ok(
      (await readFile(path.join(result.directory, file))).length > 0,
      file,
    );
  const exportedCSV = await readFile(
    path.join(result.directory, "decisions.csv"),
    "utf8",
  );
  assert.ok(exportedCSV.includes('"Artifact, ""spike""\nconfirmed"'));
  assert.notEqual(
    (await project.export(exportParent)).directory,
    result.directory,
    "exports never overwrite previous runs",
  );
});

test("BIDS session, task and run labels let runs be reviewed together", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-runs-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  project = await Project.open(path.join(dir, "runs.eyeris"), worker, true);
  for (const name of [
    "sub-002_ses-02_task-memory_run-3.rds",
    "sub-002_task-memory.rds",
    "sub-003_task-memory.rds",
  ])
    await project.importFile(path.join(dir, name));
  assert.deepEqual(project.summary().runs, ["01", "03", "07"]);
  // Without a run entity, numbered blocks name the runs, as in bidsify().
  assert.deepEqual(
    project
      .list({ participant: "003" })
      .rows.map((e) => `${e.block}:${e.run}`)
      .filter((v, i, all) => all.indexOf(v) === i),
    ["block_1:01", "block_7:07"],
  );
  const named = project.list({ run: "03" });
  assert.equal(named.total, 3);
  assert.ok(
    named.rows.every(
      (e) =>
        e.participant === "002" &&
        e.session === "02" &&
        e.task === "memory" &&
        e.block === "block_1",
    ),
  );
  // A participant's runs are listed together, in session and run order.
  assert.deepEqual(
    project.list({ participant: "002" }).rows.map((e) => e.session + e.run),
    ["01", "01", "01", "0203", "0203", "0203"],
  );
  const exportParent = path.join(dir, "exports");
  await mkdir(exportParent);
  const result = await project.export(exportParent);
  const header = (
    await readFile(path.join(result.directory, "decisions.csv"), "utf8")
  ).split("\n")[0];
  assert.match(header, /"participant","session","task","run","label"/);
});

test("large queues page correctly and changed sources cannot inherit decisions", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-scale-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  const projectDir = path.join(dir, "large.eyeris");
  project = await Project.open(projectDir, worker, true);
  assert.equal(
    (await project.importFile(path.join(dir, "sub-large.rds"))).count,
    10001,
  );
  const first = project.list();
  const second = project.list({ offset: 80 });
  assert.equal(first.total, 10001);
  assert.equal(first.rows.length, 80);
  assert.equal(second.rows[0].ordinal, 81);
  assert.equal(project.list({ offset: 10000 }).rows.length, 1);
  const e = first.rows[0];
  project.decision({
    id: e.id,
    status: "keep",
    stage: "final",
    reviewer: "Tester",
    reason: "",
  });
  assert.equal(project.list({ status: "unreviewed" }).total, 10000);
  // The same filename with new bytes is a new source, with fresh decisions.
  await copyFile(
    path.join(dir, "sub-001_task-memory.rds"),
    path.join(dir, "sub-large.rds"),
  );
  assert.equal(
    (await project.importFile(path.join(dir, "sub-large.rds"))).count,
    3,
  );
  assert.equal(project.summary().counts.keep, 1);
  assert.equal(project.summary().counts.unreviewed, 10003);
  // Tampering with the stored copy is detected before plotting or export.
  await writeFile(
    path.join(projectDir, "sources", `${e.source_id}.rds`),
    "changed",
  );
  await assert.rejects(
    () => project.trace(e.id, "final"),
    /source has changed/,
  );
  const dest = path.join(dir, "exports");
  await mkdir(dest);
  await assert.rejects(() => project.export(dest), /source has changed/);
  const { readdir } = await import("node:fs/promises");
  assert.deepEqual(
    await readdir(dest),
    [],
    "failed export leaves no partial output",
  );
});

test("real eyeris pipeline output can be reviewed", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-real-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  const demo = path.join(dir, "sub-demo.rds");
  execFileSync(resolveRscript(), [path.join(root, "r/make-demo.R"), demo], {
    stdio: "pipe",
  });
  project = await Project.open(path.join(dir, "real.eyeris"), worker, true);
  const result = await project.importFile(demo);
  assert.ok(result.count > 0);
  const { rows } = project.list();
  assert.ok(rows[0].meta.stages.length > 2);
  assert.equal(rows[0].meta.blocks, 1);
  assert.equal(rows[0].run, "01");
  for (const stage of rows[0].meta.stages) {
    const trace = await project.trace(rows[0].id, stage);
    assert.equal(trace.time.length, trace.signal.length);
    assert.ok(trace.samples > 0);
  }
});

test("epochs missing too much data are excluded automatically, with the reason", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-auto-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  project = await Project.open(path.join(dir, "auto.eyeris"), worker, true);
  const imported = await project.importFile(
    path.join(dir, "sub-005_task-memory.rds"),
  );
  assert.equal(imported.autoExcluded, 0, "the rule is off by default");
  const [partial, clean, mostly] = project.list().rows;
  assert.ok(
    Math.abs(partial.meta.stageMissing.pupil_raw - 3700 / 12000) < 1e-12,
  );
  assert.ok(
    Math.abs(partial.meta.stageMissing.pupil_raw_lpfilt - 700 / 12000) < 1e-12,
  );
  const byId = (id) => project.list().rows.find((e) => e.id === id);
  // A reviewer's decision is never changed by the rule.
  project.decision({
    id: mostly.id,
    status: "keep",
    stage: "final",
    reviewer: "Tester",
    reason: "",
  });
  let summary = await project.setAutoExclude({
    enabled: true,
    threshold: 25,
    stage: "final",
  });
  assert.equal(summary.autoExcluded, 0, "5.8% at the final stage is kept");
  assert.equal(byId(mostly.id).status, "keep");
  // Measured before interpolation, the first epoch is 30.8% missing.
  summary = await project.setAutoExclude({
    enabled: true,
    threshold: 25,
    stage: "pupil_raw",
  });
  assert.equal(summary.autoExcluded, 1);
  assert.deepEqual(summary.autoExclude, {
    enabled: true,
    threshold: 25,
    stage: "pupil_raw",
  });
  let excluded = byId(partial.id);
  assert.equal(excluded.status, "exclude");
  assert.equal(excluded.reviewer, AUTO_REVIEWER);
  assert.equal(excluded.stage, "pupil_raw");
  assert.equal(
    excluded.reason,
    "Excessive missing data: 30.8% of samples missing in pupil_raw, above the 25% automatic exclusion threshold",
  );
  assert.equal(byId(clean.id).status, "unreviewed");
  // Undo applies to reviewers' decisions, not automatic ones.
  assert.equal(project.undo().id, mostly.id);
  assert.equal(byId(mostly.id).status, "unreviewed");
  assert.equal(byId(partial.id).status, "exclude");
  // Raising the threshold returns epochs below it to unreviewed.
  summary = await project.setAutoExclude({
    enabled: true,
    threshold: 40,
    stage: "pupil_raw",
  });
  assert.equal(byId(partial.id).status, "unreviewed");
  assert.equal(byId(partial.id).reviewer, "");
  assert.equal(byId(mostly.id).status, "exclude");
  assert.match(byId(mostly.id).reason, /60\.0% of samples missing/);
  // A reviewer who marks an automatic exclusion unreviewed overrides it.
  project.decision({
    id: mostly.id,
    status: "unreviewed",
    stage: "final",
    reviewer: "Tester",
    reason: "",
  });
  await project.setAutoExclude({
    enabled: true,
    threshold: 40,
    stage: "pupil_raw",
  });
  assert.equal(byId(mostly.id).status, "unreviewed");
  assert.equal(byId(mostly.id).reviewer, "Tester");
  // New epochs are checked as they are imported.
  await project.setAutoExclude({ enabled: true, threshold: 0, stage: "final" });
  assert.equal(
    (await project.importFile(path.join(dir, "sub-001_task-memory.rds")))
      .autoExcluded,
    3,
  );
  // Epochs indexed before per-stage fractions were stored are recomputed.
  project.db
    .prepare("UPDATE epochs SET meta = json_remove(meta, '$.stageMissing')")
    .run();
  summary = await project.setAutoExclude({
    enabled: true,
    threshold: 25,
    stage: "pupil_raw",
  });
  assert.equal(byId(partial.id).status, "exclude");
  assert.ok(byId(clean.id).meta.stageMissing.pupil_raw > 0);
  // Turning the rule off restores every automatic exclusion.
  summary = await project.setAutoExclude({
    enabled: false,
    threshold: 25,
    stage: "pupil_raw",
  });
  assert.equal(summary.autoExcluded, 0);
  assert.equal(summary.counts.exclude, 0);
  for (const bad of [100, -1, Number.NaN])
    await assert.rejects(
      () =>
        project.setAutoExclude({
          enabled: true,
          threshold: bad,
          stage: "final",
        }),
      /threshold/,
    );
  assert.throws(
    () =>
      project.decision({
        id: clean.id,
        status: "keep",
        stage: "final",
        reviewer: AUTO_REVIEWER,
        reason: "",
      }),
    /reserved/,
  );
  const history = project.db
    .prepare("SELECT kind FROM actions WHERE kind = 'auto'")
    .all();
  assert.ok(history.length >= 4, "automatic changes are audited");
  await project.setAutoExclude({
    enabled: true,
    threshold: 25,
    stage: "pupil_raw",
  });
  const exportParent = path.join(dir, "exports");
  await mkdir(exportParent);
  const result = await project.export(exportParent);
  const manifest = JSON.parse(
    await readFile(path.join(result.directory, "manifest.json"), "utf8"),
  );
  assert.deepEqual(manifest.autoExclude, {
    enabled: true,
    threshold: 25,
    stage: "pupil_raw",
  });
  assert.ok(
    manifest.decisions.some(
      (d) => d.reviewer === AUTO_REVIEWER && d.status === "exclude",
    ),
  );
});

test("review resumes where it was left and exports every subject at once", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-resume-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    await project?.exporting?.done;
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  const projectDir = path.join(dir, "resume.eyeris");
  project = await Project.open(projectDir, worker, true);
  await project.importFile(path.join(dir, "sub-001_task-memory.rds"));
  // The same run processed twice is kept apart by its source.
  await copyFile(
    path.join(dir, "sub-002_task-memory.rds"),
    path.join(dir, "sub-001_task-memory_desc-rerun.rds"),
  );
  await project.importFile(
    path.join(dir, "sub-001_task-memory_desc-rerun.rds"),
  );
  await project.importFile(path.join(dir, "sub-005_task-memory.rds"));
  assert.deepEqual(project.summary().progress, [
    { participant: "001", total: 6, keep: 0, exclude: 0, unreviewed: 6 },
    { participant: "005", total: 3, keep: 0, exclude: 0, unreviewed: 3 },
  ]);
  const queue = project.list().rows;
  const decide = (e, status) =>
    project.decision({
      id: e.id,
      status,
      stage: "final",
      reviewer: "Tester",
      reason: "",
    });
  assert.deepEqual(project.nextUnreviewed({}, null), {
    id: queue[0].id,
    offset: 0,
  });
  decide(queue[0], "keep");
  decide(queue[2], "exclude");
  assert.equal(project.nextUnreviewed({}, queue[0].id).id, queue[1].id);
  assert.equal(project.nextUnreviewed({}, queue[1].id).id, queue[3].id);
  // The search wraps around to earlier epochs.
  assert.equal(project.nextUnreviewed({}, queue.at(-1).id).id, queue[1].id);
  assert.equal(
    project.nextUnreviewed({ participant: "005" }, null).id,
    queue[6].id,
  );
  assert.equal(
    project.nextUnreviewed({ status: "unreviewed" }, queue[1].id).id,
    queue[3].id,
  );
  // Decisions and the review position survive closing the project.
  const position = {
    epochId: queue[3].id,
    filters: {
      status: "all",
      participant: "001",
      run: "",
      search: "",
      stage: "final",
      sort: "natural",
      offset: 0,
    },
  };
  project.saveReviewPosition(position);
  assert.throws(() => project.saveReviewPosition({ epochId: 4 }), /Invalid/);
  project.close();
  project = await Project.open(projectDir, worker);
  assert.deepEqual(project.summary().position, position);
  assert.deepEqual(project.summary().progress[0], {
    participant: "001",
    total: 6,
    keep: 1,
    exclude: 1,
    unreviewed: 4,
  });
  for (const e of project.list().rows.filter((e) => e.status === "unreviewed"))
    decide(e, "keep");
  assert.equal(project.nextUnreviewed({}, null), null);

  const exportParent = path.join(dir, "exports");
  await mkdir(exportParent);
  const started = project.startExport(exportParent);
  assert.deepEqual(started.exporting, { total: 0, completed: 0, current: "" });
  assert.throws(() => project.startExport(exportParent), /already running/);
  await project.exporting.done;
  const { lastExport, exporting } = project.summary();
  assert.equal(exporting, null);
  assert.equal(lastExport.error, null);
  assert.deepEqual(lastExport.counts, {
    total: 9,
    keep: 8,
    exclude: 1,
    unreviewed: 0,
  });
  const exported = lastExport.directory;
  const retained = await readdir(path.join(exported, "retained", "sub-001"));
  assert.equal(retained.length, 4, "two sources, CSV and RDS each");
  assert.ok(
    retained.every((f) =>
      /^sub-001_task-memory_run-01_epoch-probe_source-[0-9a-f]{12}\.(csv|rds)$/.test(
        f,
      ),
    ),
  );
  assert.deepEqual(await readdir(path.join(exported, "excluded")), ["sub-001"]);
  assert.deepEqual(
    (await readFile(path.join(exported, "summary.csv"), "utf8"))
      .trim()
      .split("\n"),
    [
      '"participant","session","task","run","epochs","kept","excluded","unreviewed","excluded_automatically"',
      '"001","","memory","01","6","5","1","0","0"',
      '"005","","memory","01","3","3","0","0","0"',
    ],
  );
  const decisions = (
    await readFile(path.join(exported, "decisions.csv"), "utf8")
  ).split("\n");
  assert.match(decisions[0], /"file"$/);
  assert.match(
    decisions[1],
    /"sub-00[15]\/sub-00[15]_task-memory_run-01_epoch-probe/,
  );
  assert.match(
    await readFile(path.join(exported, "README.txt"), "utf8"),
    /Analyze retained\/ only/,
  );
  await assert.rejects(() => readdir(path.join(exported, "unreviewed")), {
    code: "ENOENT",
  });
});

test("run diagnostics average a run's epochs and keep each trace", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-average-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  project = await Project.open(path.join(dir, "average.eyeris"), worker, true);
  await project.importFile(path.join(dir, "sub-005_task-memory.rds"));
  await project.importFile(path.join(dir, "sub-003_task-memory.rds"));
  const groups = project.diagnosticGroups();
  assert.deepEqual(
    groups.map((g) => [g.participant, g.run, g.label, g.epochs]),
    [
      ["003", "01", "epoch_probe", 3],
      ["003", "01", "epoch_second", 3],
      ["003", "07", "epoch_probe", 3],
      ["005", "01", "epoch_probe", 3],
    ],
  );
  const key = groups[3].key;
  const all = await project.average({
    key,
    stage: "pupil_raw",
    include: "all",
  });
  assert.equal(all.epochs, 3);
  assert.equal(all.traces.length, 3);
  assert.equal(all.time.length, 600);
  // The window is -1 to 1 s, so time is measured from the event.
  assert.equal(all.onset, true);
  assert.ok(Math.abs(all.time[0] + 1) < 1e-12);
  assert.ok(Math.abs(all.time.at(-1) - 1) < 1e-12);
  // Missing samples stay missing: at the start only the second epoch has data.
  assert.deepEqual(
    all.traces.map((trace) => trace[0] === null),
    [true, false, true],
  );
  assert.equal(all.n[0], 1);
  assert.equal(all.se[0], null);
  assert.ok(Math.abs(all.mean[0] - all.traces[1][0]) < 1e-9);
  // Identical epochs average to themselves with no spread.
  assert.equal(all.n.at(-1), 3);
  assert.ok(Math.abs(all.mean.at(-1) - all.traces[0].at(-1)) < 1e-9);
  assert.equal(all.se.at(-1), 0);
  const final = await project.average({ key, stage: "final", include: "all" });
  assert.equal(final.stage, "pupil_raw_lpfilt");
  // Excluded epochs are left out unless every epoch is requested.
  const [first] = project.list({ participant: "005" }).rows;
  project.decision({
    id: first.id,
    status: "exclude",
    stage: "final",
    reviewer: "Tester",
    reason: "",
  });
  const included = await project.average({
    key,
    stage: "final",
    include: "included",
  });
  assert.equal(included.epochs, 2);
  assert.ok(!included.ids.includes(first.id));
  assert.deepEqual(
    await project.average({ key, stage: "final", include: "kept" }),
    { epochs: 0, stage: "final" },
  );
  await assert.rejects(
    () => project.average({ key, stage: "final", include: "some" }),
    /Choose/,
  );
  await assert.rejects(
    () => project.average({ key, stage: "pupil_missing", include: "all" }),
    /selected stage/,
  );
});

test("epochs split by event fields or by joined behavioral data", async (t) => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-split-test-"));
  const worker = new RWorker();
  let project;
  t.after(async () => {
    project?.close();
    worker.close();
    await rm(dir, { recursive: true, force: true });
  });
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  const projectDir = path.join(dir, "split.eyeris");
  project = await Project.open(projectDir, worker, true);
  for (const name of ["sub-001_task-memory.rds", "sub-002_task-memory.rds"])
    await project.importFile(path.join(dir, name));
  const fields = await project.epochFields();
  assert.ok(fields.includes("trial") && fields.includes("matched_event"));
  assert.ok(!fields.includes("arbitrary_metadata"), "varies within an epoch");
  assert.ok(!fields.includes("timebin") && !fields.includes("block"));
  const key = project
    .diagnosticGroups()
    .find((g) => g.participant === "001").key;
  const request = { key, scope: "run", stage: "pupil_raw", include: "all" };
  let split = await project.split({
    ...request,
    by: { from: "epoch", column: "matched_event" },
    join: null,
  });
  assert.deepEqual(
    split.series.map((s) => [s.label, s.n]),
    [
      ["EVENT_OTHER", 1],
      ["REPEATED_EVENT", 2],
    ],
  );
  assert.equal(split.onset, true);
  assert.ok(Math.abs(split.time[0] + 1) < 1e-12);
  assert.equal(split.time.length, 600);
  // Identical epochs: the group mean is their value, with no spread.
  assert.equal(split.series[1].se.at(-1), 0);
  assert.ok(
    Math.abs(split.series[1].mean.at(-1) - split.series[0].mean.at(-1)) < 1e-9,
  );

  // Behavioral tables are joined within each subject's session, task and run.
  const bids = path.join(dir, "dataset");
  for (const [file, text] of [
    [
      "sub-001/beh/sub-001_task-memory_beh.tsv",
      "trial\taccuracy\n7\t1\n8.0\t0\n",
    ],
    // Trial 8 appears twice, so it cannot be matched to one row.
    [
      "sub-002/beh/sub-002_task-memory_beh.tsv",
      "trial\taccuracy\n7\t0\n8\t0\n8\t1\n",
    ],
    ["sub-002/beh/notes.txt", "ignored"],
  ]) {
    await mkdir(path.dirname(path.join(bids, file)), { recursive: true });
    await writeFile(path.join(bids, file), text);
  }
  const linked = await project.linkBehavior(bids);
  assert.deepEqual(
    {
      files: linked.files,
      rows: linked.rows,
      columns: linked.columns,
      root: linked.root,
    },
    { files: 2, rows: 5, columns: ["accuracy", "trial"], root: bids },
  );
  const byAccuracy = {
    by: { from: "behavior", column: "accuracy" },
    join: { epoch: "trial", behavior: "trial" },
  };
  split = await project.split({ ...request, scope: "all", ...byAccuracy });
  assert.deepEqual(
    split.series.map((s) => [s.label, s.n]),
    [
      ["0", 3],
      ["1", 2],
    ],
  );
  assert.deepEqual(
    [split.epochs, split.total, split.ambiguous, split.unmatched],
    [5, 6, 1, 0],
  );
  // Pooled across sources: at the first sample, group 0 holds sub-001's
  // 4100 and sub-002's -200 (its first epoch) and 4100.
  const values = [4100, -200, 4100];
  const mean = values.reduce((a, b) => a + b) / 3;
  const sd = Math.sqrt(values.reduce((a, v) => a + (v - mean) ** 2, 0) / 2);
  assert.ok(Math.abs(split.series[0].mean[0] - mean) < 1e-6);
  assert.ok(Math.abs(split.series[0].se[0] - sd / Math.sqrt(3)) < 1e-6);
  split = await project.split({ ...request, scope: "subject", ...byAccuracy });
  assert.equal(split.total, 3);
  assert.deepEqual(
    split.series.map((s) => [s.label, s.n]),
    [
      ["0", 1],
      ["1", 2],
    ],
  );
  // Without a split, the scope's epochs are averaged together.
  split = await project.split({
    ...request,
    scope: "all",
    by: null,
    join: null,
  });
  assert.deepEqual(
    split.series.map((s) => [s.label, s.n]),
    [["All epochs", 6]],
  );
  await assert.rejects(
    () => project.split({ ...request, ...byAccuracy, join: null }),
    /identify each trial/,
  );
  await assert.rejects(
    () => project.linkBehavior(path.join(dir, "exports-missing")),
    /ENOENT/,
  );
  // Behavioral data and fields of earlier epochs persist and are recomputed.
  project.db
    .prepare("UPDATE epochs SET meta = json_remove(meta, '$.fields')")
    .run();
  project.close();
  project = await Project.open(projectDir, worker);
  assert.equal(project.behavior().rows, 5);
  assert.ok((await project.epochFields()).includes("trial"));
  // At most eight groups.
  await project.importFile(path.join(dir, "sub-large.rds"));
  const large = project
    .diagnosticGroups()
    .find((g) => g.participant === "large");
  await assert.rejects(
    () =>
      project.split({
        ...request,
        key: large.key,
        by: { from: "epoch", column: "trial" },
        join: null,
      }),
    /10001 values/,
  );
});
