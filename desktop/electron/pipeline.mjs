import { resolveRscript } from "./rscript.mjs";
import { spawn } from "node:child_process";
import { constants, createReadStream, createWriteStream } from "node:fs";
import { finished, pipeline as copyStream } from "node:stream/promises";
import {
  mkdir,
  copyFile,
  writeFile,
  readFile,
  readdir,
  rename,
  rm,
  access,
  appendFile,
} from "node:fs/promises";
import { createHash, randomUUID } from "node:crypto";
import path from "node:path";
import { rDirectory, rEnvironment } from "./runtime.mjs";
import { runWithWindowsRecovery } from "./processing-recovery.mjs";
import { scanBids } from "./bids.mjs";

const entity = (value) =>
  typeof value === "string" && /^[a-zA-Z0-9]{1,64}$/.test(value);
// Copy the tables of a job's DuckDB database into the project's database. Tables
// are named by subject, session, task and run, so existing tables are the same
// published run and are kept.
function mergeDatabase(source, target) {
  return new Promise((resolve, reject) => {
    const child = spawn(
      resolveRscript(),
      [
        "--vanilla",
        path.join(rDirectory(), "merge-database.R"),
        source,
        target,
      ],
      {
        env: rEnvironment(),
        stdio: ["ignore", "pipe", "pipe"],
        windowsHide: true,
      },
    );
    let log = "";
    for (const stream of [child.stdout, child.stderr])
      stream.on("data", (data) => (log = (log + data).slice(-3000)));
    child.on("error", reject);
    child.on("close", (code) =>
      code === 0
        ? resolve()
        : reject(new Error(`The DuckDB database could not be merged. ${log}`)),
    );
  });
}
async function files(dir, base = dir) {
  const result = [];
  for (const e of await readdir(dir, { withFileTypes: true })) {
    const name = path.join(dir, e.name);
    if (e.isDirectory()) result.push(...(await files(name, base)));
    else if (e.isFile()) result.push(path.relative(base, name));
  }
  return result;
}
const recordingsTable = (name) =>
  `CREATE TABLE ${name}(id TEXT PRIMARY KEY, subject TEXT NOT NULL REFERENCES subjects(id), session TEXT NOT NULL, task TEXT NOT NULL, run TEXT NOT NULL DEFAULT '', name TEXT NOT NULL, file TEXT NOT NULL, created_at TEXT NOT NULL, UNIQUE(subject,session,task,run))`;
const runOrder = "subject, session, task, length(run), run";
const sourcePath = (r) =>
  path.join(
    "sourcedata",
    `sub-${r.subject}`,
    `ses-${r.session}`,
    `task-${r.task}`,
    `run-${r.run}`,
    r.name,
  );
export class Pipeline {
  constructor(project) {
    this.project = project;
    this.running = new Map();
    this.queue = [];
    this.concurrency = 1;
    this.waiters = [];
    // The jobs queued since processing was last idle, for overall progress.
    this.batch = [];
    this.publishing = Promise.resolve();
    this.importing = null;
    this.lastImport = null;
    project.db
      .exec(`CREATE TABLE IF NOT EXISTS subjects(id TEXT PRIMARY KEY, created_at TEXT NOT NULL);
      ${recordingsTable("IF NOT EXISTS recordings")};
      CREATE TABLE IF NOT EXISTS jobs(id TEXT PRIMARY KEY, recording_id TEXT NOT NULL REFERENCES recordings(id), status TEXT NOT NULL, phase TEXT NOT NULL, config TEXT NOT NULL, started_at TEXT NOT NULL, finished_at TEXT, error TEXT, outputs TEXT NOT NULL DEFAULT '[]');
      CREATE TABLE IF NOT EXISTS job_recordings(job_id TEXT NOT NULL REFERENCES jobs(id), recording_id TEXT NOT NULL REFERENCES recordings(id), PRIMARY KEY(job_id, recording_id));`);
    this.migrate();
    project.db
      .prepare(
        "UPDATE jobs SET status='interrupted', phase='interrupted', error='Processing was interrupted. Run this recording again.' WHERE status IN ('running','publishing')",
      )
      .run();
  }
  // Projects created before run numbers allowed one ASC per subject, session and
  // task. SQLite cannot change a UNIQUE constraint in place, so rebuild the
  // table. Earlier recordings keep an empty run: eyeris still numbers their
  // blocks as runs, exactly as before.
  migrate() {
    const db = this.project.db;
    const columns = db
      .prepare("SELECT name FROM pragma_table_info('recordings')")
      .all()
      .map((c) => c.name);
    if (columns.includes("run")) return;
    db.exec("PRAGMA foreign_keys=OFF");
    try {
      this.project.transaction(() => {
        db.exec(`${recordingsTable("recordings_next")};
          INSERT INTO recordings_next(id, subject, session, task, name, file, created_at)
            SELECT id, subject, session, task, name, file, created_at FROM recordings;
          DROP TABLE recordings;
          ALTER TABLE recordings_next RENAME TO recordings;
          INSERT OR IGNORE INTO job_recordings SELECT id, recording_id FROM jobs;`);
        if (db.prepare("PRAGMA foreign_key_check").all().length)
          throw new Error("This project's recordings could not be upgraded.");
      });
    } finally {
      db.exec("PRAGMA foreign_keys=ON");
    }
  }
  snapshot() {
    const db = this.project.db;
    const links = new Map();
    for (const row of db
      .prepare(
        `SELECT l.job_id, l.recording_id FROM job_recordings l JOIN recordings r ON r.id = l.recording_id ORDER BY ${runOrder}`,
      )
      .all())
      links.set(row.job_id, [
        ...(links.get(row.job_id) || []),
        row.recording_id,
      ]);
    return {
      subjects: db.prepare("SELECT * FROM subjects ORDER BY id").all(),
      recordings: db
        .prepare(`SELECT * FROM recordings ORDER BY ${runOrder}, created_at`)
        .all(),
      jobs: db
        .prepare("SELECT * FROM jobs ORDER BY started_at DESC")
        .all()
        .map((j) => ({
          ...j,
          config: JSON.parse(j.config),
          recordings: links.get(j.id) || [j.recording_id],
        })),
      running: [...this.running.values()].map(
        ({ id, log, phase, recordings, current }) => ({
          id,
          log,
          phase,
          recordings,
          current,
        }),
      ),
      queued: this.queue.map(({ id, recordings }) => ({ id, recordings })),
      batch: this.batch,
      settings: this.settings(),
      importing: this.importing && {
        total: this.importing.total,
        completed: this.importing.completed,
        bytes: this.importing.bytes,
        totalBytes: this.importing.totalBytes,
        current: this.importing.current,
      },
      lastImport: this.lastImport,
    };
  }
  // Pipeline settings are saved with the project, so they are set once for every
  // subject and run, and kept when the project is reopened.
  settings() {
    const saved = this.project.db
      .prepare("SELECT value FROM metadata WHERE key='pipeline_settings'")
      .get();
    return saved ? JSON.parse(saved.value) : null;
  }
  saveSettings(settings) {
    const value = JSON.stringify(settings);
    if (
      !settings ||
      typeof settings !== "object" ||
      Array.isArray(settings) ||
      value.length > 100_000
    )
      throw new Error("Invalid pipeline settings.");
    this.project.db
      .prepare(
        "INSERT INTO metadata VALUES ('pipeline_settings', ?) ON CONFLICT(key) DO UPDATE SET value=excluded.value",
      )
      .run(value);
    return this.snapshot();
  }
  // Called when the app quits: unfinished jobs are recorded as interrupted.
  dispose() {
    this.disposed = true;
    this.queue = [];
    this.cancelImport();
    for (const active of this.running.values()) {
      this.project.db
        .prepare(
          "UPDATE jobs SET status='interrupted', phase='interrupted' WHERE id=?",
        )
        .run(active.id);
      active.child?.kill();
    }
  }
  addSubject(id) {
    if (!entity(id))
      throw new Error(
        "Subject IDs must contain only letters and digits, without the sub- prefix.",
      );
    this.project.db
      .prepare("INSERT OR IGNORE INTO subjects VALUES (?,?)")
      .run(id, new Date().toISOString());
    return this.snapshot();
  }
  // Each ASC is one run of a subject's session and task. Several files are
  // numbered consecutively in filename order, after any existing runs unless a
  // first run is given.
  async addRecording(input, originals) {
    if (this.importing) throw new Error("Wait for the BIDS import to finish.");
    const db = this.project.db;
    const selected = [originals].flat();
    if (!entity(input.subject) || !entity(input.session) || !entity(input.task))
      throw new Error(
        "Subject, session and task must contain only letters and digits.",
      );
    if (!db.prepare("SELECT 1 FROM subjects WHERE id=?").get(input.subject))
      throw new Error("Create the subject first.");
    if (
      !selected.length ||
      selected.some((file) => path.extname(file).toLowerCase() !== ".asc")
    )
      throw new Error("Select EyeLink .asc files.");
    const existing = db
      .prepare(
        "SELECT run FROM recordings WHERE subject=? AND session=? AND task=?",
      )
      .all(input.subject, input.session, input.task)
      .map((r) => r.run);
    const requested = String(input.run ?? "").trim();
    if (requested && !/^\d{1,3}$/.test(requested))
      throw new Error("Run numbers must be whole numbers from 1 to 999.");
    // A recording without a run number holds at least run 01.
    const first = requested
      ? Number(requested)
      : Math.max(0, ...existing.map((run) => Number(run) || 1)) + 1;
    if (first < 1 || first + selected.length - 1 > 999)
      throw new Error("Run numbers must be whole numbers from 1 to 999.");
    const rows = [...selected]
      .sort((a, b) =>
        path
          .basename(a)
          .localeCompare(path.basename(b), undefined, { numeric: true }),
      )
      .map((original, i) => {
        const run = String(first + i).padStart(2, "0");
        return {
          id: randomUUID(),
          run,
          original,
          name: path.basename(original),
          file: sourcePath({ ...input, run, name: path.basename(original) }),
        };
      });
    const taken = rows.filter((r) => existing.includes(r.run));
    if (taken.length)
      throw new Error(
        `sub-${input.subject} already has ${taken.map((r) => `run ${r.run}`).join(", ")} for ses-${input.session} task-${input.task}. Choose another first run.`,
      );
    const copied = [];
    try {
      for (const row of rows) {
        const target = path.join(this.project.directory, row.file);
        await mkdir(path.dirname(target), { recursive: true });
        try {
          await copyFile(row.original, target, constants.COPYFILE_EXCL);
        } catch (error) {
          // A file here that no recording uses was left by an add interrupted
          // before it was recorded, and is replaced. A recording's file never is.
          if (
            error.code !== "EEXIST" ||
            db.prepare("SELECT 1 FROM recordings WHERE file=?").get(row.file)
          )
            throw error;
          await copyFile(row.original, target);
        }
        copied.push(target);
      }
      const created = new Date().toISOString();
      const insert = db.prepare(
        "INSERT INTO recordings(id, subject, session, task, run, name, file, created_at) VALUES (?,?,?,?,?,?,?,?)",
      );
      this.project.transaction(() => {
        for (const row of rows)
          insert.run(
            row.id,
            input.subject,
            input.session,
            input.task,
            row.run,
            row.name,
            row.file,
            created,
          );
      });
    } catch (error) {
      for (const file of copied) await rm(file, { force: true });
      throw error;
    }
    return this.snapshot();
  }
  // Add every recording in a BIDS dataset. Files are copied in the background so
  // the window keeps updating; each recording is committed once its copy is
  // complete. Files already in the project are skipped, so importing a dataset
  // again adds only its new recordings.
  async importBids(root) {
    if (this.busy || this.importing)
      throw new Error("Wait for processing or the current import to finish.");
    const db = this.project.db;
    const scan = await scanBids(root);
    if (!scan.recordings.length && !scan.skipped.length)
      throw new Error(
        "No EyeLink .asc files were found in sub-*/eye or sub-*/ses-*/eye folders.",
      );
    const skipped = [...scan.skipped];
    const plan = [];
    const groups = Map.groupBy(
      scan.recordings,
      (r) => `${r.subject}/${r.session}/${r.task}`,
    );
    for (const files of groups.values()) {
      const { subject, session, task } = files[0];
      const existing = db
        .prepare(
          "SELECT run, name FROM recordings WHERE subject=? AND session=? AND task=?",
        )
        .all(subject, session, task);
      const runs = new Set(existing.map((r) => r.run));
      const pending = [];
      for (const r of files) {
        if (
          existing.some((e) => e.name === r.name && (!r.run || e.run === r.run))
        )
          skipped.push({ file: r.relative, reason: "already in this project" });
        else if (runs.has(r.run))
          skipped.push({
            file: r.relative,
            reason: `sub-${subject} already has run ${r.run} for ses-${session} task-${task}`,
          });
        else if (r.run) {
          runs.add(r.run);
          plan.push(r);
        } else pending.push(r);
      }
      // Files without a run entity follow the existing runs, in filename order.
      let next = Math.max(0, ...[...runs].map((run) => Number(run) || 1));
      for (const r of pending.sort((a, b) =>
        a.name.localeCompare(b.name, undefined, { numeric: true }),
      )) {
        next += 1;
        if (next > 999)
          skipped.push({ file: r.relative, reason: "run numbers end at 999" });
        else plan.push({ ...r, run: String(next).padStart(2, "0") });
      }
    }
    plan.sort((a, b) =>
      `${a.subject}/${a.session}/${a.task}/${a.run.padStart(3, "0")}`.localeCompare(
        `${b.subject}/${b.session}/${b.task}/${b.run.padStart(3, "0")}`,
        undefined,
        { numeric: true },
      ),
    );
    const importing = {
      total: plan.length,
      completed: 0,
      bytes: 0,
      totalBytes: plan.reduce((n, r) => n + r.size, 0),
      current: "",
      cancel: new AbortController(),
    };
    this.importing = importing;
    const subjects = new Set();
    let error = null;
    importing.finished = (async () => {
      for (const r of plan) {
        if (importing.cancel.signal.aborted) break;
        importing.current = r.relative;
        const file = sourcePath(r);
        const target = path.join(this.project.directory, file);
        try {
          await mkdir(path.dirname(target), { recursive: true });
          const source = createReadStream(r.file);
          source.on("data", (chunk) => (importing.bytes += chunk.length));
          await copyStream(source, createWriteStream(target), {
            signal: importing.cancel.signal,
          });
          this.project.transaction(() => {
            db.prepare("INSERT OR IGNORE INTO subjects VALUES (?,?)").run(
              r.subject,
              new Date().toISOString(),
            );
            db.prepare(
              "INSERT INTO recordings(id, subject, session, task, run, name, file, created_at) VALUES (?,?,?,?,?,?,?,?)",
            ).run(
              randomUUID(),
              r.subject,
              r.session,
              r.task,
              r.run,
              r.name,
              file,
              new Date().toISOString(),
            );
          });
          subjects.add(r.subject);
          importing.completed += 1;
        } catch (e) {
          await rm(target, { force: true });
          if (!importing.cancel.signal.aborted)
            error = `${r.relative}: ${e.message}`;
          break;
        }
      }
      this.lastImport = {
        id: randomUUID(),
        root,
        added: importing.completed,
        subjects: subjects.size,
        skipped,
        cancelled: importing.cancel.signal.aborted,
        error,
      };
      this.importing = null;
    })();
    return this.snapshot();
  }
  cancelImport() {
    this.importing?.cancel.abort();
  }
  validate(settings) {
    if (
      !settings ||
      !settings.glassbox ||
      typeof settings.glassbox !== "object" ||
      Array.isArray(settings.glassbox)
    )
      throw new Error("Invalid pipeline settings.");
    const allowed = [
      "load_asc",
      "resample",
      "deblink",
      "detransient",
      "interpolate",
      "lpfilt",
      "downsample",
      "bin",
      "detrend",
      "zscore",
      "seed",
    ];
    for (const key of Object.keys(settings.glassbox))
      if (!allowed.includes(key))
        throw new Error(`Unsupported glassbox option: ${key}`);
    if (settings.glassbox.downsample && settings.glassbox.bin)
      throw new Error("Choose downsampling or binning, not both.");
    if (settings.epoch) {
      const allowedEpoch = [
        "events",
        "limits",
        "label",
        "baseline",
        "baseline_type",
        "baseline_events",
        "baseline_period",
        "hz",
      ];
      if (
        typeof settings.epoch !== "object" ||
        Array.isArray(settings.epoch) ||
        Object.keys(settings.epoch).some((k) => !allowedEpoch.includes(k))
      )
        throw new Error("Invalid epoch settings.");
      if (
        typeof settings.epoch.events !== "string" ||
        !settings.epoch.events.trim()
      )
        throw new Error("Enter an epoch event pattern or turn epoching off.");
      if (!entity(settings.epoch.label))
        throw new Error("Epoch labels must contain only letters and digits.");
      if (
        settings.epoch.limits !== null &&
        (!Array.isArray(settings.epoch.limits) ||
          settings.epoch.limits.length !== 2 ||
          !settings.epoch.limits.every(Number.isFinite) ||
          settings.epoch.limits[0] >= settings.epoch.limits[1])
      )
        throw new Error("Epoch start must be earlier than epoch end.");
    }
    if (
      typeof settings.report !== "boolean" ||
      typeof settings.database !== "boolean"
    )
      throw new Error("Invalid output settings.");
  }
  // Process one or more recordings in a single R session that writes one BIDS
  // folder, as a script looping over runs would. Session-level reports and the
  // database then include every run instead of conflicting between jobs.
  async start(recordingIds, settings) {
    return this.enqueue([recordingIds], settings);
  }
  // Queue one job per group of recordings, all with the same settings. Jobs run
  // in order, up to `concurrency` at a time, each in its own R process.
  async enqueue(groups, settings) {
    if (this.importing) throw new Error("Wait for the BIDS import to finish.");
    this.validate(settings);
    const jobs = groups.map((ids) => this.prepare(ids));
    if (!jobs.length) throw new Error("Select at least one recording.");
    const taken = new Set(
      [...this.running.values(), ...this.queue].flatMap((j) => j.recordings),
    );
    for (const records of jobs)
      for (const r of records) {
        if (taken.has(r.id))
          throw new Error(
            `${r.name} is already being processed. Wait for it to finish, or cancel it.`,
          );
        taken.add(r.id);
      }
    if (!this.busy) this.batch = [];
    for (const records of jobs) {
      const id = randomUUID();
      this.queue.push({
        id,
        records,
        recordings: records.map((r) => r.id),
        settings,
      });
      this.batch.push(id);
    }
    this.schedule();
    return this.snapshot();
  }
  prepare(recordingIds) {
    const ids = [...new Set([recordingIds].flat())];
    if (!ids.length) throw new Error("Select at least one recording.");
    const records = this.project.db
      .prepare(
        `SELECT * FROM recordings WHERE id IN (${ids.map(() => "?").join(",")}) ORDER BY ${runOrder}`,
      )
      .all(...ids);
    if (records.length !== ids.length) throw new Error("Recording not found.");
    const together = (a, b) =>
      a !== b &&
      a.subject === b.subject &&
      a.session === b.session &&
      a.task === b.task;
    const unnumbered = records.find(
      (r) => !r.run && records.some((other) => together(r, other)),
    );
    if (unnumbered)
      throw new Error(
        `${unnumbered.name} has no run number, so eyeris numbers its blocks as runs. Process it separately from other runs of sub-${unnumbered.subject} ses-${unnumbered.session} task-${unnumbered.task}.`,
      );
    return records;
  }
  get busy() {
    return this.running.size > 0 || this.queue.length > 0;
  }
  // Resolves once every queued and running job has finished.
  idle() {
    return this.busy
      ? new Promise((resolve) => this.waiters.push(resolve))
      : Promise.resolve();
  }
  schedule() {
    while (
      !this.disposed &&
      this.running.size < this.concurrency &&
      this.queue.length
    ) {
      const job = this.queue.shift();
      const active = {
        id: job.id,
        recordings: job.recordings,
        current: job.recordings[0],
        child: null,
        log: "",
        phase: "starting",
        cancelled: false,
      };
      this.running.set(job.id, active);
      active.done = this.run(job, active)
        .catch(() => {})
        .finally(() => {
          this.running.delete(job.id);
          this.schedule();
          if (!this.busy)
            for (const resolve of this.waiters.splice(0)) resolve();
        });
    }
  }
  async run({ id, records, settings }, active) {
    const db = this.project.db;
    const dir = path.join(this.project.directory, "processing", id);
    this.project.transaction(() => {
      db.prepare(
        "INSERT INTO jobs(id,recording_id,status,phase,config,started_at) VALUES (?,?,?,?,?,?)",
      ).run(
        id,
        records[0].id,
        "running",
        "starting",
        JSON.stringify(settings),
        new Date().toISOString(),
      );
      const link = db.prepare("INSERT INTO job_recordings VALUES (?,?)");
      for (const r of records) link.run(id, r.id);
    });
    const recordings = records.map((r) => ({
      input: path.join(this.project.directory, r.file),
      subject: r.subject,
      session: r.session,
      task: r.task,
      run: r.run || null,
      output: path.join(
        dir,
        `sub-${r.subject}_ses-${r.session}_task-${r.task}${r.run ? `_run-${r.run}` : ""}.rds`,
      ),
    }));
    const config = { ...settings, bids: path.join(dir, "bids"), recordings };
    const run = (attempt) =>
      new Promise((resolve) => {
        const stream = createWriteStream(path.join(dir, "process.log"));
        const closed = finished(stream);
        // Keep a disk-write error observable when the child closes, without an
        // unhandled rejection while processing is still running.
        closed.catch(() => {});
        const child = spawn(
          resolveRscript(),
          [
            "--vanilla",
            path.join(rDirectory(), "process.R"),
            path.join(dir, "config.json"),
          ],
          {
            env: rEnvironment(),
            stdio: ["ignore", "pipe", "pipe"],
            windowsHide: true,
          },
        );
        active.child = child;
        active.log = "";
        let stdout = "";
        const log = (data) => {
          stream.write(data);
          active.log = (
            active.log + data.toString().replace(/\x1b\[[0-9;]*m/g, "")
          ).slice(-18000);
        };
        if (attempt > 1)
          log(
            "Restarting processing after a Windows R access violation; the first attempt is preserved in failed-attempt-1.\n",
          );
        child.stderr.on("data", log);
        child.stdout.on("data", (data) => {
          log(data);
          stdout += data.toString();
          const lines = stdout.split("\n");
          stdout = lines.pop();
          for (const line of lines)
            if (line.startsWith("@@EYERIS@@")) {
              try {
                const event = JSON.parse(line.slice(10));
                active.phase = event.phase;
                if (event.recording)
                  active.current =
                    active.recordings[event.recording - 1] ?? active.current;
              } catch {}
            }
        });
        let spawnError;
        child.on("error", (error) => {
          spawnError = error;
          log(error.message);
        });
        child.on("close", async (code, signal) => {
          stream.end();
          try {
            await closed;
          } catch (error) {
            spawnError = error;
          }
          resolve({ code, signal, spawnError });
        });
      });
    let result;
    try {
      await mkdir(path.join(dir, "bids"), { recursive: true });
      await writeFile(
        path.join(dir, "config.json"),
        JSON.stringify(config, null, 2),
      );
      await writeFile(
        path.join(dir, "reproduce.R"),
        `# Run with the eyeris version recorded in runtime.json.\n# The JSON configuration preserves every selected parameter.\ncfg <- jsonlite::fromJSON("config.json", simplifyDataFrame=FALSE)\nfor (rec in cfg$recordings) {\n  x <- do.call(eyeris::glassbox, c(list(file=rec$input, interactive_preview=FALSE), cfg$glassbox))\n  if (!is.null(cfg$epoch)) x <- do.call(eyeris::epoch, c(list(eyeris=x), cfg$epoch))\n  eyeris::bidsify(x, bids_dir=cfg$bids, participant_id=rec$subject, session_num=rec$session, task_name=rec$task, run_num=rec$run, html_report=cfg$report, db_enabled=cfg$database, db_path="eyeris")\n  saveRDS(x, rec$output)\n}\n`,
      );
      result =
        active.cancelled || this.disposed
          ? { code: null }
          : await runWithWindowsRecovery({
              run,
              directory: dir,
              outputs: recordings.map((r) => r.output),
              isCancelled: () => active.cancelled || this.disposed,
              onRetry: () => {
                active.phase = "restarting";
              },
            });
    } catch (error) {
      result = { code: null, spawnError: error };
    }
    const { code, signal, spawnError } = result;
    if (this.disposed) {
      return;
    }
    let status = active.cancelled
      ? "cancelled"
      : code === 0 && !spawnError
        ? "completed"
        : "failed";
    let error =
      spawnError?.message ||
      (status === "failed"
        ? `R processing terminated ${signal ? `by signal ${signal}` : `with exit code ${code}`}.\n${active.log.slice(-3000)}`
        : null);
    try {
      if (status === "completed") {
        active.phase = "publishing";
        db.prepare(
          "UPDATE jobs SET status='publishing', phase='publishing' WHERE id=?",
        ).run(id);
        // Jobs finish in any order; publish and index them one at a time.
        const outputs = await this.exclusive(async () => {
          const published = await this.publishBids(dir);
          if (settings.epoch)
            for (const r of recordings)
              try {
                await this.project.importFile(r.output);
              } catch (e) {
                throw new Error(`${path.basename(r.output)}: ${e.message}`);
              }
          return published;
        });
        db.prepare("UPDATE jobs SET outputs=? WHERE id=?").run(
          JSON.stringify(outputs),
          id,
        );
      }
    } catch (e) {
      status = "failed";
      error = e.message;
      await appendFile(path.join(dir, "process.log"), `\n${error}\n`).catch(
        () => {},
      );
    }
    db.prepare(
      "UPDATE jobs SET status=?,phase=?,error=?,finished_at=? WHERE id=?",
    ).run(status, status, error, new Date().toISOString(), id);
  }
  exclusive(fn) {
    const result = this.publishing.then(fn, fn);
    this.publishing = result.catch(() => {});
    return result;
  }
  async publishBids(dir) {
    const source = path.join(dir, "bids");
    const names = await files(source);
    const target = path.join(this.project.directory, "bids");
    // Each immutable processing folder retains the exact BIDS output of that run.
    // The common bids/ tree receives only non-conflicting files. A re-run remains
    // available under processing/<id>/bids rather than overwriting an earlier run.
    // The DuckDB database is shared by every subject but holds one table per
    // subject, session, task and run, so it is merged rather than compared.
    const databases = names.filter((name) => name.endsWith(".eyerisdb"));
    const pending = [];
    for (const name of names) {
      if (databases.includes(name)) continue;
      const dest = path.join(target, name);
      try {
        await access(dest);
        const a = createHash("sha256")
          .update(await readFile(dest))
          .digest("hex");
        const b = createHash("sha256")
          .update(await readFile(path.join(source, name)))
          .digest("hex");
        if (a !== b) {
          return []; // A versioned, complete BIDS dataset remains in this run folder.
        }
      } catch (e) {
        if (e.code !== "ENOENT") throw e;
        pending.push(name);
      }
    }
    const installed = [];
    const merged = [];
    try {
      for (const name of databases) {
        const dest = path.join(target, name);
        const staged = `${dest}.${randomUUID()}.tmp`;
        try {
          await copyFile(dest, staged);
        } catch (e) {
          if (e.code !== "ENOENT") throw e;
          pending.push(name);
          continue;
        }
        merged.push({ staged, dest });
        await mergeDatabase(path.join(source, name), staged);
      }
      for (const name of pending) {
        const dest = path.join(target, name);
        await mkdir(path.dirname(dest), { recursive: true });
        await copyFile(path.join(source, name), dest);
        installed.push(dest);
      }
      for (const { staged, dest } of merged) await rename(staged, dest);
    } catch (e) {
      for (const file of installed) await rm(file, { force: true });
      for (const { staged } of merged) await rm(staged, { force: true });
      throw e;
    }
    return names;
  }
  // Cancel one queued or running job, or everything when no ID is given.
  cancel(id) {
    const removed = this.queue.filter((job) => !id || job.id === id);
    this.queue = this.queue.filter((job) => !removed.includes(job));
    this.batch = this.batch.filter((job) => !removed.some((r) => r.id === job));
    for (const active of this.running.values())
      if ((!id || active.id === id) && active.phase !== "publishing") {
        active.cancelled = true;
        active.child?.kill();
      }
    if (!this.busy) for (const resolve of this.waiters.splice(0)) resolve();
  }
  async log(id) {
    if (!this.project.db.prepare("SELECT 1 FROM jobs WHERE id=?").get(id))
      throw new Error("Unknown processing run.");
    return (
      await readFile(
        path.join(this.project.directory, "processing", id, "process.log"),
        "utf8",
      )
    ).slice(-18000);
  }
}
