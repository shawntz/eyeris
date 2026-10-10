import { resolveRscript } from "./rscript.mjs";
import { spawn } from "node:child_process";
import { constants, createWriteStream } from "node:fs";
import { finished } from "node:stream/promises";
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

const entity = (value) =>
  typeof value === "string" && /^[a-zA-Z0-9]{1,64}$/.test(value);
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
export class Pipeline {
  constructor(project) {
    this.project = project;
    this.active = null;
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
      active: this.active
        ? {
            id: this.active.id,
            log: this.active.log,
            phase: this.active.phase,
            recordings: this.active.recordings,
            current: this.active.current,
          }
        : null,
    };
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
          file: path.join(
            "sourcedata",
            `sub-${input.subject}`,
            `ses-${input.session}`,
            `task-${input.task}`,
            `run-${run}`,
            path.basename(original),
          ),
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
    if (this.active)
      throw new Error("A pipeline is already running in this project.");
    this.validate(settings);
    const ids = [...new Set([recordingIds].flat())];
    if (!ids.length) throw new Error("Select at least one recording.");
    const db = this.project.db;
    const records = db
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
    const id = randomUUID();
    const dir = path.join(this.project.directory, "processing", id);
    await mkdir(path.join(dir, "bids"), { recursive: true });
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
    await writeFile(
      path.join(dir, "config.json"),
      JSON.stringify(config, null, 2),
    );
    await writeFile(
      path.join(dir, "reproduce.R"),
      `# Run with the eyeris version recorded in runtime.json.\n# The JSON configuration preserves every selected parameter.\ncfg <- jsonlite::fromJSON("config.json", simplifyDataFrame=FALSE)\nfor (rec in cfg$recordings) {\n  x <- do.call(eyeris::glassbox, c(list(file=rec$input, interactive_preview=FALSE), cfg$glassbox))\n  if (!is.null(cfg$epoch)) x <- do.call(eyeris::epoch, c(list(eyeris=x), cfg$epoch))\n  eyeris::bidsify(x, bids_dir=cfg$bids, participant_id=rec$subject, session_num=rec$session, task_name=rec$task, run_num=rec$run, html_report=cfg$report, db_enabled=cfg$database, db_path="eyeris")\n  saveRDS(x, rec$output)\n}\n`,
    );
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
    const active = {
      id,
      recordings: records.map((r) => r.id),
      current: records[0].id,
      child: null,
      log: "",
      phase: "starting",
      cancelled: false,
    };
    this.active = active;
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
    active.done = (async () => {
      let result;
      try {
        result = await runWithWindowsRecovery({
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
          this.project.db
            .prepare(
              "UPDATE jobs SET status='publishing', phase='publishing' WHERE id=?",
            )
            .run(id);
          const outputs = await this.publishBids(dir);
          if (settings.epoch)
            for (const r of recordings)
              try {
                await this.project.importFile(r.output);
              } catch (e) {
                throw new Error(`${path.basename(r.output)}: ${e.message}`);
              }
          this.project.db
            .prepare("UPDATE jobs SET outputs=? WHERE id=?")
            .run(JSON.stringify(outputs), id);
        }
      } catch (e) {
        status = "failed";
        error = e.message;
        await appendFile(path.join(dir, "process.log"), `\n${error}\n`).catch(
          () => {},
        );
      }
      this.project.db
        .prepare(
          "UPDATE jobs SET status=?,phase=?,error=?,finished_at=? WHERE id=?",
        )
        .run(status, status, error, new Date().toISOString(), id);
      this.active = null;
    })();
    return this.snapshot();
  }
  async publishBids(dir) {
    const source = path.join(dir, "bids");
    const names = await files(source);
    const target = path.join(this.project.directory, "bids");
    // Each immutable processing folder retains the exact BIDS output of that run.
    // The common bids/ tree receives only non-conflicting files. A re-run remains
    // available under processing/<id>/bids rather than overwriting an earlier run.
    const pending = [];
    for (const name of names) {
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
    try {
      for (const name of pending) {
        const dest = path.join(target, name);
        await mkdir(path.dirname(dest), { recursive: true });
        await copyFile(path.join(source, name), dest);
        installed.push(dest);
      }
    } catch (e) {
      for (const file of installed) await rm(file, { force: true });
      throw e;
    }
    return names;
  }
  cancel() {
    if (this.active && this.active.phase !== "publishing") {
      this.active.cancelled = true;
      this.active.child.kill();
    }
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
