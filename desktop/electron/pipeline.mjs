import { resolveRscript } from "./rscript.mjs";
import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
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
export class Pipeline {
  constructor(project) {
    this.project = project;
    this.active = null;
    project.db
      .exec(`CREATE TABLE IF NOT EXISTS subjects(id TEXT PRIMARY KEY, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS recordings(id TEXT PRIMARY KEY, subject TEXT NOT NULL REFERENCES subjects(id), session TEXT NOT NULL, task TEXT NOT NULL, name TEXT NOT NULL, file TEXT NOT NULL, created_at TEXT NOT NULL, UNIQUE(subject,session,task));
      CREATE TABLE IF NOT EXISTS jobs(id TEXT PRIMARY KEY, recording_id TEXT NOT NULL REFERENCES recordings(id), status TEXT NOT NULL, phase TEXT NOT NULL, config TEXT NOT NULL, started_at TEXT NOT NULL, finished_at TEXT, error TEXT, outputs TEXT NOT NULL DEFAULT '[]');`);
    project.db
      .prepare(
        "UPDATE jobs SET status='interrupted', phase='interrupted', error='Processing was interrupted. Run this recording again.' WHERE status IN ('running','publishing')",
      )
      .run();
  }
  snapshot() {
    const db = this.project.db;
    return {
      subjects: db.prepare("SELECT * FROM subjects ORDER BY id").all(),
      recordings: db
        .prepare("SELECT * FROM recordings ORDER BY created_at")
        .all(),
      jobs: db
        .prepare("SELECT * FROM jobs ORDER BY started_at DESC")
        .all()
        .map((j) => ({ ...j, config: JSON.parse(j.config) })),
      active: this.active
        ? { id: this.active.id, log: this.active.log, phase: this.active.phase }
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
  async addRecording(input, original) {
    if (!entity(input.subject) || !entity(input.session) || !entity(input.task))
      throw new Error(
        "Subject, session and task must contain only letters and digits.",
      );
    if (
      !this.project.db
        .prepare("SELECT 1 FROM subjects WHERE id=?")
        .get(input.subject)
    )
      throw new Error("Create the subject first.");
    if (
      this.project.db
        .prepare(
          "SELECT 1 FROM recordings WHERE subject=? AND session=? AND task=?",
        )
        .get(input.subject, input.session, input.task)
    )
      throw new Error(
        "This subject already has an ASC for that session and task. Use another session or task for a different recording.",
      );
    if (path.extname(original).toLowerCase() !== ".asc")
      throw new Error("Select an EyeLink .asc file.");
    const id = randomUUID();
    const relative = path.join(
      "sourcedata",
      `sub-${input.subject}`,
      `ses-${input.session}`,
      `task-${input.task}`,
      path.basename(original),
    );
    const target = path.join(this.project.directory, relative);
    await mkdir(path.dirname(target), { recursive: true });
    await copyFile(original, target);
    this.project.db
      .prepare("INSERT INTO recordings VALUES (?,?,?,?,?,?,?)")
      .run(
        id,
        input.subject,
        input.session,
        input.task,
        path.basename(original),
        relative,
        new Date().toISOString(),
      );
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
  async start(recordingId, settings) {
    if (this.active)
      throw new Error("A pipeline is already running in this project.");
    this.validate(settings);
    const record = this.project.db
      .prepare("SELECT * FROM recordings WHERE id=?")
      .get(recordingId);
    if (!record) throw new Error("Recording not found.");
    const id = randomUUID();
    const dir = path.join(this.project.directory, "processing", id);
    await mkdir(path.join(dir, "bids"), { recursive: true });
    const output = path.join(
      dir,
      `sub-${record.subject}_ses-${record.session}_task-${record.task}.rds`,
    );
    const config = {
      ...settings,
      input: path.join(this.project.directory, record.file),
      subject: record.subject,
      session: record.session,
      task: record.task,
      bids: path.join(dir, "bids"),
      output,
    };
    await writeFile(
      path.join(dir, "config.json"),
      JSON.stringify(config, null, 2),
    );
    await writeFile(
      path.join(dir, "reproduce.R"),
      `# Run with the eyeris version recorded in runtime.json.\n# The JSON configuration preserves every selected parameter.\ncfg <- jsonlite::fromJSON("config.json")\nx <- do.call(eyeris::glassbox, c(list(file=cfg$input, interactive_preview=FALSE), cfg$glassbox))\nif (!is.null(cfg$epoch)) x <- do.call(eyeris::epoch, c(list(eyeris=x), cfg$epoch))\neyeris::bidsify(x, bids_dir=cfg$bids, participant_id=cfg$subject, session_num=cfg$session, task_name=cfg$task, html_report=cfg$report, db_enabled=cfg$database, db_path="eyeris")\nsaveRDS(x, cfg$output)\n`,
    );
    this.project.db
      .prepare(
        "INSERT INTO jobs(id,recording_id,status,phase,config,started_at) VALUES (?,?,?,?,?,?)",
      )
      .run(
        id,
        recordingId,
        "running",
        "starting",
        JSON.stringify(settings),
        new Date().toISOString(),
      );
    const active = {
      id,
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
                active.phase = JSON.parse(line.slice(10)).phase;
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
          output,
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
          const outputs = await this.publishBids(dir, record, id);
          if (settings.epoch) await this.project.importFile(output);
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
  async publishBids(dir, recording, jobId) {
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
