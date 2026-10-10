import { DatabaseSync } from "node:sqlite";
import { createHash, randomUUID } from "node:crypto";
import { createReadStream } from "node:fs";
import { mkdir, copyFile, rename, rm, writeFile, stat } from "node:fs/promises";
import path from "node:path";
import packageMetadata from "../package.json" with { type: "json" };

const states = ["unreviewed", "keep", "exclude"];
// Version 2 adds BIDS session, task and run labels to epochs and allows several
// recordings, one per run, for a subject, session and task.
const schemaVersion = "2";
// Automatic exclusions are recorded under this reviewer name. They apply only
// to epochs no person has decided on, and never change a person's decision.
export const AUTO_REVIEWER = "eyeris auto-exclude";
const autoDefaults = { enabled: false, threshold: 25, stage: "final" };
const digest = (text) => createHash("sha256").update(text).digest("hex");
async function fileHash(file) {
  const hash = createHash("sha256");
  for await (const chunk of createReadStream(file)) hash.update(chunk);
  return hash.digest("hex");
}
const entity = (name, key) =>
  new RegExp(`(?:^|_)${key}-([^_.]+)`, "i").exec(name)?.[1] || "";
const runLabel = (value) =>
  /^\d+$/.test(value) ? String(Number(value)).padStart(2, "0") : value;
// eyeris::bidsify() applies a file's run number only when the recording holds a
// single block. Otherwise each block becomes its own run, so label it that way.
function epochRuns(epochs, run) {
  const blocks = new Map();
  for (const e of epochs)
    blocks.set(e.eye, (blocks.get(e.eye) || new Set()).add(e.block));
  return epochs.map((e) => {
    const count = Number.isInteger(e.blocks)
      ? e.blocks
      : blocks.get(e.eye).size;
    if (run && count <= 1) return run;
    const block = /^block_(\d+)$/.exec(e.block)?.[1];
    return block ? runLabel(block) : run;
  });
}
function entities(name) {
  return {
    participant: entity(name, "sub") || name.replace(/\.rds$/i, ""),
    session: entity(name, "ses"),
    task: entity(name, "task"),
    run: runLabel(entity(name, "run")),
  };
}
const csv = (rows) =>
  rows
    .map((row) =>
      row.map((v) => `"${String(v ?? "").replaceAll('"', '""')}"`).join(","),
    )
    .join("\n") + "\n";

export class Project {
  static async open(directory, worker, create = false) {
    const dbPath = path.join(directory, "review.sqlite");
    let exists = false;
    try {
      await stat(dbPath);
      exists = true;
    } catch (error) {
      if (!create || error.code !== "ENOENT") throw error;
    }
    await mkdir(path.join(directory, "sources"), { recursive: true });
    const project = new Project(directory, worker, exists);
    return project;
  }
  constructor(directory, worker, exists) {
    this.directory = directory;
    this.worker = worker;
    this.verified = new Set();
    this.db = new DatabaseSync(path.join(directory, "review.sqlite"));
    if (exists) {
      let schema;
      try {
        schema = this.db
          .prepare("SELECT value FROM metadata WHERE key=?")
          .get("schema")?.value;
      } catch {}
      if (schema !== "1" && schema !== schemaVersion) {
        this.db.close();
        throw new Error(
          Number(schema) > Number(schemaVersion)
            ? "This project was created by a newer version of eyeris. Update the app to open it."
            : "This folder does not contain a compatible eyeris review project.",
        );
      }
    }
    this.db
      .exec(`PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON; PRAGMA synchronous=FULL;
      CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS sources (id TEXT PRIMARY KEY, name TEXT NOT NULL, participant TEXT NOT NULL, imported_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS epochs (
        id TEXT PRIMARY KEY, source_id TEXT NOT NULL REFERENCES sources(id), participant TEXT NOT NULL,
        event TEXT NOT NULL, trial TEXT NOT NULL, label TEXT NOT NULL, block TEXT NOT NULL, eye TEXT NOT NULL,
        ordinal INTEGER NOT NULL, missing REAL NOT NULL, meta TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'unreviewed' CHECK(status IN ('unreviewed','keep','exclude')),
        reason TEXT NOT NULL DEFAULT '', reviewer TEXT NOT NULL DEFAULT '', stage TEXT NOT NULL DEFAULT '', updated_at TEXT,
        session TEXT NOT NULL DEFAULT '', task TEXT NOT NULL DEFAULT '', run TEXT NOT NULL DEFAULT ''
      );
      CREATE INDEX IF NOT EXISTS epochs_queue ON epochs(status, participant, source_id, ordinal);
      CREATE TABLE IF NOT EXISTS actions (
        seq INTEGER PRIMARY KEY AUTOINCREMENT, epoch_id TEXT NOT NULL REFERENCES epochs(id),
        kind TEXT NOT NULL, before_json TEXT NOT NULL, after_json TEXT NOT NULL, at TEXT NOT NULL, undone INTEGER NOT NULL DEFAULT 0
      );`);
    this.migrate();
  }
  migrate() {
    const columns = this.db
      .prepare("SELECT name FROM pragma_table_info('epochs')")
      .all()
      .map((c) => c.name);
    this.transaction(() => {
      if (!columns.includes("run")) {
        for (const column of ["session", "task", "run"])
          this.db.exec(
            `ALTER TABLE epochs ADD COLUMN ${column} TEXT NOT NULL DEFAULT ''`,
          );
        const update = this.db.prepare(
          "UPDATE epochs SET session=?, task=?, run=? WHERE id=?",
        );
        for (const source of this.db.prepare("SELECT * FROM sources").all()) {
          const { session, task, run } = entities(source.name);
          const epochs = this.db
            .prepare("SELECT id, meta FROM epochs WHERE source_id=?")
            .all(source.id)
            .map((row) => ({ id: row.id, ...JSON.parse(row.meta) }));
          const runs = epochRuns(epochs, run);
          epochs.forEach((e, i) => update.run(session, task, runs[i], e.id));
        }
      }
      this.db
        .prepare(
          "INSERT INTO metadata VALUES ('schema', ?) ON CONFLICT(key) DO UPDATE SET value=excluded.value",
        )
        .run(schemaVersion);
    });
  }
  close() {
    this.db.close();
  }
  transaction(fn) {
    this.db.exec("BEGIN IMMEDIATE");
    try {
      const result = fn();
      this.db.exec("COMMIT");
      return result;
    } catch (e) {
      this.db.exec("ROLLBACK");
      throw e;
    }
  }
  summary() {
    const counts = { total: 0, keep: 0, exclude: 0, unreviewed: 0 };
    for (const row of this.db
      .prepare("SELECT status, COUNT(*) AS n FROM epochs GROUP BY status")
      .all()) {
      counts[row.status] = row.n;
      counts.total += row.n;
    }
    return {
      name: path.basename(this.directory).replace(/\.eyeris$/, ""),
      directory: this.directory,
      counts,
      sources: this.db
        .prepare("SELECT * FROM sources ORDER BY imported_at")
        .all(),
      participants: this.db
        .prepare("SELECT DISTINCT participant FROM epochs ORDER BY participant")
        .all()
        .map((r) => r.participant),
      runs: this.db
        .prepare(
          "SELECT DISTINCT run FROM epochs WHERE run != '' ORDER BY length(run), run",
        )
        .all()
        .map((r) => r.run),
      stages: this.db
        .prepare(
          "SELECT DISTINCT j.value AS stage FROM epochs e, json_each(e.meta, '$.stages') j",
        )
        .all()
        .map((r) => r.stage),
      autoExclude: this.autoExclude(),
      autoExcluded: this.db
        .prepare("SELECT COUNT(*) AS n FROM epochs WHERE reviewer=?")
        .get(AUTO_REVIEWER).n,
      canUndo: !!this.db
        .prepare(
          "SELECT 1 FROM actions WHERE kind='decision' AND undone=0 LIMIT 1",
        )
        .get(),
    };
  }
  async importFile(file) {
    if (path.extname(file).toLowerCase() !== ".rds")
      throw new Error(
        "Only saved .rds eyeris objects are supported in this version.",
      );
    // Hash the copied bytes, not a file which may change during the import.
    const temporary = path.join(
      this.directory,
      "sources",
      `${randomUUID()}.tmp`,
    );
    await copyFile(file, temporary);
    try {
      const id = await fileHash(temporary);
      if (this.db.prepare("SELECT 1 FROM sources WHERE id=?").get(id))
        return { duplicate: true, count: 0 };
      const epochs = await this.worker.request("index", { path: temporary });
      const name = path.basename(file);
      const { participant, session, task, run } = entities(name);
      const runs = epochRuns(epochs, run);
      const target = path.join(this.directory, "sources", `${id}.rds`);
      await rename(temporary, target);
      this.transaction(() => {
        this.db
          .prepare(
            "INSERT INTO sources (id, name, participant, imported_at) VALUES (?, ?, ?, ?)",
          )
          .run(id, name, participant, new Date().toISOString());
        const insert = this.db.prepare(
          "INSERT INTO epochs (id, source_id, participant, session, task, run, event, trial, label, block, eye, ordinal, missing, meta) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        );
        epochs.forEach((e, i) =>
          insert.run(
            digest(`${id}:${e.key}`),
            id,
            participant,
            session,
            task,
            runs[i],
            e.event,
            e.trial,
            e.label,
            e.block,
            e.eye,
            e.ordinal,
            e.missing,
            JSON.stringify(e),
          ),
        );
      });
      return {
        duplicate: false,
        count: epochs.length,
        autoExcluded: this.applyAutoExclude(id),
      };
    } finally {
      await rm(temporary, { force: true });
    }
  }
  autoExclude() {
    const saved = this.db
      .prepare("SELECT value FROM metadata WHERE key='auto_exclude'")
      .get();
    return saved ? JSON.parse(saved.value) : { ...autoDefaults };
  }
  async setAutoExclude(rule) {
    if (
      !rule ||
      typeof rule.enabled !== "boolean" ||
      typeof rule.threshold !== "number" ||
      !(rule.threshold >= 0 && rule.threshold < 100) ||
      typeof rule.stage !== "string" ||
      !rule.stage ||
      rule.stage.length > 200
    )
      throw new Error(
        "Enter a missing-data threshold from 0 to less than 100 percent.",
      );
    const value = {
      enabled: rule.enabled,
      threshold: rule.threshold,
      stage: rule.stage,
    };
    if (value.enabled && value.stage !== "final")
      await this.backfillStageMissing();
    this.db
      .prepare(
        "INSERT INTO metadata VALUES ('auto_exclude', ?) ON CONFLICT(key) DO UPDATE SET value=excluded.value",
      )
      .run(JSON.stringify(value));
    this.applyAutoExclude();
    return this.summary();
  }
  // Epochs indexed by earlier versions store only the final stage's missing
  // fraction. Compute every stage's from the source before applying a rule.
  async backfillStageMissing() {
    const rows = this.db
      .prepare(
        "SELECT id, source_id, meta FROM epochs WHERE json_type(meta, '$.stageMissing') IS NULL ORDER BY source_id",
      )
      .all();
    for (const [source, epochs] of Map.groupBy(rows, (r) => r.source_id)) {
      const metas = epochs.map((e) => JSON.parse(e.meta));
      const fractions = await this.worker.request("missing", {
        path: await this.sourcePath(source),
        epochs: metas,
      });
      const update = this.db.prepare("UPDATE epochs SET meta=? WHERE id=?");
      this.transaction(() =>
        epochs.forEach((e, i) =>
          update.run(
            JSON.stringify({ ...metas[i], stageMissing: fractions[i] }),
            e.id,
          ),
        ),
      );
    }
  }
  // Apply the missing-data rule to every epoch without a person's decision, in
  // one source or the whole project. Epochs that no longer exceed the threshold
  // return to unreviewed. Each change is recorded in the audit history.
  applyAutoExclude(sourceId) {
    const rule = this.autoExclude();
    const rows = this.db
      .prepare(
        `SELECT * FROM epochs WHERE reviewer IN ('', ?)${sourceId ? " AND source_id=?" : ""}`,
      )
      .all(AUTO_REVIEWER, ...(sourceId ? [sourceId] : []));
    const now = new Date().toISOString();
    const fields = ["status", "reason", "reviewer", "stage", "updated_at"];
    const record = this.db.prepare(
      "INSERT INTO actions (epoch_id, kind, before_json, after_json, at) VALUES (?, 'auto', ?, ?, ?)",
    );
    let excluded = 0;
    this.transaction(() => {
      for (const row of rows) {
        const meta = JSON.parse(row.meta);
        const stage = rule.stage === "final" ? meta.finalStage : rule.stage;
        const fraction = !meta.stages.includes(stage)
          ? null
          : (meta.stageMissing?.[stage] ??
            (stage === meta.finalStage ? row.missing : null));
        const after =
          rule.enabled && fraction !== null && fraction * 100 > rule.threshold
            ? {
                status: "exclude",
                reason: `Excessive missing data: ${(fraction * 100).toFixed(1)}% of samples missing in ${stage}, above the ${rule.threshold}% automatic exclusion threshold`,
                reviewer: AUTO_REVIEWER,
                stage,
                updated_at: now,
              }
            : row.reviewer === AUTO_REVIEWER
              ? {
                  status: "unreviewed",
                  reason: "",
                  reviewer: "",
                  stage: "",
                  updated_at: now,
                }
              : null;
        if (after?.status === "exclude") excluded += 1;
        if (
          !after ||
          (row.status === after.status && row.reason === after.reason)
        )
          continue;
        const before = Object.fromEntries(fields.map((key) => [key, row[key]]));
        this.setDecision(row.id, after);
        record.run(row.id, JSON.stringify(before), JSON.stringify(after), now);
      }
    });
    return excluded;
  }
  list(filters = {}) {
    const where = [];
    const args = [];
    if (filters.status && filters.status !== "all") {
      if (!states.includes(filters.status))
        throw new Error("Invalid review status.");
      where.push("status = ?");
      args.push(filters.status);
    }
    if (filters.participant) {
      where.push("participant = ?");
      args.push(filters.participant);
    }
    if (filters.run) {
      where.push("run = ?");
      args.push(filters.run);
    }
    if (filters.search) {
      const search = String(filters.search).slice(0, 200);
      where.push(
        "(instr(lower(event), lower(?)) > 0 OR instr(lower(trial), lower(?)) > 0 OR instr(lower(label), lower(?)) > 0 OR instr(lower(participant), lower(?)) > 0)",
      );
      args.push(search, search, search, search);
    }
    if (filters.stage && filters.stage !== "final") {
      where.push(
        "EXISTS (SELECT 1 FROM json_each(epochs.meta, '$.stages') WHERE value=?)",
      );
      args.push(filters.stage);
    }
    const sql = where.length ? `WHERE ${where.join(" AND ")}` : "";
    const offset = Math.max(0, Math.trunc(Number(filters.offset) || 0));
    const sort =
      filters.sort === "missing"
        ? "missing DESC, source_id, label, block, ordinal"
        : "participant, session, task, length(run), run, source_id, label, block, ordinal";
    const total = this.db
      .prepare(`SELECT COUNT(*) AS n FROM epochs ${sql}`)
      .get(...args).n;
    const rows = this.db
      .prepare(`SELECT * FROM epochs ${sql} ORDER BY ${sort} LIMIT 80 OFFSET ?`)
      .all(...args, offset)
      .map((row) => this.deserialize(row));
    return { rows, total, offset };
  }
  deserialize(row) {
    return { ...row, meta: JSON.parse(row.meta) };
  }
  epoch(id) {
    const row = this.db.prepare("SELECT * FROM epochs WHERE id=?").get(id);
    if (!row) throw new Error("Epoch was not found in this project.");
    return this.deserialize(row);
  }
  async sourcePath(id) {
    const file = path.join(this.directory, "sources", `${id}.rds`);
    if (!this.verified.has(id)) {
      if ((await fileHash(file)) !== id)
        throw new Error(
          "An imported source has changed. Reimport it as a new source before reviewing.",
        );
      this.verified.add(id);
    }
    return file;
  }
  async trace(id, stage, range) {
    const epoch = this.epoch(id);
    const resolved = stage === "final" ? epoch.meta.finalStage : stage;
    if (!epoch.meta.stages.includes(resolved))
      throw new Error("This epoch does not contain the selected stage.");
    return this.worker.request("trace", {
      path: await this.sourcePath(epoch.source_id),
      epoch: epoch.meta,
      stage: resolved,
      range,
    });
  }
  decision(input) {
    if (!states.includes(input.status))
      throw new Error("Invalid review status.");
    const epoch = this.epoch(input.id);
    const stage = input.stage === "final" ? epoch.meta.finalStage : input.stage;
    if (!epoch.meta.stages.includes(stage))
      throw new Error("Invalid review stage.");
    if (
      typeof input.reviewer !== "string" ||
      !input.reviewer.trim() ||
      input.reviewer.length > 200
    )
      throw new Error("Enter a reviewer name.");
    if (input.reviewer.trim() === AUTO_REVIEWER)
      throw new Error(
        "That reviewer name is reserved for automatic exclusions.",
      );
    if (typeof input.reason !== "string" || input.reason.length > 2000)
      throw new Error("The note is too long.");
    const fields = ["status", "reason", "reviewer", "stage", "updated_at"];
    const before = Object.fromEntries(fields.map((key) => [key, epoch[key]]));
    const after = {
      status: input.status,
      reason: input.reason,
      reviewer: input.reviewer.trim(),
      stage,
      updated_at: new Date().toISOString(),
    };
    this.transaction(() => {
      this.setDecision(epoch.id, after);
      this.db
        .prepare(
          "INSERT INTO actions (epoch_id, kind, before_json, after_json, at) VALUES (?, ?, ?, ?, ?)",
        )
        .run(
          epoch.id,
          "decision",
          JSON.stringify(before),
          JSON.stringify(after),
          after.updated_at,
        );
    });
    return this.epoch(epoch.id);
  }
  setDecision(id, d) {
    this.db
      .prepare(
        "UPDATE epochs SET status=?, reason=?, reviewer=?, stage=?, updated_at=? WHERE id=?",
      )
      .run(d.status, d.reason, d.reviewer, d.stage, d.updated_at, id);
  }
  undo() {
    const action = this.db
      .prepare(
        "SELECT * FROM actions WHERE kind='decision' AND undone=0 ORDER BY seq DESC LIMIT 1",
      )
      .get();
    if (!action) return null;
    this.transaction(() => {
      this.setDecision(action.epoch_id, JSON.parse(action.before_json));
      this.db
        .prepare("UPDATE actions SET undone=1 WHERE seq=?")
        .run(action.seq);
      this.db
        .prepare(
          "INSERT INTO actions (epoch_id, kind, before_json, after_json, at) VALUES (?, ?, ?, ?, ?)",
        )
        .run(
          action.epoch_id,
          "undo",
          action.after_json,
          action.before_json,
          new Date().toISOString(),
        );
    });
    return this.epoch(action.epoch_id);
  }
  async export(destination) {
    const rows = this.db
      .prepare("SELECT * FROM epochs ORDER BY source_id, label, block, ordinal")
      .all()
      .map((row) => this.deserialize(row));
    if (!rows.length) throw new Error("Import epochs before exporting.");
    const name = `eyeris-review-${new Date().toISOString().replaceAll(":", "-").replaceAll(".", "-")}-${randomUUID().slice(0, 8)}`;
    const staging = path.join(destination, `.${name}.tmp`);
    const target = path.join(destination, name);
    await mkdir(staging, { recursive: true });
    try {
      const sources = this.summary().sources;
      const manifest = {
        schemaVersion: 1,
        exportedAt: new Date().toISOString(),
        application: `${packageMetadata.name}/${packageMetadata.version}`,
        policy:
          "Only explicitly kept epochs are retained. Unreviewed epochs are exported separately. All stored stages and original samples are preserved.",
        // Exclusions made by this rule have the reviewer "eyeris auto-exclude".
        autoExclude: this.autoExclude(),
        sources,
        decisions: rows.map(({ meta, ...row }) => ({ ...row, locator: meta })),
        history: this.db.prepare("SELECT * FROM actions ORDER BY seq").all(),
      };
      await writeFile(
        path.join(staging, "manifest.json"),
        JSON.stringify(manifest, null, 2),
      );
      const cols = [
        "id",
        "source_id",
        "participant",
        "session",
        "task",
        "run",
        "label",
        "block",
        "eye",
        "trial",
        "event",
        "status",
        "reason",
        "reviewer",
        "stage",
        "updated_at",
      ];
      await writeFile(
        path.join(staging, "decisions.csv"),
        csv([cols, ...rows.map((r) => cols.map((c) => r[c]))]),
      );
      for (const source of sources) {
        const epochs = rows
          .filter((r) => r.source_id === source.id)
          .map((r) => ({ ...r.meta, id: r.id, status: r.status }));
        // Recheck before exporting even when this source was previously viewed.
        this.verified.delete(source.id);
        await this.worker.request("export", {
          path: await this.sourcePath(source.id),
          epochs,
          destination: path.join(staging, source.id),
        });
      }
      await writeFile(
        path.join(staging, "README.txt"),
        "eyeris review export\n\nEach source SHA-256 folder contains retained/, excluded/, and unreviewed/.\nEach table is saved as CSV and RDS, preserving all original columns and samples.\n.review_epoch_id joins samples to decisions.csv and manifest.json.\nTable filenames hex-encode eye/epoch-label/block to avoid naming collisions.\nThese are epoch tables, not complete eyeris pipeline objects. Continuous signals,\nbaseline lists and confounds are not filtered by this export.\nDecisions apply to individual epochs across all stored stages, not sibling\nepoch definitions or the other eye. No unreviewed epoch is treated as kept.\nRead an RDS with readRDS(); CSV missing values are empty fields.\n",
      );
      await rename(staging, target);
      return { directory: target, counts: this.summary().counts };
    } catch (error) {
      await rm(staging, { recursive: true, force: true });
      throw error;
    }
  }
}
