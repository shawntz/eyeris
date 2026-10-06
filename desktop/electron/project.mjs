import { DatabaseSync } from "node:sqlite";
import { createHash, randomUUID } from "node:crypto";
import { createReadStream } from "node:fs";
import { mkdir, copyFile, rename, rm, writeFile, stat } from "node:fs/promises";
import path from "node:path";

const states = ["unreviewed", "keep", "exclude"];
const digest = (text) => createHash("sha256").update(text).digest("hex");
async function fileHash(file) {
  const hash = createHash("sha256");
  for await (const chunk of createReadStream(file)) hash.update(chunk);
  return hash.digest("hex");
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
      try {
        const schema = this.db
          .prepare("SELECT value FROM metadata WHERE key=?")
          .get("schema");
        if (schema?.value !== "1") throw new Error("Unsupported schema");
      } catch {
        this.db.close();
        throw new Error(
          "This folder does not contain a compatible eyeris review project.",
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
        reason TEXT NOT NULL DEFAULT '', reviewer TEXT NOT NULL DEFAULT '', stage TEXT NOT NULL DEFAULT '', updated_at TEXT
      );
      CREATE INDEX IF NOT EXISTS epochs_queue ON epochs(status, participant, source_id, ordinal);
      CREATE TABLE IF NOT EXISTS actions (
        seq INTEGER PRIMARY KEY AUTOINCREMENT, epoch_id TEXT NOT NULL REFERENCES epochs(id),
        kind TEXT NOT NULL, before_json TEXT NOT NULL, after_json TEXT NOT NULL, at TEXT NOT NULL, undone INTEGER NOT NULL DEFAULT 0
      );`);
    const schema = this.db
      .prepare("SELECT value FROM metadata WHERE key=?")
      .get("schema");
    if (schema && schema.value !== "1") {
      this.db.close();
      throw new Error("This project was created by a different app version.");
    }
    this.db
      .prepare("INSERT OR IGNORE INTO metadata VALUES (?, ?)")
      .run("schema", "1");
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
      stages: this.db
        .prepare(
          "SELECT DISTINCT j.value AS stage FROM epochs e, json_each(e.meta, '$.stages') j",
        )
        .all()
        .map((r) => r.stage),
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
      const participant =
        /(?:^|_)sub-([^_.]+)/i.exec(name)?.[1] || name.replace(/\.rds$/i, "");
      const target = path.join(this.directory, "sources", `${id}.rds`);
      await rename(temporary, target);
      this.transaction(() => {
        this.db
          .prepare("INSERT INTO sources VALUES (?, ?, ?, ?)")
          .run(id, name, participant, new Date().toISOString());
        const insert = this.db.prepare(
          "INSERT INTO epochs (id, source_id, participant, event, trial, label, block, eye, ordinal, missing, meta) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        );
        for (const e of epochs)
          insert.run(
            digest(`${id}:${e.key}`),
            id,
            participant,
            e.event,
            e.trial,
            e.label,
            e.block,
            e.eye,
            e.ordinal,
            e.missing,
            JSON.stringify(e),
          );
      });
      return { duplicate: false, count: epochs.length };
    } finally {
      await rm(temporary, { force: true });
    }
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
        : "participant, source_id, label, block, ordinal";
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
        application: "eyeris-desktop/0.1.0",
        policy:
          "Only explicitly kept epochs are retained. Unreviewed epochs are exported separately. All stored stages and original samples are preserved.",
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
