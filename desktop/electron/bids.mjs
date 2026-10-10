import { open, readdir, stat } from "node:fs/promises";
import path from "node:path";

const label = /^[a-zA-Z0-9]{1,64}$/;
const entity = (name, key) =>
  new RegExp(`(?:^|_)${key}-([^_.]+)`, "i").exec(name)?.[1];
// macOS writes an AppleDouble file, "._" plus the name, beside each file it
// copies to a drive without native metadata support (exFAT, FAT, network
// shares). It holds Finder metadata, not data, and starts with these bytes.
const appleDouble = Buffer.from([0x00, 0x05, 0x16, 0x07]);
export async function isAppleDouble(file) {
  const handle = await open(file, "r");
  try {
    const { buffer, bytesRead } = await handle.read(Buffer.alloc(4), 0, 4, 0);
    return bytesRead === 4 && buffer.equals(appleDouble);
  } finally {
    await handle.close();
  }
}
// Hidden files, including AppleDouble files, are never recordings or tables.
const hidden = (name) => name.startsWith(".");
async function directories(dir, prefix) {
  return (await readdir(dir, { withFileTypes: true }))
    .filter((e) => e.isDirectory() && e.name.toLowerCase().startsWith(prefix))
    .map((e) => e.name)
    .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }));
}

// Find EyeLink recordings in a BIDS dataset, in sub-<label>/[ses-<label>/]eye/.
// The root may also be a single sub-<label> folder. Entities come from the
// folders and the filename, which must agree. Datasets without sessions use
// session 01. Runs are optional; the caller numbers files without one.
export async function scanBids(root) {
  const single = /^sub-/i.test(path.basename(root));
  const base = single ? path.dirname(root) : root;
  const subjects = single
    ? [path.basename(root)]
    : await directories(base, "sub-");
  const recordings = [];
  const skipped = [];
  for (const subjectFolder of subjects) {
    const subjectDir = path.join(base, subjectFolder);
    const folders = [
      { dir: subjectDir, session: undefined },
      ...(await directories(subjectDir, "ses-")).map((name) => ({
        dir: path.join(subjectDir, name),
        session: name.slice(4),
      })),
    ];
    for (const { dir, session: folderSession } of folders) {
      let entries;
      try {
        entries = await readdir(path.join(dir, "eye"), { withFileTypes: true });
      } catch (error) {
        if (error.code === "ENOENT" || error.code === "ENOTDIR") continue;
        throw error;
      }
      entries.sort((a, b) =>
        a.name.localeCompare(b.name, undefined, { numeric: true }),
      );
      for (const e of entries) {
        if (!e.isFile() || hidden(e.name) || !/\.asc$/i.test(e.name)) continue;
        const file = path.join(dir, "eye", e.name);
        const relative = path.relative(base, file);
        const subject = subjectFolder.slice(4);
        const named = {
          subject: entity(e.name, "sub"),
          session: entity(e.name, "ses"),
        };
        const session = named.session ?? folderSession ?? "01";
        const task = entity(e.name, "task");
        const run = entity(e.name, "run") ?? "";
        const reason =
          named.subject !== undefined && named.subject !== subject
            ? `the filename's sub-${named.subject} does not match its folder`
            : named.session !== undefined &&
                folderSession !== undefined &&
                named.session !== folderSession
              ? `the filename's ses-${named.session} does not match its folder`
              : !task
                ? "the filename has no task- entity"
                : ![subject, session, task].every((v) => label.test(v))
                  ? "subject, session and task labels must be letters and digits"
                  : run && !(/^\d{1,3}$/.test(run) && Number(run) > 0)
                    ? `run-${run} is not a run number from 1 to 999`
                    : "";
        // The session and task, when known, let a selective import list only
        // the skipped files it was asked for.
        if (reason) skipped.push({ file: relative, reason, session, task });
        else
          recordings.push({
            file,
            relative,
            name: e.name,
            subject,
            session,
            task,
            run: run && String(Number(run)).padStart(2, "0"),
            size: (await stat(file)).size,
          });
      }
    }
  }
  return { recordings, skipped };
}

// Parse a BIDS tab-separated file. "n/a" and empty cells are missing values.
export function parseTsv(text) {
  const lines = text.replace(/^﻿/, "").split(/\r?\n/).filter(Boolean);
  if (!lines.length) return { columns: [], rows: [] };
  const columns = lines[0].split("\t").map((c) => c.trim());
  const rows = lines.slice(1).map((line) => {
    const cells = line.split("\t");
    return Object.fromEntries(
      columns.flatMap((c, i) => {
        const v = (cells[i] ?? "").trim();
        return v && v !== "n/a" ? [[c, v]] : [];
      }),
    );
  });
  return { columns, rows };
}

// Find behavioral tables in sub-<label>/[ses-<label>/]beh/*.tsv, with the
// subject, session, task and run of each from its folders and filename.
export async function scanBehavior(root) {
  const single = /^sub-/i.test(path.basename(root));
  const base = single ? path.dirname(root) : root;
  const subjects = single
    ? [path.basename(root)]
    : await directories(base, "sub-");
  const files = [];
  for (const subjectFolder of subjects) {
    const subjectDir = path.join(base, subjectFolder);
    for (const { dir, session } of [
      { dir: subjectDir, session: "" },
      ...(await directories(subjectDir, "ses-")).map((name) => ({
        dir: path.join(subjectDir, name),
        session: name.slice(4),
      })),
    ]) {
      let entries;
      try {
        entries = await readdir(path.join(dir, "beh"), { withFileTypes: true });
      } catch (error) {
        if (error.code === "ENOENT" || error.code === "ENOTDIR") continue;
        throw error;
      }
      for (const e of entries) {
        if (!e.isFile() || hidden(e.name) || !/\.tsv$/i.test(e.name)) continue;
        const subject = subjectFolder.slice(4);
        if ((entity(e.name, "sub") ?? subject) !== subject) continue;
        const run = entity(e.name, "run") ?? "";
        files.push({
          file: path.join(dir, "beh", e.name),
          relative: path.relative(base, path.join(dir, "beh", e.name)),
          participant: subject,
          session: entity(e.name, "ses") ?? session,
          task: entity(e.name, "task") ?? "",
          run: /^\d+$/.test(run) ? String(Number(run)).padStart(2, "0") : run,
        });
      }
    }
  }
  return files;
}
