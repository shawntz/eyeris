import { execFileSync } from "node:child_process";
import {
  cp,
  mkdir,
  readFile,
  writeFile,
  readdir,
  realpath,
  chmod,
} from "node:fs/promises";
import path from "node:path";
import { createHash } from "node:crypto";

export async function files(directory) {
  const result = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const file = path.join(directory, entry.name);
    if (entry.isDirectory()) result.push(...(await files(file)));
    else if (entry.isFile()) result.push(file);
  }
  return result;
}
const run = (command, args) =>
  execFileSync(command, args, {
    encoding: "utf8",
    maxBuffer: 16 * 1024 * 1024,
  });
const systemMac = (name) =>
  name.startsWith("/usr/lib/") || name.startsWith("/System/Library/");
const systemLinux = (name) =>
  /^(ld-linux|lib(c|m|dl|rt|pthread|resolv|util)\.so)/.test(
    path.basename(name),
  );

// Relocate every native library and its dependency closure, including package
// DLLs/shared objects. A missing dependency fails packaging instead of falling
// back to a library that happens to exist on the end user's machine.
export async function relocateNative(roots, runtime, origins = new Map()) {
  if (process.platform === "win32") return [];
  const native = [];
  for (const root of roots)
    for (const file of await files(root)) {
      const data = await readFile(file);
      const magic = data.subarray(0, 4).toString("hex");
      // Java class files share the universal Mach-O magic number.
      if (
        ["cafebabe", "bebafeca"].includes(magic) &&
        !run("/usr/bin/file", ["-b", file]).includes("Mach-O")
      )
        continue;
      if (
        process.platform === "darwin"
          ? ["cffaedfe", "cefaedfe", "cafebabe", "bebafeca"].includes(magic)
          : magic === "7f454c46"
      )
        native.push(file);
    }
  const byOriginal = new Map();
  const byName = new Map();
  for (const file of native) {
    const source = origins.get(file) || file;
    byOriginal.set(await realpath(source), file);
    byName.set(path.basename(file), file);
  }
  const extra = path.join(runtime, "native");
  await mkdir(extra, { recursive: true });
  const records = [];
  for (let index = 0; index < native.length; index++) {
    const file = native[index];
    const original = origins.get(file) || file;
    const mac = process.platform === "darwin";
    // Official Linux Pandoc is static: it has no dependencies or RPATH to
    // relocate, and ldd correctly exits nonzero for it.
    if (!mac && !/\bDYNAMIC\b/.test(run("readelf", ["--program-headers", file])))
      continue;
    const output = run(
      mac ? "/usr/bin/otool" : "ldd",
      mac ? ["-L", file] : [file],
    );
    if (!mac && output.includes("not found"))
      throw new Error(`Unresolved native dependency in ${file}: ${output}`);
    const dependencies = mac
      ? output
          .split("\n")
          .filter((line) => line.includes(" (compatibility version"))
          .map((line) => line.trim().split(" (compatibility")[0])
          .filter(Boolean)
      : [...output.matchAll(/(?:=>\s+)?(\/[^\s]+)\s+\(/g)].map(
          (match) => match[1],
        );
    const changes = [];
    for (const dependency of new Set(dependencies)) {
      if (mac ? systemMac(dependency) : systemLinux(dependency)) continue;
      let source = dependency;
      if (mac && dependency.startsWith("@loader_path/"))
        source = path.resolve(path.dirname(original), dependency.slice(13));
      if (mac && dependency.startsWith("@executable_path/"))
        source = path.resolve(path.dirname(original), dependency.slice(17));
      let target;
      try {
        target = byOriginal.get(await realpath(source));
      } catch {}
      target ||= byName.get(path.basename(dependency));
      if (!target) {
        if (!path.isAbsolute(source))
          throw new Error(`Cannot resolve ${dependency} needed by ${original}`);
        const actual = await realpath(source).catch(() => {
          throw new Error(
            `Missing native dependency ${source} required by ${original}`,
          );
        });
        const prefix = mac
          ? createHash("sha256").update(actual).digest("hex").slice(0, 10) + "-"
          : "";
        target = path.join(
          extra,
          prefix + path.basename(mac ? actual : dependency),
        );
        await cp(actual, target);
        await chmod(target, 0o755);
        origins.set(target, actual);
        byOriginal.set(actual, target);
        byName.set(path.basename(dependency), target);
        native.push(target);
        records.push({ file: path.relative(runtime, target), source: actual });
      }
      if (mac)
        changes.push(
          "-change",
          dependency,
          "@loader_path/" +
            path.relative(path.dirname(file), target).split(path.sep).join("/"),
        );
    }
    await chmod(file, 0o755);
    if (mac) {
      // Remove old signatures before modifying load commands; sign the result.
      try {
        run("/usr/bin/codesign", ["--remove-signature", file]);
      } catch {}
      if (changes.length) run("/usr/bin/install_name_tool", [...changes, file]);
      run("/usr/bin/codesign", ["--force", "--sign", "-", file]);
    } else {
      const relative = path.relative(path.dirname(file), extra);
      run("patchelf", [
        "--set-rpath",
        `$ORIGIN:${relative ? "$ORIGIN/" + relative : "$ORIGIN"}`,
        file,
      ]);
    }
  }
  return records;
}

export async function collectNativeNotices(records, runtime) {
  for (const record of records) {
    const name = path.basename(record.file);
    const destination = path.join(runtime, "native-licenses", name);
    await mkdir(destination, { recursive: true });
    const brew = record.source.match(/^(.*\/Cellar\/([^/]+)\/([^/]+))\//);
    if (brew) {
      record.package = `${brew[2]} ${brew[3]}`;
      for (const entry of await readdir(brew[1])) {
        if (/^(COPYING|LICENSE|AUTHORS|NOTICE)/i.test(entry))
          await cp(path.join(brew[1], entry), path.join(destination, entry), {
            recursive: true,
          });
      }
      const recipe = await readFile(
        path.join(brew[1], ".brew", `${brew[2]}.rb`),
        "utf8",
      );
      record.sourceUrl = recipe.match(/^\s*url "([^"]+)"/m)?.[1];
      await writeFile(path.join(destination, "homebrew-formula.rb"), recipe);
    } else if (process.platform === "linux") {
      let owner;
      for (const candidate of [
        record.source,
        record.source.replace(/^\/usr\/lib\//, "/lib/"),
      ]) {
        try {
          owner = run("dpkg-query", ["-S", candidate]).split(": ")[0];
          break;
        } catch {}
      }
      if (!owner)
        throw new Error(
          `Cannot identify license/source package for ${record.source}`,
        );
      record.package = run("dpkg-query", [
        "-W",
        "-f",
        "${Package} ${Version} ${source:Package} ${source:Version}",
        owner,
      ]).trim();
      await cp(
        path.join("/usr/share/doc", owner.split(":")[0], "copyright"),
        path.join(destination, "copyright"),
      );
    } else {
      throw new Error(
        `Supply a runtime with traceable native dependency licenses: ${record.source}`,
      );
    }
  }
}
