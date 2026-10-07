import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import {
  readdir,
  readFile,
  writeFile,
  mkdir,
  link,
  copyFile,
  stat,
} from "node:fs/promises";
import path from "node:path";
import yaml from "js-yaml";
import semver from "semver";

export const repository = "shawntz/eyeris";
export const channelTag = "desktop-latest";
export function releaseTag(version) {
  if (!semver.valid(version) || semver.prerelease(version))
    throw new Error("Desktop releases require a stable semantic version");
  return `desktop-v${version}`;
}
export function validateTag(tag, version) {
  if (tag !== releaseTag(version))
    throw new Error("Desktop tag must match desktop/package.json exactly");
}
export function assertNewer(version, current) {
  if (current && !semver.gt(version, current))
    throw new Error(`Desktop ${version} must be newer than ${current}`);
}
export async function hash(file, algorithm = "sha512", encoding = "base64") {
  const sum = createHash(algorithm);
  for await (const chunk of createReadStream(file)) sum.update(chunk);
  return sum.digest(encoding);
}
async function walk(directory) {
  const result = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const file = path.join(directory, entry.name);
    if (entry.isDirectory()) result.push(...(await walk(file)));
    else if (entry.isFile()) result.push(file);
  }
  return result;
}
export async function prepareRelease(source, output, version) {
  const tag = releaseTag(version);
  const base = `https://github.com/${repository}/releases/download/${tag}/`;
  const assets = new Map();
  const channels = new Map();
  for (const file of await walk(source)) {
    const name = path.basename(file);
    if (/^latest(?:-mac|-linux)?\.yml$/.test(name)) {
      const info = yaml.load(await readFile(file, "utf8"));
      if (
        info.version !== version ||
        !Array.isArray(info.files) ||
        !info.files.length
      )
        throw new Error(`Invalid update metadata: ${name}`);
      const merged = channels.get(name) || {
        version,
        files: [],
        releaseDate: info.releaseDate,
      };
      for (const item of info.files) {
        if (
          typeof item.url !== "string" ||
          item.url !== path.basename(item.url) ||
          !item.url.startsWith(`eyeris-${version}-`)
        )
          throw new Error(`Unexpected update artifact: ${item.url}`);
        if (merged.files.some((existing) => existing.url === item.url))
          throw new Error(`Duplicate update artifact: ${item.url}`);
        merged.files.push(item);
      }
      channels.set(name, merged);
    } else if (
      name.startsWith(`eyeris-${version}-`) &&
      /\.(exe|dmg|zip|AppImage|blockmap)$/.test(name)
    ) {
      if (assets.has(name))
        throw new Error(`Duplicate release artifact: ${name}`);
      assets.set(name, file);
    }
  }
  const aliases = {
    "eyeris-windows-x64.exe": `eyeris-${version}-win-x64.exe`,
    "eyeris-macos-arm64.dmg": `eyeris-${version}-mac-arm64.dmg`,
    "eyeris-linux-x64.AppImage": `eyeris-${version}-linux-x86_64.AppImage`,
  };
  const required = [
    ...Object.values(aliases),
    `eyeris-${version}-mac-arm64.zip`,
  ];
  for (const name of required)
    if (!assets.has(name))
      throw new Error(`Missing platform artifact: ${name}`);
  for (const name of ["latest.yml", "latest-mac.yml", "latest-linux.yml"]) {
    if (!channels.has(name)) throw new Error(`Missing update channel: ${name}`);
  }
  const referenced = new Set();
  for (const info of channels.values()) {
    for (const item of info.files) {
      const file = assets.get(item.url);
      if (
        !file ||
        (await hash(file)) !== item.sha512 ||
        (await stat(file)).size !== item.size
      )
        throw new Error(`Update artifact checksum/size mismatch: ${item.url}`);
      referenced.add(item.url);
      item.url = base + item.url;
    }
    info.path = info.files[0].url;
    info.sha512 = info.files[0].sha512;
  }
  for (const name of required.filter((name) => !name.endsWith(".dmg"))) {
    if (!referenced.has(name))
      throw new Error(`Artifact absent from update metadata: ${name}`);
  }
  await mkdir(output, { recursive: true });
  for (const [name, file] of assets) {
    await link(file, path.join(output, name)).catch(() =>
      copyFile(file, path.join(output, name)),
    );
  }
  for (const [name, info] of channels)
    await writeFile(path.join(output, name), yaml.dump(info));
  const names = [...assets.keys(), ...channels.keys()];
  const sums = await Promise.all(
    names.map(
      async (name) =>
        `${await hash(path.join(output, name), "sha256", "hex")}  ${name}`,
    ),
  );
  await writeFile(path.join(output, "SHA256SUMS"), sums.join("\n") + "\n");
  await writeFile(
    path.join(output, "desktop-channel.json"),
    JSON.stringify({ version, tag }) + "\n",
  );
  return {
    tag,
    aliases,
    files: [...names, "SHA256SUMS", "desktop-channel.json"],
    channels: [...channels.keys()],
  };
}
