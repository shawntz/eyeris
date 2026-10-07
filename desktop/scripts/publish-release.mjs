import { execFileSync } from "node:child_process";
import { readFile, mkdtemp, copyFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { findRelease } from "./github-releases.mjs";
import {
  repository,
  channelTag,
  validateTag,
  assertNewer,
  prepareRelease,
  hash,
} from "./release-assets.mjs";

const version = JSON.parse(
  await readFile(new URL("../package.json", import.meta.url)),
).version;
validateTag(
  process.env.DESKTOP_RELEASE_TAG || process.env.GITHUB_REF_NAME,
  version,
);
const gh = (...args) =>
  execFileSync("gh", args, { encoding: "utf8", maxBuffer: 16 * 1024 * 1024 });
const api = (route, data) =>
  JSON.parse(
    execFileSync(
      "gh",
      [
        "api",
        `repos/${repository}/${route}`,
        ...(data ? ["--method", "PATCH", "--input", "-"] : []),
      ],
      { input: data ? JSON.stringify(data) : undefined, encoding: "utf8" },
    ),
  );
const release = (tag, options) => findRelease(api, tag, options);
const channel = release(channelTag);
let current = null;
if (channel && !channel.draft) {
  const index = channel.assets.find(
    (asset) => asset.name === "desktop-channel.json",
  );
  if (!index)
    throw new Error(
      "Published desktop channel has no version index; repair it before promotion",
    );
  current = JSON.parse(
    gh(
      "api",
      "-H",
      "Accept: application/octet-stream",
      `repos/${repository}/releases/assets/${index.id}`,
    ),
  ).version;
}
// Equal versions may resume an interrupted promotion, but never replace an
// already-published binary with a rebuilt, differently signed artifact.
if (current !== version) assertNewer(version, current);
const out = await mkdtemp(path.join(tmpdir(), "eyeris-release-"));
const plan = await prepareRelease(
  path.resolve(process.argv[2] || "release-input"),
  out,
  version,
);
const existing = release(plan.tag);
if (existing && !existing.draft) {
  for (const name of plan.files) {
    const asset = existing.assets.find((entry) => entry.name === name);
    const digest = `sha256:${await hash(path.join(out, name), "sha256", "hex")}`;
    if (asset?.digest !== digest)
      throw new Error(
        `Published asset differs or lacks a digest: ${name}. Use a new desktop version.`,
      );
  }
} else {
  if (!existing)
    gh(
      "release",
      "create",
      plan.tag,
      "--repo",
      repository,
      "--verify-tag",
      "--draft",
      "--latest=false",
      "--title",
      `eyeris Desktop ${version}`,
      "--notes",
      "Self-contained desktop installers. R and Pandoc are included. Desktop versioning is independent of the R package.",
    );
  gh(
    "release",
    "upload",
    plan.tag,
    ...plan.files.map((name) => path.join(out, name)),
    "--repo",
    repository,
    "--clobber",
  );
  const staged = release(plan.tag, { required: true });
  api(`releases/${staged.id}`, { draft: false, make_latest: "false" });
}
if (!channel)
  gh(
    "release",
    "create",
    channelTag,
    "--repo",
    repository,
    "--target",
    execFileSync("git", ["rev-parse", "HEAD"], { encoding: "utf8" }).trim(),
    "--draft",
    "--latest=false",
    "--title",
    "eyeris Desktop — latest downloads",
    "--notes",
    "Desktop download channel.",
  );
// Installers first, metadata last. Metadata always points at the complete,
// versioned release above, so checks cannot download a half-published update.
for (const [alias, name] of Object.entries(plan.aliases)) {
  await copyFile(path.join(out, name), path.join(out, alias));
  gh(
    "release",
    "upload",
    channelTag,
    path.join(out, alias),
    "--repo",
    repository,
    "--clobber",
  );
}
gh(
  "release",
  "upload",
  channelTag,
  ...[...plan.channels, "desktop-channel.json"].map((name) =>
    path.join(out, name),
  ),
  "--repo",
  repository,
  "--clobber",
);
const notes =
  `Latest desktop version: **${version}**.\n\n` +
  Object.keys(plan.aliases)
    .map(
      (name) =>
        `- [${name}](https://github.com/${repository}/releases/download/${channelTag}/${name})`,
    )
    .join("\n") +
  `\n\n[Versioned release and checksums](https://github.com/${repository}/releases/tag/${plan.tag}). Desktop releases are independent of the R/CRAN package.\n`;
api(`releases/${release(channelTag, { required: true }).id}`, {
  body: notes,
  draft: false,
  make_latest: "false",
});
console.log(`Published ${plan.tag} and updated ${channelTag}.`);
