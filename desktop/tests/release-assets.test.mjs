import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import yaml from "js-yaml";
import {
  prepareRelease,
  hash,
  validateTag,
  assertNewer,
} from "../scripts/release-assets.mjs";
async function fixture(t) {
  const root = await mkdtemp(path.join(tmpdir(), "eyeris-release-test-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const source = path.join(root, "source");
  const output = path.join(root, "output");
  const matrix = [
    ["win-x64", "latest.yml", ["exe"]],
    ["mac-arm64", "latest-mac.yml", ["zip", "dmg"]],
    ["linux-x86_64", "latest-linux.yml", ["AppImage"]],
  ];
  for (const [target, channel, extensions] of matrix) {
    const dir = path.join(source, target);
    await mkdir(dir, { recursive: true });
    const files = [];
    for (const ext of extensions) {
      const name = `eyeris-0.3.0-${target}.${ext}`;
      const file = path.join(dir, name);
      await writeFile(file, "test artifact");
      files.push({ url: name, sha512: await hash(file), size: 13 });
    }
    await writeFile(
      path.join(dir, channel),
      yaml.dump({ version: "0.3.0", files }),
    );
  }
  return { source, output };
}
test("desktop tags and version promotion are independent of CRAN and monotonic", () => {
  validateTag("desktop-v0.3.0", "0.3.0");
  assert.throws(() => validateTag("v3.3.0", "0.3.0"), /Desktop tag/);
  assert.throws(() => validateTag("desktop-v0.2.0", "0.3.0"), /Desktop tag/);
  assert.throws(
    () => validateTag("desktop-v0.3.0-beta.1", "0.3.0-beta.1"),
    /stable/,
  );
  assertNewer("0.3.0", "0.2.0");
  assert.throws(() => assertNewer("0.2.0", "0.3.0"), /newer/);
});
test("three-platform releases need no Intel artifacts and use immutable update URLs", async (t) => {
  const { source, output } = await fixture(t);
  const plan = await prepareRelease(source, output, "0.3.0");
  assert.deepEqual(Object.keys(plan.aliases).sort(), [
    "eyeris-linux-x64.AppImage",
    "eyeris-macos-arm64.dmg",
    "eyeris-windows-x64.exe",
  ]);
  const mac = yaml.load(
    await readFile(path.join(output, "latest-mac.yml"), "utf8"),
  );
  assert.equal(mac.files.length, 2);
  assert.ok(mac.files.every((item) => item.url.includes("mac-arm64")));
  for (const item of mac.files)
    assert.match(
      item.url,
      /^https:\/\/github.com\/shawntz\/eyeris\/releases\/download\/desktop-v0\.3\.0\/eyeris-/,
    );
  assert.match(
    await readFile(path.join(output, "SHA256SUMS"), "utf8"),
    /latest-linux.yml/,
  );
});
test("each active platform is still required for release preparation", async (t) => {
  for (const target of ["win-x64", "mac-arm64", "linux-x86_64"]) {
    await t.test(target, async (t) => {
      const { source, output } = await fixture(t);
      await rm(path.join(source, target), { recursive: true });
      await assert.rejects(
        prepareRelease(source, output, "0.3.0"),
        /Missing platform/,
      );
    });
  }
});
test("artifact corruption prevents release preparation", async (t) => {
  const { source, output } = await fixture(t);
  await writeFile(
    path.join(source, "win-x64", "eyeris-0.3.0-win-x64.exe"),
    "corrupted",
  );
  await assert.rejects(prepareRelease(source, output, "0.3.0"), /checksum/);
});
