import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { runInNewContext } from "node:vm";
const source = readFileSync(
  new URL("../electron-builder.cjs", import.meta.url),
  "utf8",
);
function config(platform, env) {
  const context = { process: { platform, env }, module: { exports: {} } };
  runInNewContext(source, context);
  return context.module.exports;
}
test("release signing requires Apple credentials and supports optional Windows signing", () => {
  const mac = config("darwin", { EYERIS_RELEASE: "1" });
  assert.equal(mac.forceCodeSigning, true);
  assert.equal(mac.mac.notarize, true);
  assert.equal(
    config("win32", { EYERIS_RELEASE: "1" }).forceCodeSigning,
    false,
  );
  assert.equal(
    config("win32", { EYERIS_RELEASE: "1", CSC_LINK: "certificate" })
      .forceCodeSigning,
    true,
  );
  assert.equal(
    config("linux", { EYERIS_RELEASE: "1" }).forceCodeSigning,
    false,
  );
});
test("CI packages embed only the desktop feed without forcing signing", () => {
  for (const platform of ["darwin", "win32", "linux"]) {
    const result = config(platform, {});
    assert.equal(result.forceCodeSigning, false);
    assert.equal(result.mac.identity, null);
    assert.equal(result.publish.provider, "generic");
    assert.equal(
      result.publish.url,
      "https://github.com/shawntz/eyeris/releases/download/desktop-latest/",
    );
  }
});
