import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, access } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import yaml from "js-yaml";
import { prepareSigningKeychain } from "../scripts/mac-signing.mjs";

test("Mac signing uses separate certificate and keychain passwords and preserves existing keychains", async () => {
  const directory = await mkdtemp(path.join(tmpdir(), "eyeris-signing-test-"));
  const calls = [];
  const masked = [];
  try {
    const keychain = await prepareSigningKeychain({
      directory,
      certificate: Buffer.from("test certificate").toString("base64"),
      certificatePassword: "p12 password",
      password: "temporary keychain password",
      mask: (value) => masked.push(value),
      run: (args) => {
        calls.push(args);
        if (args[0] === "list-keychains" && args.length === 3)
          return '    "/existing/login.keychain-db"\n    "/existing/other keychain"\n';
        if (args[0] === "find-identity")
          return '  1) 123456 "Developer ID Application: Test (TEAM)"';
        return "";
      },
    });
    assert.equal(
      calls.find((args) => args[0] === "import").at(-1),
      "p12 password",
    );
    assert.equal(
      calls.find((args) => args[0] === "set-key-partition-list").at(-2),
      "temporary keychain password",
    );
    assert.deepEqual(
      calls.find((args) => args[0] === "list-keychains" && args.includes("-s")),
      [
        "list-keychains",
        "-d",
        "user",
        "-s",
        keychain,
        "/existing/login.keychain-db",
        "/existing/other keychain",
      ],
    );
    assert.deepEqual(masked, ["temporary keychain password"]);
    await assert.rejects(access(path.join(directory, "eyeris-signing.p12")), {
      code: "ENOENT",
    });
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
test("Mac signing removes exported private-key files even when import fails", async () => {
  const directory = await mkdtemp(
    path.join(tmpdir(), "eyeris-signing-failure-"),
  );
  try {
    await assert.rejects(
      prepareSigningKeychain({
        directory,
        certificate: "dGVzdA==",
        certificatePassword: "test",
        run: () => {
          throw new Error("import failed");
        },
      }),
      /import failed/,
    );
    await assert.rejects(access(path.join(directory, "eyeris-signing.p12")), {
      code: "ENOENT",
    });
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
test("Mac releases use the prepared keychain and always clean signing material", async () => {
  const workflow = yaml.load(
    await readFile(
      new URL("../../.github/workflows/desktop.yml", import.meta.url),
      "utf8",
    ),
  );
  const steps = workflow.jobs.desktop.steps;
  const build = steps.find(
    (step) => step.name === "Build signed native installer",
  );
  assert.match(build.run, /unset CSC_LINK CSC_KEY_PASSWORD/);
  assert.match(build.run, /test -n "\$CSC_KEYCHAIN"/);
  assert.match(
    steps.find((step) => step.name === "Prepare Apple signing keychain").if,
    /inputs.release/,
  );
  const cleanup = steps.find(
    (step) => step.name === "Remove Apple signing credentials",
  );
  assert.match(cleanup.if, /always\(\)/);
  assert.match(cleanup.run, /delete-keychain/);
  assert.match(cleanup.run, /eyeris-notarization.p8/);
});
