import test from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  mkdir,
  writeFile,
  readFile,
  readdir,
  rm,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { runWithWindowsRecovery } from "../electron/processing-recovery.mjs";

async function fixture(t) {
  const directory = await mkdtemp(path.join(tmpdir(), "eyeris-recovery-"));
  t.after(() => rm(directory, { recursive: true, force: true }));
  await mkdir(path.join(directory, "bids"));
  await writeFile(path.join(directory, "config.json"), "original settings");
  return {
    directory,
    output: path.join(directory, "result.rds"),
    platform: "win32",
    isCancelled: () => false,
  };
}

for (const code of [3221225477, -1073741819]) {
  test(`Windows native crash ${code} retries once with no partial outputs`, async (t) => {
    const options = await fixture(t);
    const result = await runWithWindowsRecovery({
      ...options,
      async run(attempt) {
        if (attempt === 1) {
          await writeFile(
            path.join(options.directory, "bids", "partial.db"),
            "incomplete database",
          );
          await writeFile(options.output, "partial RDS");
          await writeFile(
            path.join(options.directory, "runtime.json"),
            "incomplete metadata",
          );
          await writeFile(
            path.join(options.directory, "process.log"),
            "original crash details",
          );
          return { code };
        }
        assert.equal(attempt, 2);
        assert.deepEqual(
          await readdir(path.join(options.directory, "bids")),
          [],
        );
        await assert.rejects(readFile(options.output), { code: "ENOENT" });
        await assert.rejects(
          readFile(path.join(options.directory, "runtime.json")),
          { code: "ENOENT" },
        );
        assert.equal(
          await readFile(
            path.join(options.directory, "failed-attempt-1", "process.log"),
            "utf8",
          ),
          "original crash details",
        );
        assert.equal(
          await readFile(
            path.join(
              options.directory,
              "failed-attempt-1",
              "bids",
              "partial.db",
            ),
            "utf8",
          ),
          "incomplete database",
        );
        assert.equal(
          await readFile(path.join(options.directory, "config.json"), "utf8"),
          "original settings",
        );
        await writeFile(
          path.join(options.directory, "bids", "complete.csv"),
          "complete output",
        );
        return { code: 0 };
      },
    });
    assert.equal(result.code, 0);
    assert.deepEqual(await readdir(path.join(options.directory, "bids")), [
      "complete.csv",
    ]);
    assert.equal(
      JSON.parse(await readFile(path.join(options.directory, "recovery.json")))
        .exitCode,
      code,
    );
  });
}

test("a second native crash stays failed and cannot loop", async (t) => {
  const options = await fixture(t);
  let attempts = 0;
  const result = await runWithWindowsRecovery({
    ...options,
    run: async () => {
      attempts++;
      return { code: 3221225477 };
    },
  });
  assert.equal(attempts, 2);
  assert.equal(result.code, 3221225477);
});

test("ordinary errors, signals, cancellation, spawn failures, and other platforms never retry", async (t) => {
  const cases = [
    { result: { code: 0 } },
    { result: { code: 1 } },
    { result: { code: null, signal: "SIGTERM" } },
    { result: { code: 3221225477, spawnError: new Error("cannot spawn") } },
    { result: { code: 3221225477 }, isCancelled: () => true },
    { result: { code: 3221225477 }, platform: "darwin" },
    { result: { code: 3221225477 }, platform: "linux" },
  ];
  for (const { result, ...override } of cases) {
    let attempts = 0;
    const options = await fixture(t);
    assert.equal(
      await runWithWindowsRecovery({
        ...options,
        ...override,
        run: async () => {
          attempts++;
          return result;
        },
      }),
      result,
    );
    assert.equal(attempts, 1);
    assert.deepEqual((await readdir(options.directory)).sort(), [
      "bids",
      "config.json",
    ]);
  }
});

test("cancellation during recovery prevents a new R process", async (t) => {
  const options = await fixture(t);
  let cancelled = false;
  let attempts = 0;
  await runWithWindowsRecovery({
    ...options,
    run: async () => {
      attempts++;
      return { code: 3221225477 };
    },
    onRetry: () => {
      cancelled = true;
    },
    isCancelled: () => cancelled,
  });
  assert.equal(attempts, 1);
});

test("an archive failure prevents retrying into dirty outputs", async (t) => {
  const options = await fixture(t);
  await mkdir(path.join(options.directory, "failed-attempt-1"));
  let attempts = 0;
  await assert.rejects(
    runWithWindowsRecovery({
      ...options,
      run: async () => {
        attempts++;
        return { code: 3221225477 };
      },
    }),
    { code: "EEXIST" },
  );
  assert.equal(attempts, 1);
});
