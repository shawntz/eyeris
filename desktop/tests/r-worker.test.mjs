import { test } from "node:test";
import assert from "node:assert/strict";
import childProcess from "node:child_process";
import { EventEmitter } from "node:events";
import { syncBuiltinESMExports } from "node:module";
import { PassThrough } from "node:stream";
import { RWorker } from "../electron/r-worker.mjs";

function fakeWorker(t) {
  const child = new EventEmitter();
  child.stdin = new PassThrough();
  child.stdout = new PassThrough();
  child.stderr = new PassThrough();
  child.kill = t.mock.fn(() => child.emit("exit", null, "SIGTERM"));
  t.mock.method(childProcess, "spawn", () => child);
  syncBuiltinESMExports();
  const previous = process.env.EYERIS_RSCRIPT;
  process.env.EYERIS_RSCRIPT = process.execPath;
  const worker = new RWorker();
  t.after(() => {
    worker.close();
    child.stdin.destroy();
    child.stdout.destroy();
    child.stderr.destroy();
    t.mock.restoreAll();
    syncBuiltinESMExports();
    if (previous === undefined) delete process.env.EYERIS_RSCRIPT;
    else process.env.EYERIS_RSCRIPT = previous;
  });
  return { worker, child };
}

for (const [code, signal, expected] of [
  [null, "SIGTERM", /R worker exited \(signal SIGTERM\)/],
  [7, null, /R worker exited \(code 7\)/],
]) {
  test(`R worker reports ${signal || `exit code ${code}`}`, async (t) => {
    const { worker, child } = fakeWorker(t);
    const rejected = assert.rejects(worker.request("ping"), expected);
    child.emit("exit", code, signal);
    await rejected;
    assert.equal(worker.pending.size, 0);
    assert.equal(worker.child, null);
  });
}

test("timed-out request rejects before termination rejects other requests", async (t) => {
  t.mock.timers.enable({ apis: ["setTimeout"] });
  const { worker, child } = fakeWorker(t);
  const timedOut = assert.rejects(
    worker.request("slow"),
    /R worker request slow timed out after 300 seconds/,
  );
  const timedOutId = worker.counter;
  t.mock.timers.tick(1_000);
  const interrupted = assert.rejects(
    worker.request("queued"),
    /R worker exited \(signal SIGTERM\)/,
  );
  child.kill.mock.mockImplementation(() => {
    assert.equal(worker.pending.has(timedOutId), false);
    child.emit("exit", null, "SIGTERM");
  });
  t.mock.timers.tick(299_000);
  await Promise.all([timedOut, interrupted]);
  assert.equal(child.kill.mock.callCount(), 1);
  assert.equal(worker.pending.size, 0);
  assert.equal(worker.child, null);
});
