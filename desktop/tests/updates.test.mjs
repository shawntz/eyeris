import { test } from "node:test";
import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import { DesktopUpdates } from "../electron/updates.mjs";
const flush = () => new Promise((resolve) => setImmediate(resolve));
function setup(t, enabled = true, busy = () => false) {
  const updater = new EventEmitter();
  updater.checkForUpdates = t.mock.fn(async () =>
    updater.emit("update-available", { version: "0.3.0" }),
  );
  updater.downloadUpdate = t.mock.fn(async () => {
    updater.emit("download-progress", { percent: 57.2 });
    updater.emit("update-downloaded", { version: "0.3.0" });
  });
  updater.quitAndInstall = t.mock.fn();
  const updates = new DesktopUpdates({
    updater,
    enabled,
    version: "0.2.0",
    busy,
  });
  t.after(() => updates.stop());
  return { updates, updater };
}
test("update check is single-flight, downloads require consent, and restart waits for idle", async (t) => {
  let busy = true;
  const { updates, updater } = setup(t, true, () => busy);
  assert.equal(updater.autoDownload, false);
  assert.equal(updater.autoInstallOnAppQuit, false);
  assert.equal(updater.allowDowngrade, false);
  assert.equal(updater.allowPrerelease, false);
  updates.check();
  updates.check();
  await flush();
  assert.equal(updater.checkForUpdates.mock.callCount(), 1);
  assert.equal(updater.downloadUpdate.mock.callCount(), 0);
  assert.equal(updates.snapshot().status, "available");
  updates.download();
  await flush();
  assert.equal(updates.snapshot().status, "downloaded");
  updates.check();
  assert.equal(updater.checkForUpdates.mock.callCount(), 1);
  assert.throws(() => updates.install(), /processing/);
  assert.equal(updater.quitAndInstall.mock.callCount(), 0);
  busy = false;
  updates.install();
  await flush();
  assert.deepEqual(updater.quitAndInstall.mock.calls[0].arguments, [
    false,
    true,
  ]);
});
test("network errors are recoverable without interrupting the app", async (t) => {
  const { updates, updater } = setup(t);
  updater.checkForUpdates.mock.mockImplementationOnce(async () => {
    throw new Error("offline");
  });
  updates.check();
  await flush();
  assert.equal(updates.snapshot().status, "error");
  updates.check();
  await flush();
  assert.equal(updates.snapshot().status, "available");
  assert.throws(() => updates.install(), /Download/);
});
test("automatic checks run after startup and periodically, and stop on quit", async (t) => {
  t.mock.timers.enable({ apis: ["setTimeout", "setInterval"] });
  const { updates, updater } = setup(t);
  updates.start();
  t.mock.timers.tick(29_999);
  await flush();
  assert.equal(updater.checkForUpdates.mock.callCount(), 0);
  t.mock.timers.tick(1);
  await flush();
  assert.equal(updater.checkForUpdates.mock.callCount(), 1);
  t.mock.timers.tick(6 * 60 * 60 * 1000);
  await flush();
  assert.equal(updater.checkForUpdates.mock.callCount(), 2);
  updates.stop();
  t.mock.timers.tick(6 * 60 * 60 * 1000);
  await flush();
  assert.equal(updater.checkForUpdates.mock.callCount(), 2);
});
test("development and unsupported installs never contact the update server", async (t) => {
  const { updates, updater } = setup(t, false);
  updates.start();
  updates.check();
  updates.download();
  await flush();
  assert.equal(updater.checkForUpdates.mock.callCount(), 0);
  assert.equal(updater.downloadUpdate.mock.callCount(), 0);
  assert.equal(updates.snapshot().status, "unavailable");
});
