import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

test("desktop update UI downloads on request and offers an explicit restart", async () => {
  const directory = await mkdtemp(path.join(tmpdir(), "eyeris-update-ui-"));
  const env = { ...process.env, EYERIS_TEST_USER_DATA: directory };
  delete env.ELECTRON_RUN_AS_NODE;
  const app = await electron.launch({ args: ["."], cwd: process.cwd(), env });
  try {
    const page = await app.firstWindow();
    // Startup checks R on the shared IPC queue; wait until they finish before
    // measuring the updater UI's response to events on slower native runners.
    await expect(
      page.getByRole("button", { name: "New project", exact: true }),
    ).toBeEnabled({ timeout: 30_000 });
    // Emit genuine updater events without networking or changing the installed
    // app. Only this test process gets the mocked download/install methods.
    await app.evaluate(async ({ app }) => {
      const { createRequire } = process.getBuiltinModule("module");
      const require = createRequire(pathForPackage(app.getAppPath()));
      function pathForPackage(root: string) {
        return root + "/package.json";
      }
      const { autoUpdater } = require("electron-updater");
      autoUpdater.downloadUpdate = async () => {
        autoUpdater.emit("download-progress", { percent: 60 });
        autoUpdater.emit("update-downloaded", { version: "0.3.0" });
        return [];
      };
      autoUpdater.quitAndInstall = () => {
        autoUpdater.testInstallRequested = true;
      };
      autoUpdater.emit("update-available", { version: "0.3.0" });
    });
    const updates = page.getByRole("complementary", { name: "App updates" });
    await expect(updates).toContainText("eyeris 0.3.0 is available");
    await updates.getByRole("button", { name: "Download update" }).click();
    await expect(updates).toContainText("ready to install");
    await page.screenshot({
      path: "test-results/update-ready.png",
      fullPage: true,
    });
    await updates.getByRole("button", { name: "Restart and install" }).click();
    await expect(updates).toContainText("Restarting to install");
    expect(
      await app.evaluate(async ({ app }) => {
        const { createRequire } = process.getBuiltinModule("module");
        return createRequire(app.getAppPath() + "/package.json")(
          "electron-updater",
        ).autoUpdater.testInstallRequested;
      }),
    ).toBe(true);
  } finally {
    await app.close();
    await rm(directory, { recursive: true, force: true });
  }
});
