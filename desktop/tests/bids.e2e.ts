import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, mkdir, copyFile, rm } from "node:fs/promises";
import path from "node:path";
import { tmpdir } from "node:os";
import { execFileSync } from "node:child_process";
import { captureScreenshot } from "./screenshot.mjs";

test("a BIDS folder adds every subject and run in one step", async () => {
  test.setTimeout(120000);
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-ui-bids-"));
  const env = {
    ...process.env,
    EYERIS_TEST_USER_DATA: path.join(dir, "app-data"),
  };
  delete env.ELECTRON_RUN_AS_NODE;
  const asc = execFileSync(
    resolveRscript(),
    ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
    { encoding: "utf8" },
  ).trim();
  const root = path.join(dir, "dataset");
  for (const file of [
    "sub-001/ses-01/eye/sub-001_ses-01_task-memory_run-01_eye.asc",
    "sub-001/ses-01/eye/sub-001_ses-01_task-memory_run-02_eye.asc",
    "sub-002/ses-01/eye/sub-002_ses-01_task-memory_run-01_eye.asc",
  ]) {
    await mkdir(path.dirname(path.join(root, file)), { recursive: true });
    await copyFile(asc, path.join(root, file));
  }
  const app = await electron.launch({ args: ["."], cwd: process.cwd(), env });
  try {
    const page = await app.firstWindow();
    const errors: string[] = [];
    page.on("pageerror", (e) => errors.push(e.message));
    await expect(
      page.getByRole("button", { name: "New project", exact: true }),
    ).toBeEnabled({ timeout: 30_000 });
    await app.evaluate(
      ({ dialog }, filePath) => {
        dialog.showSaveDialog = async () => ({ canceled: false, filePath });
      },
      path.join(dir, "Study.eyeris"),
    );
    await page
      .getByRole("button", { name: "New project", exact: true })
      .click();
    await app.evaluate(({ dialog }, folder) => {
      dialog.showOpenDialog = async () => ({
        canceled: false,
        filePaths: [folder],
      });
    }, root);
    await page.getByRole("button", { name: "Import BIDS folder" }).click();
    await expect(
      page.getByText("Added 3 recordings for 2 subjects."),
    ).toBeVisible({ timeout: 60_000 });
    await expect(
      page
        .locator(".subject-list")
        .getByRole("button", { name: /^sub-00[12]/ }),
    ).toHaveCount(2);
    await expect(page.getByRole("heading", { name: "sub-001" })).toBeVisible();
    await expect(page.locator(".recordings-table tbody tr")).toHaveCount(2);
    await expect(
      page.getByRole("button", { name: "Run pipeline on 2 recordings" }),
    ).toBeVisible();
    await captureScreenshot(page, { path: "test-results/bids-import.png" });
    await page
      .locator(".subject-list")
      .getByRole("button", { name: /^sub-002/ })
      .click();
    await expect(page.locator(".recordings-table tbody tr")).toHaveCount(1);
    await page.getByRole("button", { name: "Dismiss import summary" }).click();
    await expect(page.getByText("Added 3 recordings")).toHaveCount(0);
    expect(errors).toEqual([]);
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
