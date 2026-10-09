import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, rm, copyFile, readdir } from "node:fs/promises";
import path from "node:path";
import { tmpdir } from "node:os";
import { execFileSync } from "node:child_process";
import { captureScreenshot } from "./screenshot.mjs";
test("splash → subject → ASC runs → glassbox and BIDS → epoch review", async () => {
  test.setTimeout(300000);
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-ui-pipeline-"));
  const env = {
    ...process.env,
    EYERIS_TEST_USER_DATA: path.join(dir, "app-data"),
  };
  delete env.ELECTRON_RUN_AS_NODE;
  const app = await electron.launch({ args: ["."], cwd: process.cwd(), env });
  try {
    const page = await app.firstWindow();
    const errors: string[] = [];
    page.on("pageerror", (e) => errors.push(e.message));
    await expect(
      page.getByRole("button", { name: "New project", exact: true }),
    ).toBeEnabled({ timeout: 30_000 });
    await expect(page.getByAltText("eyeris logo sticker")).toBeVisible();
    await captureScreenshot(page, { path: "test-results/splash.png" });
    const project = path.join(dir, "Pupil study.eyeris");
    await app.evaluate(({ dialog }, filePath) => {
      dialog.showSaveDialog = async () => ({ canceled: false, filePath });
    }, project);
    await page
      .getByRole("button", { name: "New project", exact: true })
      .click();
    await page.getByLabel("New subject ID").fill("001");
    await captureScreenshot(page, { path: "test-results/subject-focus.png" });
    await page.getByRole("button", { name: "Create subject" }).click();
    await page
      .getByRole("textbox", { name: "Task", exact: true })
      .fill("memory");
    const asc = execFileSync(
      resolveRscript(),
      ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
      { encoding: "utf8" },
    ).trim();
    // Two runs of the same session and task, one ASC each.
    const runs = [path.join(dir, "run2.asc"), path.join(dir, "run1.asc")];
    for (const file of runs) await copyFile(asc, file);
    await app.evaluate(({ dialog }, filePaths) => {
      dialog.showOpenDialog = async () => ({ canceled: false, filePaths });
    }, runs);
    await page.getByRole("button", { name: "Add ASC files" }).click();
    await expect(
      page.getByRole("heading", { name: "Glassbox pipeline" }),
    ).toBeVisible();
    await expect(page.locator(".recordings-table tbody tr")).toHaveCount(2);
    await expect(
      page.getByRole("checkbox", {
        name: "Process sub-001_ses-01_task-memory_run-02",
      }),
    ).toBeChecked();
    await page.getByRole("checkbox", { name: "Extract epochs" }).check();
    await page
      .getByRole("textbox", { name: "Event pattern" })
      .fill("PROBE_START_{trial}");
    await page
      .getByRole("button", { name: "Run pipeline on 2 recordings" })
      .click();
    await expect(
      page.getByText("Processing complete", { exact: true }),
    ).toBeVisible({ timeout: 270000 });
    expect(await readdir(path.join(project, "bids"))).toContain("derivatives");
    const state = await page.evaluate(() => window.eyeris.projectState());
    expect(state.pipeline.jobs[0].status).toBe("completed");
    expect(state.pipeline.jobs[0].recordings).toHaveLength(2);
    expect(state.project.runs).toEqual(["01", "02"]);
    expect(state.project.counts.total).toBeGreaterThan(0);
    expect(
      JSON.parse(state.pipeline.jobs[0].outputs).some((p: string) =>
        p.endsWith(".html"),
      ),
    ).toBe(true);
    await page
      .locator(".processing-content")
      .evaluate((el) => (el.scrollTop = 0));
    await captureScreenshot(page, { path: "test-results/processing.png" });
    await page
      .getByRole("button", { name: "Review epochs", exact: true })
      .click();
    await expect(
      page.getByRole("button", { name: /Keep epoch/ }),
    ).toBeEnabled();
    // Both runs are reviewed together, and the queue can narrow to one.
    await expect(
      page.getByRole("combobox", { name: "Participant filter" }),
    ).toHaveValue("001");
    await expect(page.locator(".epoch-row").first()).toContainText("Run 01");
    await page.getByRole("combobox", { name: "Run filter" }).selectOption("02");
    await expect(page.locator(".epoch-row").first()).toContainText("Run 02");
    expect(
      await page.locator(".epoch-row").filter({ hasText: "Run 01" }).count(),
    ).toBe(0);
    await captureScreenshot(page, { path: "test-results/review-real.png" });
    await page.getByRole("button", { name: /Keep epoch/ }).click();
    await expect(page.locator(".epoch-status.keep")).toHaveCount(1);
    expect(errors).toEqual([]);
  } catch (e) {
    try {
      const page = await app.firstWindow();
      console.log(await page.locator("body").innerText());
      await captureScreenshot(page, {
        path: "test-results/pipeline-failure.png",
      });
    } catch (diagnosticError) {
      console.warn(
        "Could not capture pipeline failure diagnostics:",
        diagnosticError,
      );
    }
    throw e;
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
