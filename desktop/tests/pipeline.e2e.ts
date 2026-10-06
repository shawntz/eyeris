import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, rm, readFile, readdir } from "node:fs/promises";
import path from "node:path";
import { tmpdir } from "node:os";
import { execFileSync } from "node:child_process";
test("splash → subject → ASC → glassbox and BIDS → epoch review", async () => {
  test.setTimeout(180000);
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
    ).toBeVisible();
    await expect(page.getByAltText("eyeris logo sticker")).toBeVisible();
    await page.screenshot({ path: "test-results/splash.png" });
    const project = path.join(dir, "Pupil study.eyeris");
    await app.evaluate(({ dialog }, filePath) => {
      dialog.showSaveDialog = async () => ({ canceled: false, filePath });
    }, project);
    await page
      .getByRole("button", { name: "New project", exact: true })
      .click();
    await page.getByLabel("New subject ID").fill("001");
    await page.screenshot({ path: "test-results/subject-focus.png" });
    await page.getByRole("button", { name: "Create subject" }).click();
    await page
      .getByRole("textbox", { name: "Task", exact: true })
      .fill("memory");
    const asc = execFileSync(
      resolveRscript(),
      ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
      { encoding: "utf8" },
    ).trim();
    await app.evaluate(({ dialog }, file) => {
      dialog.showOpenDialog = async () => ({
        canceled: false,
        filePaths: [file],
      });
    }, asc);
    await page.getByRole("button", { name: "Add ASC file" }).click();
    await expect(
      page.getByRole("heading", { name: "Glassbox pipeline" }),
    ).toBeVisible();
    await page.getByRole("checkbox", { name: "Extract epochs" }).check();
    await page
      .getByRole("textbox", { name: "Event pattern" })
      .fill("PROBE_START_{trial}");
    await page
      .getByRole("button", { name: "Run pipeline", exact: true })
      .click();
    await expect(
      page.getByText("Processing complete", { exact: true }),
    ).toBeVisible({ timeout: 150000 });
    expect(await readdir(path.join(project, "bids"))).toContain("derivatives");
    const state = await page.evaluate(() => window.eyeris.projectState());
    expect(state.pipeline.jobs[0].status).toBe("completed");
    expect(state.project.counts.total).toBeGreaterThan(0);
    expect(
      JSON.parse(state.pipeline.jobs[0].outputs).some((p: string) =>
        p.endsWith(".html"),
      ),
    ).toBe(true);
    await page
      .locator(".processing-content")
      .evaluate((el) => (el.scrollTop = 0));
    await page.screenshot({ path: "test-results/processing.png" });
    await page
      .getByRole("button", { name: "Review epochs", exact: true })
      .click();
    await expect(
      page.getByRole("button", { name: /Keep epoch/ }),
    ).toBeEnabled();
    await page.screenshot({ path: "test-results/review-real.png" });
    await page.getByRole("button", { name: /Keep epoch/ }).click();
    await expect(page.locator(".epoch-status.keep")).toHaveCount(1);
    expect(errors).toEqual([]);
  } catch (e) {
    const page = await app.firstWindow();
    console.log(await page.locator("body").innerText());
    await page.screenshot({ path: "test-results/pipeline-failure.png" });
    throw e;
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
