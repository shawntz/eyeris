import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, mkdir, rm, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";

test("desktop review, stage changes, keyboard decisions, resume, and export", async () => {
  test.setTimeout(120000);
  const root = process.cwd();
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-electron-test-"));
  const projectDir = path.join(dir, "Memory study.eyeris");
  const exportDir = path.join(dir, "exports");
  await mkdir(exportDir);
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  const env = {
    ...process.env,
    EYERIS_TEST_USER_DATA: path.join(dir, "app-data"),
  };
  delete env.ELECTRON_RUN_AS_NODE;
  let app = await electron.launch({ args: ["."], cwd: root, env });
  try {
    let page = await app.firstWindow();
    const errors: string[] = [];
    page.on("pageerror", (e) => errors.push(e.message));
    await expect(
      page.getByRole("button", { name: "New project", exact: true }),
    ).toBeVisible();
    await app.evaluate(({ dialog }, directory) => {
      dialog.showSaveDialog = async () => ({
        canceled: false,
        filePath: directory,
      });
    }, projectDir);
    await page.getByRole("button", { name: "New project" }).click();
    await app.evaluate(
      ({ dialog }, filename) => {
        dialog.showOpenDialog = async () => ({
          canceled: false,
          filePaths: [filename],
        });
      },
      path.join(dir, "sub-001_task-memory.rds"),
    );
    await page.getByRole("button", { name: "Import processed RDS" }).click();
    await expect(
      page.getByRole("button", { name: /Keep epoch/ }),
    ).toBeEnabled();
    await expect(page.locator(".epoch-row")).toHaveCount(3);
    await expect(page.locator("canvas")).toBeVisible();
    await page
      .getByRole("combobox", { name: "Preprocessing stage" })
      .selectOption("pupil_raw");
    await expect(page.locator(".plot-heading")).toContainText("Raw signal");
    await page
      .getByRole("combobox", { name: "Preprocessing stage" })
      .selectOption("final");
    await expect(page.locator(".plot-heading")).toContainText(
      "Low-pass filtered",
    );
    // Zoom and reset are real data requests, not image scaling.
    const canvas = page.locator("canvas");
    const box = (await canvas.boundingBox())!;
    await page.mouse.move(box.x + box.width * 0.35, box.y + box.height * 0.5);
    await page.mouse.down();
    await page.mouse.move(box.x + box.width * 0.65, box.y + box.height * 0.5);
    await page.mouse.up();
    await expect(
      page.getByRole("button", { name: "Reset zoom" }),
    ).toBeEnabled();
    await page.getByRole("button", { name: "Reset zoom" }).click();
    await expect(
      page.getByRole("button", { name: /Keep epoch/ }),
    ).toBeEnabled();
    await page.getByRole("button", { name: /Keep epoch/ }).click();
    await expect(page.locator(".epoch-status.keep")).toHaveCount(1);
    await expect(
      page.getByRole("button", { name: /Exclude epoch/ }),
    ).toBeEnabled();
    await page.getByLabel("Review note", { exact: true }).fill("A brief spike");
    await page.locator("h1").click();
    await page.keyboard.press("x");
    await expect(page.locator(".epoch-status.exclude")).toHaveCount(1);
    await page.keyboard.press("u");
    await expect(page.locator(".epoch-status.exclude")).toHaveCount(0);
    await page.getByLabel("Review note", { exact: true }).fill("A brief spike");
    await page.getByRole("button", { name: /Exclude epoch/ }).click();
    await expect(page.locator(".epoch-status.exclude")).toHaveCount(1);
    await app.evaluate(({ dialog }, directory) => {
      dialog.showOpenDialog = async () => ({
        canceled: false,
        filePaths: [directory],
      });
    }, exportDir);
    await page.getByRole("button", { name: "Export review" }).click();
    await expect(page.getByRole("status")).toContainText(
      "Exported 1 kept, 1 excluded, and 1 unreviewed",
    );
    const state = await page.evaluate(() => window.eyeris.init());
    expect(state.project?.counts).toEqual({
      total: 3,
      keep: 1,
      exclude: 1,
      unreviewed: 1,
    });
    await page.getByRole("button", { name: "Dismiss notification" }).click();
    const keepBox = await page
      .getByRole("button", { name: /Keep epoch/ })
      .boundingBox();
    expect(keepBox!.y + keepBox!.height).toBeLessThan(
      await page.evaluate(() => window.innerHeight),
    );
    await page.screenshot({
      path: path.join(root, "test-results/review-desktop.png"),
      fullPage: true,
    });
    expect(errors).toEqual([]);
    await app.close();
    app = await electron.launch({ args: ["."], cwd: root, env });
    page = await app.firstWindow();
    await page.getByRole("button", { name: /Memory study/ }).click();
    await page.getByRole("button", { name: /Epoch review/ }).click();
    await expect(page.locator(".epoch-status.keep")).toHaveCount(1);
    await expect(page.locator(".epoch-status.exclude")).toHaveCount(1);
    await page.getByRole("button", { name: "To review", exact: true }).click();
    await expect(page.locator(".epoch-row")).toHaveCount(1);
    const report = await page.evaluate(() =>
      window.eyeris.list({
        status: "exclude",
        participant: "",
        search: "",
        stage: "final",
        sort: "natural",
        offset: 0,
      }),
    );
    expect(report.rows[0].reason).toContain("A brief spike");
    // Navigation must cross page boundaries without skipping epochs, including
    // a filtered queue that shrinks after each decision.
    await app.evaluate(
      ({ dialog }, filename) => {
        dialog.showOpenDialog = async () => ({
          canceled: false,
          filePaths: [filename],
        });
      },
      path.join(dir, "sub-large.rds"),
    );
    await page.getByRole("button", { name: "Import processed RDS" }).click();
    await expect(page.getByRole("status")).toContainText(
      "10,001 epochs imported",
      { timeout: 30000 },
    );
    await page
      .getByRole("combobox", { name: "Participant filter" })
      .selectOption("large");
    await expect(page.locator(".epoch-row")).toHaveCount(80);
    await page.getByRole("button", { name: "Next page", exact: true }).click();
    await expect(page.locator(".signal-heading h2")).toContainText("Trial 81");
    await page
      .getByRole("button", { name: "Previous epoch", exact: true })
      .click();
    await expect(page.locator(".signal-heading h2")).toContainText("Trial 80");
    await page.getByRole("button", { name: /Keep epoch/ }).click();
    await expect(page.locator(".signal-heading h2")).toContainText("Trial 81");
    await page.getByRole("button", { name: "Next epoch", exact: true }).click();
    await expect(page.locator(".signal-heading h2")).toContainText("Trial 82");
    await app.evaluate(({ BrowserWindow }) => {
      BrowserWindow.getAllWindows()[0].setContentSize(1080, 740);
    });
    await expect.poll(() => page.evaluate(() => window.innerWidth)).toBe(1080);
    const smallButton = await page
      .getByRole("button", { name: /Keep epoch/ })
      .boundingBox();
    expect(smallButton!.y + smallButton!.height).toBeLessThan(
      await page.evaluate(() => window.innerHeight),
    );
  } catch (error) {
    const page = await app.firstWindow();
    await page.screenshot({
      path: path.join(root, "test-results/review-failure.png"),
      fullPage: true,
    });
    console.log(await page.locator("body").innerText());
    console.log(
      "Focus:",
      await page.evaluate(() => document.activeElement?.tagName),
    );
    throw error;
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
