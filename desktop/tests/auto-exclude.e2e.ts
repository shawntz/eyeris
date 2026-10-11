import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { captureScreenshot } from "./screenshot.mjs";

test("epochs missing too much data are excluded automatically with the reason", async () => {
  test.setTimeout(120000);
  const root = process.cwd();
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-ui-auto-"));
  execFileSync(resolveRscript(), [path.join(root, "tests/fixture.R"), dir]);
  const env = {
    ...process.env,
    EYERIS_TEST_USER_DATA: path.join(dir, "app-data"),
  };
  delete env.ELECTRON_RUN_AS_NODE;
  const app = await electron.launch({ args: ["."], cwd: root, env });
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
      path.join(dir, "Auto.eyeris"),
    );
    await page.getByRole("button", { name: "New project" }).click();
    await app.evaluate(
      ({ dialog }, file) => {
        dialog.showOpenDialog = async () => ({
          canceled: false,
          filePaths: [file],
        });
      },
      path.join(dir, "sub-005_task-memory.rds"),
    );
    await page.getByRole("button", { name: "Import processed RDS" }).click();
    await expect(page.locator(".epoch-row")).toHaveCount(3);
    await page.getByText(/Automatic exclusion · off/).click();
    await page
      .getByRole("checkbox", {
        name: "Automatically exclude epochs with missing data",
      })
      .check();
    await page
      .getByRole("combobox", { name: "Missing data stage" })
      .selectOption("pupil_raw");
    await page
      .getByRole("spinbutton", { name: "Missing data threshold" })
      .fill("25");
    await page
      .getByRole("spinbutton", { name: "Missing data threshold" })
      .press("Enter");
    await expect(
      page.getByText(/2 epochs excluded automatically/),
    ).toBeVisible();
    // 30.8% and 60% of the raw samples are missing; 0.8% is kept.
    await expect(
      page.locator(".epoch-row").filter({ hasText: "auto" }),
    ).toHaveCount(2);
    await expect(page.locator(".epoch-status.exclude")).toHaveCount(2);
    await page
      .locator(".epoch-row")
      .filter({ hasText: "auto" })
      .first()
      .click();
    await expect(
      page.getByRole("combobox", { name: "Exclusion reason" }),
    ).toHaveValue("Excessive missing data");
    await expect(
      page.getByRole("textbox", { name: "Review note" }),
    ).toHaveValue(
      "30.8% of samples missing in pupil_raw, above the 25% automatic exclusion threshold",
    );
    await captureScreenshot(page, { path: "test-results/auto-exclude.png" });
    // Keeping it is a reviewer's decision, which the rule never changes.
    await page.getByRole("button", { name: /Keep epoch/ }).click();
    await expect(page.locator(".epoch-status.keep")).toHaveCount(1);
    await expect(
      page.getByText(/1 epoch excluded automatically/),
    ).toBeVisible();
    // Missing data at either end: the kept epoch's raw signal starts with a
    // gap too, but a reviewer's decision still stands.
    await page
      .getByRole("checkbox", {
        name: "Automatically exclude epochs missing data at the start or end",
      })
      .check();
    await expect(
      page.getByText("The first or last sample missing", { exact: false }),
    ).toBeVisible();
    await expect(
      page.getByText(/1 epoch excluded automatically/),
    ).toBeVisible();
    const edge = page.getByRole("spinbutton", { name: "Edge window" });
    await edge.fill("900");
    await edge.press("Enter");
    // Within 900 ms of the start, the epoch with only a mid-epoch gap (from
    // 833 ms) is excluded as well.
    await expect(
      page.getByText(/2 epochs excluded automatically/),
    ).toBeVisible();
    await expect(page.locator(".auto-exclude-panel > summary")).toHaveText(
      "Automatic exclusion · more than 25% missing or missing data within 900 ms of either end in pupil_raw · 2 excluded",
    );
    // The epoch only the new rule excludes: the second REPEATED_EVENT.
    await page
      .locator(".epoch-row")
      .filter({ hasText: "auto" })
      .filter({ hasText: "REPEATED_EVENT" })
      .click();
    await expect(page.locator(".signal-heading p")).toContainText(
      "REPEATED_EVENT",
    );
    await expect(
      page.getByRole("combobox", { name: "Exclusion reason" }),
    ).toHaveValue("Missing data at the start or end");
    await expect(
      page.getByRole("textbox", { name: "Review note" }),
    ).toHaveValue(
      "pupil_raw is missing data within 900 ms of the start of the epoch",
    );
    await captureScreenshot(page, {
      path: "test-results/auto-exclude-edges.png",
    });
    expect(errors).toEqual([]);
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
