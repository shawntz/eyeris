import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { mkdtemp, rm } from "node:fs/promises";
import path from "node:path";
import { tmpdir } from "node:os";
import { execFileSync } from "node:child_process";
import { captureScreenshot } from "./screenshot.mjs";

test("eye choices follow the eyes eyeris finds in each recording", async () => {
  test.setTimeout(120000);
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-ui-eyes-"));
  const env = {
    ...process.env,
    EYERIS_TEST_USER_DATA: path.join(dir, "app-data"),
  };
  delete env.ELECTRON_RUN_AS_NODE;
  const files = {
    // The demo recording has only the right eye; the other has both.
    "001": execFileSync(
      resolveRscript(),
      ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
      { encoding: "utf8" },
    ).trim(),
    "002": path.resolve("..", "inst", "extdata", "binocular.asc"),
  };
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
      path.join(dir, "Eyes.eyeris"),
    );
    await page
      .getByRole("button", { name: "New project", exact: true })
      .click();
    for (const [subject, file] of Object.entries(files)) {
      await page.getByLabel("New subject ID").fill(subject);
      await page.getByRole("button", { name: "Create subject" }).click();
      await page
        .getByRole("textbox", { name: "Task", exact: true })
        .fill("memory");
      await app.evaluate(({ dialog }, filePath) => {
        dialog.showOpenDialog = async () => ({
          canceled: false,
          filePaths: [filePath],
        });
      }, file);
      await page.getByRole("button", { name: "Add ASC files" }).click();
      await expect(page.locator(".recordings-table tbody tr")).toHaveCount(1);
    }
    const subject = (id: string) =>
      page
        .locator(".subject-list")
        .getByRole("button", { name: new RegExp(`^sub-${id}`) });
    const eye = page.getByRole("combobox", { name: "Eye" });
    // Binocular: every mode can be chosen.
    await subject("002").click();
    await expect(eye).toBeVisible({ timeout: 60_000 });
    await expect(eye.locator("option")).toHaveText([
      "Average",
      "Left",
      "Right",
      "Both, separately",
    ]);
    // Right eye only: no choice to make, so none is offered.
    await subject("001").click();
    await expect(page.locator("output.eye-fixed")).toHaveText(
      "Right, the only eye recorded",
      { timeout: 60_000 },
    );
    await expect(eye).toHaveCount(0);
    // The recorded eye lines up with the random seed field beside it.
    const box = async (selector: string) =>
      (await page.locator(selector).boundingBox())!;
    const fixed = await box("output.eye-fixed");
    const seed = await box('input[aria-label="Random seed"]');
    expect(Math.round(fixed.y)).toBe(Math.round(seed.y));
    expect(Math.round(fixed.height)).toBe(Math.round(seed.height));
    await expect(
      page.getByRole("button", { name: "Run pipeline" }),
    ).toBeEnabled();
    await captureScreenshot(page, { path: "test-results/eyes-mono.png" });
    // Both together: the choice applies to the binocular recording only.
    await page
      .locator(".subject-list")
      .getByRole("button", { name: /^All subjects/ })
      .click();
    await expect(eye).toBeVisible();
    await expect(
      page.getByText(
        "Applies to 1 binocular recording. 1 recording from one eye uses that eye.",
      ),
    ).toBeVisible();
    expect(errors).toEqual([]);
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
