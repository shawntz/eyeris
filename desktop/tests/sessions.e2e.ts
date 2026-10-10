import { resolveRscript } from "../electron/rscript.mjs";
import { test, expect, _electron as electron } from "@playwright/test";
import { access, copyFile, mkdir, mkdtemp, rm } from "node:fs/promises";
import path from "node:path";
import { tmpdir } from "node:os";
import { execFileSync } from "node:child_process";
import { captureScreenshot } from "./screenshot.mjs";

test("a session is removed from a project, leaving the others", async () => {
  test.setTimeout(120000);
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-ui-sessions-"));
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
  // sub-002 has only the retrieval session.
  for (const file of [
    "sub-001/ses-enc/eye/sub-001_ses-enc_task-clamp_run-01_eyetrack.asc",
    "sub-001/ses-ret/eye/sub-001_ses-ret_task-clamp_run-01_eyetrack.asc",
    "sub-002/ses-ret/eye/sub-002_ses-ret_task-clamp_run-01_eyetrack.asc",
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
    const project = path.join(dir, "Study.eyeris");
    await app.evaluate(
      ({ dialog }, paths) => {
        dialog.showSaveDialog = async () => ({
          canceled: false,
          filePath: paths.project,
        });
        dialog.showOpenDialog = async () => ({
          canceled: false,
          filePaths: [paths.root],
        });
      },
      { project, root },
    );
    await page
      .getByRole("button", { name: "New project", exact: true })
      .click();
    // Both sessions are imported into one project by mistake.
    await page.getByRole("button", { name: "Import BIDS folder" }).click();
    const choice = page.getByRole("region", { name: "Choose what to import" });
    await choice.getByRole("checkbox", { name: /ses-enc/ }).check();
    await choice.getByRole("checkbox", { name: /ses-ret/ }).check();
    await choice.getByRole("button", { name: "Import 3 files" }).click();
    await expect(
      page.getByText("Added 3 recordings for 2 subjects."),
    ).toBeVisible({ timeout: 60_000 });
    await page.getByRole("button", { name: "Process all subjects" }).click();
    const sessions = page.locator(".project-sessions");
    await expect(sessions.locator(".session-row")).toHaveText([
      /ses-enc1 subject · 1 recording · task-clamp/,
      /ses-ret2 subjects · 2 recordings · task-clamp/,
    ]);
    await sessions.getByRole("button", { name: "Remove ses-ret…" }).click();
    const confirm = page.getByRole("alertdialog", { name: "Remove ses-ret" });
    await expect(confirm).toContainText(
      "2 recordings of 2 subjects, with their copies in the project",
    );
    await expect(confirm).toContainText("sub-002, with no other recordings");
    await captureScreenshot(page, { path: "test-results/remove-session.png" });
    // Cancelling changes nothing.
    await confirm.getByRole("button", { name: "Cancel" }).click();
    await expect(confirm).toHaveCount(0);
    await sessions.getByRole("button", { name: "Remove ses-ret…" }).click();
    await confirm.getByRole("button", { name: "Remove ses-ret" }).click();
    await expect(sessions.getByRole("status")).toHaveText(
      "Removed ses-ret: 2 recordings and 1 subject without other recordings.",
    );
    await expect(sessions.locator(".session-row")).toHaveCount(0);
    await expect(
      page.locator(".subject-list").getByRole("button", { name: /^sub-/ }),
    ).toHaveText([/^sub-001/]);
    const state = await page.evaluate(() => window.eyeris.projectState());
    expect(
      state.pipeline.recordings.map((r: { session: string }) => r.session),
    ).toEqual(["enc"]);
    await expect(
      access(path.join(project, "sourcedata", "sub-001", "ses-ret")),
    ).rejects.toThrow();
    // The original files are untouched, so the session can go to a new project.
    await access(
      path.join(
        root,
        "sub-002/ses-ret/eye/sub-002_ses-ret_task-clamp_run-01_eyetrack.asc",
      ),
    );
    await sessions
      .getByRole("button", { name: "Dismiss removal summary" })
      .click();
    await expect(sessions).toHaveCount(0);
    expect(errors).toEqual([]);
  } finally {
    await app.close();
    await rm(dir, { recursive: true, force: true });
  }
});
