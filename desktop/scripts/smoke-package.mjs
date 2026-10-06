import { resolveRscript } from "../electron/rscript.mjs";
// Verify the installed resource paths and packaged R library, not the dev tree.
import { _electron as electron } from "@playwright/test";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
const unpacked =
  process.platform === "darwin"
    ? `mac${process.arch === "arm64" ? "-arm64" : ""}/eyeris.app/Contents/MacOS/eyeris`
    : process.platform === "win32"
      ? "win-unpacked/eyeris.exe"
      : "linux-unpacked/eyeris";
const executablePath = path.resolve(
  process.argv[2] || path.join("release", unpacked),
);
const dir = await mkdtemp(path.join(tmpdir(), "eyeris package smoke-"));
const env = {
  ...process.env,
  EYERIS_TEST_USER_DATA: path.join(dir, "settings"),
};
delete env.ELECTRON_RUN_AS_NODE;
const app = await electron.launch({ executablePath, env });
try {
  const page = await app.firstWindow();
  await page
    .getByRole("button", { name: "New project", exact: true })
    .waitFor();
  const projectDir = path.join(dir, "Smoke.eyeris");
  await app.evaluate(({ dialog }, filePath) => {
    dialog.showSaveDialog = async () => ({ canceled: false, filePath });
  }, projectDir);
  await page.evaluate(() => window.eyeris.createProject());
  await page.evaluate(() => window.eyeris.addSubject("001"));
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
  const state = await page.evaluate(() =>
    window.eyeris.addRecording({
      subject: "001",
      session: "01",
      task: "smoke",
    }),
  );
  await page.evaluate(
    (id) =>
      window.eyeris.startPipeline(id, {
        glassbox: { lpfilt: { plot_freqz: false } },
        epoch: {
          events: "PROBE_START_{trial}",
          limits: [-1, 2],
          label: "probe",
          baseline: false,
        },
        report: false,
        database: false,
      }),
    state.recordings[0].id,
  );
  let done;
  const deadline = Date.now() + 120000;
  do {
    done = await page.evaluate(() => window.eyeris.projectState());
    if (
      ["completed", "failed", "cancelled"].includes(
        done.pipeline.jobs[0]?.status,
      )
    )
      break;
    await new Promise((resolve) => setTimeout(resolve, 250));
  } while (Date.now() < deadline);
  assert.equal(
    done.pipeline.jobs[0].status,
    "completed",
    done.pipeline.jobs[0].error || JSON.stringify(done.pipeline),
  );
  assert.ok(done.project.counts.total > 0);
  const runtime = JSON.parse(
    await readFile(
      path.join(
        projectDir,
        "processing",
        done.pipeline.jobs[0].id,
        "runtime.json",
      ),
      "utf8",
    ),
  );
  assert.equal(runtime.eyeris, "3.3.0");
  const info = await app.evaluate(({ app }) => ({
    packaged: app.isPackaged,
    resource: process.resourcesPath,
  }));
  assert.equal(info.packaged, true);
  const ping = execFileSync(
    resolveRscript(),
    ["--vanilla", "-e", 'cat(find.package("eyeris"))'],
    {
      encoding: "utf8",
      env: { ...process.env, R_LIBS: path.join(info.resource, "r-library") },
    },
  ).trim();
  assert.equal(
    path.normalize(ping),
    path.join(info.resource, "r-library", "eyeris"),
  );
  const demo = await page.evaluate(() => window.eyeris.demo());
  assert.ok(
    demo.counts.total > 0,
    "Packaged demo must load the bundled R scripts and package",
  );
  console.log(
    "Packaged app launched, loaded its eyeris library, processed ASC, wrote BIDS, indexed epochs, and opened the demo.",
  );
} finally {
  await app.close();
  await rm(dir, { recursive: true, force: true });
}
