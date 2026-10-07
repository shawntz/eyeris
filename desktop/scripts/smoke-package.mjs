import { rEnvironment } from "../electron/runtime.mjs";
import { resolveRscript } from "../electron/rscript.mjs";
// Verify the installed resource paths and packaged R library, not the dev tree.
import { _electron as electron } from "@playwright/test";
import { mkdtemp, rm, readFile, mkdir, cp } from "node:fs/promises";
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
  EYERIS_RSCRIPT: "/missing-system-R/Rscript",
  R_HOME: "/missing-system-R",
  R_LIBS: "/missing-user-library",
  R_LIBS_USER: "/missing-user-library",
  R_LIBS_SITE: "/missing-site-library",
  RSTUDIO_PANDOC: "/missing-system-pandoc",
  EYERIS_TEST_USER_DATA: path.join(dir, "settings"),
};
delete env.ELECTRON_RUN_AS_NODE;
const app = await electron.launch({ executablePath, env });
try {
  const page = await app.firstWindow();
  await page
    .getByRole("button", { name: "New project", exact: true })
    .waitFor();
  const info = await app.evaluate(({ app }) => ({
    packaged: app.isPackaged,
    resource: process.resourcesPath,
  }));
  assert.equal(info.packaged, true);
  const bundledEnv = rEnvironment({
    env: { ...env, EYERIS_RESOURCE_DIR: info.resource },
  });
  const bundledR = resolveRscript({ env: bundledEnv });
  const ready = await page.evaluate(() => window.eyeris.init());
  assert.equal(ready.warning, "");
  const projectDir = path.join(dir, "Smoke.eyeris");
  await app.evaluate(({ dialog }, filePath) => {
    dialog.showSaveDialog = async () => ({ canceled: false, filePath });
  }, projectDir);
  await page.evaluate(() => window.eyeris.createProject());
  await page.evaluate(() => window.eyeris.addSubject("001"));
  const asc = execFileSync(
    bundledR,
    ["-e", "cat(eyeris::eyelink_asc_demo_dataset())"],
    { encoding: "utf8", env: bundledEnv },
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
        report: true,
        database: true,
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
  const outputs = JSON.parse(done.pipeline.jobs[0].outputs);
  assert.ok(
    outputs.some((file) => file.endsWith(".html")),
    "Bundled Pandoc must generate HTML reports",
  );
  assert.ok(
    outputs.some((file) => file.endsWith(".eyerisdb")),
    "Bundled DuckDB must generate a database",
  );
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
  const run = path.join(projectDir, "processing", done.pipeline.jobs[0].id);
  const recovered = await readFile(
    path.join(run, "recovery.json"),
    "utf8",
  ).catch((error) => {
    if (error.code !== "ENOENT") throw error;
    return null;
  });
  if (recovered) {
    // A successful retry must not erase the evidence needed to diagnose the
    // underlying native-library fault after this temporary project is removed.
    await cp(run, path.resolve("test-results/package-smoke-recovered-crash"), {
      recursive: true,
    });
    console.warn(
      "::warning::The packaged app recovered from a native Windows R crash. The failed attempt and recovery record are retained in test-results.",
    );
  }
  const ping = execFileSync(
    bundledR,
    ["--vanilla", "-e", 'cat(find.package("eyeris"))'],
    {
      encoding: "utf8",
      env: bundledEnv,
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
} catch (error) {
  // Preserve the complete R log and runtime state when native CI fails.
  const artifacts = path.resolve("test-results/package-smoke-failure");
  await mkdir(artifacts, { recursive: true });
  await cp(dir, artifacts, { recursive: true });
  throw error;
} finally {
  await app.close();
  await rm(dir, { recursive: true, force: true });
}
