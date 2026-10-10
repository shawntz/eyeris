import { ensureRuntime } from "./runtime-check.mjs";
import { resolveRscript } from "./rscript.mjs";
import { app, BrowserWindow, ipcMain, dialog, Menu, shell } from "electron";
import { spawn } from "node:child_process";
import { mkdir, readFile, writeFile, access } from "node:fs/promises";
import { userInfo } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { rDirectory, rEnvironment } from "./runtime.mjs";
import { RWorker } from "./r-worker.mjs";
import { Project } from "./project.mjs";
import { Pipeline } from "./pipeline.mjs";
import electronUpdater from "electron-updater";
import { DesktopUpdates } from "./updates.mjs";
import packageMetadata from "../package.json" with { type: "json" };

const root = fileURLToPath(new URL("../", import.meta.url));
const worker = new RWorker();
let window, project, demoChild, pipeline, updates;
let queue = Promise.resolve();
app.setName("eyeris");
if (process.env.EYERIS_TEST_USER_DATA)
  app.setPath("userData", process.env.EYERIS_TEST_USER_DATA);
const settingsFile = () =>
  path.join(app.getPath("userData"), "review-settings.json");
// App settings are per machine: the recent project and how many subjects to
// process at once.
async function appSettings() {
  try {
    return JSON.parse(await readFile(settingsFile(), "utf8"));
  } catch {
    return {};
  }
}
async function saveAppSettings(change) {
  const settings = { ...(await appSettings()), ...change };
  await mkdir(app.getPath("userData"), { recursive: true });
  await writeFile(settingsFile(), JSON.stringify(settings));
}
const requireProject = () => {
  if (!project) throw new Error("Open a review project first.");
  return project;
};
async function activate(directory, create) {
  await ensureRuntime();
  if (project?.exporting)
    throw new Error("Wait for the export to finish before switching projects.");
  if (pipeline?.busy || pipeline?.importing)
    throw new Error(
      "Wait for processing or importing to finish, or cancel it, before switching projects.",
    );
  const next = await Project.open(directory, worker, create);
  project?.close();
  project = next;
  pipeline = new Pipeline(project);
  try {
    pipeline.setParallel((await appSettings()).parallelJobs ?? "auto");
  } catch {
    // A limit saved on a machine with more cores falls back to automatic.
  }
  await saveAppSettings({ lastProject: directory });
  return project.summary();
}
const methods = {
  updateState: () => updates.snapshot(),
  checkForUpdates: () => updates.check(),
  downloadUpdate: () => updates.download(),
  installUpdate: () => updates.install(),
  async init() {
    let warning = "";
    try {
      await ensureRuntime();
    } catch (error) {
      warning = error.message;
    }
    const recent = (await appSettings()).lastProject ?? null;
    return {
      appVersion: packageMetadata.version,
      project: project?.summary() ?? null,
      reviewer: userInfo().username,
      warning,
      recent,
    };
  },
  async openRecent() {
    const { lastProject } = await appSettings();
    if (!lastProject) throw new Error("There is no recent project.");
    return activate(lastProject, false);
  },
  closeProject() {
    if (project?.exporting)
      throw new Error(
        "Wait for the export to finish before closing the project.",
      );
    if (pipeline?.busy || pipeline?.importing)
      throw new Error(
        "Wait for processing or importing to finish, or cancel it, before closing the project.",
      );
    project?.close();
    project = null;
    pipeline = null;
  },
  projectState() {
    requireProject();
    return { project: project.summary(), pipeline: pipeline.snapshot() };
  },
  addSubject(id) {
    requireProject();
    return pipeline.addSubject(id);
  },
  async addRecording(input) {
    requireProject();
    const result = await dialog.showOpenDialog(window, {
      title: "Add EyeLink recordings, one per run",
      filters: [{ name: "EyeLink ASC", extensions: ["asc"] }],
      properties: ["openFile", "multiSelections"],
    });
    if (result.canceled) return null;
    return pipeline.addRecording(input, result.filePaths);
  },
  async importBids() {
    requireProject();
    const result = await dialog.showOpenDialog(window, {
      title: "Import a BIDS dataset of EyeLink recordings",
      buttonLabel: "Import recordings",
      properties: ["openDirectory"],
    });
    if (result.canceled) return null;
    return pipeline.importBids(result.filePaths[0]);
  },
  cancelImport() {
    requireProject();
    pipeline.cancelImport();
    return pipeline.snapshot();
  },
  startPipeline(ids, settings) {
    requireProject();
    return pipeline.start(ids, settings);
  },
  // One job per group of recordings, such as each subject's runs.
  queuePipeline(groups, settings) {
    requireProject();
    if (!Array.isArray(groups) || !groups.every(Array.isArray))
      throw new Error("Invalid processing request.");
    return pipeline.enqueue(groups, settings);
  },
  async setParallelJobs(value) {
    requireProject();
    pipeline.setParallel(value);
    await saveAppSettings({ parallelJobs: value });
    return pipeline.snapshot();
  },
  saveSettings(settings) {
    requireProject();
    return pipeline.saveSettings(settings);
  },
  cancelPipeline(id) {
    requireProject();
    pipeline.cancel(id);
    return pipeline.snapshot();
  },
  pipelineLog(id) {
    requireProject();
    return pipeline.log(id);
  },
  async showProjectFiles(kind, id) {
    requireProject();
    const target =
      kind === "run" && pipeline.snapshot().jobs.some((j) => j.id === id)
        ? path.join(project.directory, "processing", id)
        : kind === "bids"
          ? path.join(project.directory, "bids")
          : kind === "export" && project.lastExport?.directory
            ? project.lastExport.directory
            : project.directory;
    const error = await shell.openPath(target);
    if (error) throw new Error(error);
  },
  async createProject() {
    const result = await dialog.showSaveDialog(window, {
      title: "Create review project",
      defaultPath: "Untitled.eyeris",
      buttonLabel: "Create project",
    });
    if (result.canceled) return null;
    return activate(result.filePath, true);
  },
  async openProject() {
    const result = await dialog.showOpenDialog(window, {
      title: "Open review project folder",
      properties: ["openDirectory"],
    });
    if (result.canceled) return null;
    return activate(result.filePaths[0], false);
  },
  async demo() {
    await ensureRuntime();
    const directory = path.join(app.getPath("userData"), "Demo.eyeris");
    try {
      await access(path.join(directory, "review.sqlite"));
      return await activate(directory, false);
    } catch {
      /* first demo */
    }
    const demoFile = path.join(
      app.getPath("userData"),
      "sub-demo_task-memory.rds",
    );
    await mkdir(app.getPath("userData"), { recursive: true });
    await new Promise((resolve, reject) => {
      const child = spawn(
        resolveRscript(),
        ["--vanilla", path.join(rDirectory(), "make-demo.R"), demoFile],
        {
          stdio: ["ignore", "pipe", "pipe"],
          env: rEnvironment(),
          windowsHide: true,
        },
      );
      demoChild = child;
      let log = "";
      for (const stream of [child.stdout, child.stderr])
        stream.on("data", (b) => {
          log = (log + b).slice(-5000);
        });
      const timer = setTimeout(() => child.kill(), 300_000);
      child.on("error", (error) => {
        clearTimeout(timer);
        demoChild = null;
        reject(error);
      });
      child.on("exit", (code) => {
        clearTimeout(timer);
        demoChild = null;
        if (code === 0) resolve();
        else
          reject(
            new Error(
              `Could not create the demo. Install R, eyeris, and jsonlite. ${log}`,
            ),
          );
      });
    });
    await activate(directory, true);
    await project.importFile(demoFile);
    return project.summary();
  },
  async importFiles() {
    requireProject();
    const selected = await dialog.showOpenDialog(window, {
      title: "Import epoched eyeris objects",
      filters: [{ name: "Saved R objects", extensions: ["rds"] }],
      properties: ["openFile", "multiSelections"],
    });
    if (selected.canceled) return null;
    const results = [];
    for (const file of selected.filePaths) {
      try {
        results.push({
          file: path.basename(file),
          ...(await project.importFile(file)),
        });
      } catch (e) {
        results.push({ file: path.basename(file), error: e.message });
      }
    }
    return { project: project.summary(), results };
  },
  setAutoExclude: (rule) => requireProject().setAutoExclude(rule),
  list: (filters) => requireProject().list(filters),
  diagnosticGroups: () => requireProject().diagnosticGroups(),
  average: (selection) => requireProject().average(selection),
  nextUnreviewed: (filters, fromId) =>
    requireProject().nextUnreviewed(filters, fromId),
  saveReviewPosition: (position) =>
    requireProject().saveReviewPosition(position),
  trace: (id, stage, range) => requireProject().trace(id, stage, range),
  decide: (input) => {
    const epoch = requireProject().decision(input);
    return { epoch, project: project.summary() };
  },
  undo: () => {
    const epoch = requireProject().undo();
    return { epoch, project: project.summary() };
  },
  async exportData() {
    requireProject();
    const result = await dialog.showOpenDialog(window, {
      title: "Choose an export destination",
      buttonLabel: "Export review",
      properties: ["openDirectory", "createDirectory"],
    });
    if (result.canceled) return null;
    return project.startExport(result.filePaths[0]);
  },
};

app.whenReady().then(async () => {
  updates = new DesktopUpdates({
    updater: electronUpdater.autoUpdater,
    version: packageMetadata.version,
    enabled:
      app.isPackaged &&
      !process.env.EYERIS_TEST_USER_DATA &&
      (process.platform !== "linux" || Boolean(process.env.APPIMAGE)),
    busy: () =>
      Boolean(
        pipeline?.busy ||
        project?.exporting ||
        pipeline?.importing ||
        demoChild ||
        worker.pending.size,
      ),
  });
  if (app.isPackaged) process.env.EYERIS_RESOURCE_DIR = process.resourcesPath;
  if (!app.isPackaged) process.env.EYERIS_RSCRIPT = resolveRscript();
  const pageURL = new URL("../dist/index.html", import.meta.url).href;
  for (const [method, fn] of Object.entries(methods)) {
    ipcMain.handle(`review:${method}`, (event, ...args) => {
      if (
        event.sender !== window.webContents ||
        event.senderFrame !== window.webContents.mainFrame ||
        event.senderFrame.url !== pageURL
      )
        throw new Error("Unknown review window.");
      const result = queue.then(() => {
        if (updates.state.status === "installing" && method !== "updateState")
          throw new Error("The app is restarting to install an update.");
        return fn(...args);
      });
      queue = result.catch(() => {});
      return result;
    });
  }
  Menu.setApplicationMenu(
    Menu.buildFromTemplate([
      ...(process.platform === "darwin" ? [{ role: "appMenu" }] : []),
      { role: "editMenu" },
      { role: "viewMenu" },
      { role: "windowMenu" },
    ]),
  );
  window = new BrowserWindow({
    width: 1440,
    height: 960,
    minWidth: 1080,
    minHeight: 740,
    title: "eyeris",
    ...(process.platform === "darwin"
      ? { titleBarStyle: "hidden", trafficLightPosition: { x: 18, y: 18 } }
      : {}),
    icon: path.join(root, "dist/app-icon.png"),
    backgroundColor: "#f5f5f5",
    webPreferences: {
      preload: path.join(root, "electron/preload.cjs"),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
    },
  });
  // Keep the bundle icon in packaged builds so macOS applies the same native
  // presentation in the Dock as in Finder. A raw PNG override bypasses it.
  if (process.platform === "darwin" && !app.isPackaged)
    app.dock?.setIcon(path.join(root, "dist/app-icon.png"));
  window.webContents.setWindowOpenHandler(() => ({ action: "deny" }));
  window.webContents.on("will-navigate", (event) => event.preventDefault());
  window.webContents.session.setPermissionRequestHandler(
    (_wc, _permission, callback) => callback(false),
  );
  await window.loadFile(path.join(root, "dist/index.html"));
  updates.start();
});
app.on("window-all-closed", () => app.quit());
app.on("before-quit", () => {
  updates?.stop();
  pipeline?.dispose();
  demoChild?.kill();
  worker.close();
  project?.close();
  project = null;
});
