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

const root = fileURLToPath(new URL("../", import.meta.url));
const worker = new RWorker();
let window, project, demoChild, pipeline;
let queue = Promise.resolve();
app.setName("eyeris");
if (process.env.EYERIS_TEST_USER_DATA)
  app.setPath("userData", process.env.EYERIS_TEST_USER_DATA);
const settingsFile = () =>
  path.join(app.getPath("userData"), "review-settings.json");
const requireProject = () => {
  if (!project) throw new Error("Open a review project first.");
  return project;
};
async function activate(directory, create) {
  if (pipeline?.active)
    throw new Error(
      "Wait for processing to finish or cancel it before switching projects.",
    );
  const next = await Project.open(directory, worker, create);
  project?.close();
  project = next;
  pipeline = new Pipeline(project);
  await mkdir(app.getPath("userData"), { recursive: true });
  await writeFile(settingsFile(), JSON.stringify({ lastProject: directory }));
  return project.summary();
}
const methods = {
  async init() {
    let recent = null;
    try {
      recent = JSON.parse(await readFile(settingsFile(), "utf8")).lastProject;
    } catch {}
    return {
      project: project?.summary() ?? null,
      reviewer: userInfo().username,
      warning: "",
      recent,
    };
  },
  async openRecent() {
    const settings = JSON.parse(await readFile(settingsFile(), "utf8"));
    return activate(settings.lastProject, false);
  },
  closeProject() {
    if (pipeline?.active)
      throw new Error(
        "Wait for processing to finish or cancel it before closing the project.",
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
      title: "Add EyeLink recording",
      filters: [{ name: "EyeLink ASC", extensions: ["asc"] }],
      properties: ["openFile"],
    });
    if (result.canceled) return null;
    return pipeline.addRecording(input, result.filePaths[0]);
  },
  startPipeline(id, settings) {
    requireProject();
    return pipeline.start(id, settings);
  },
  cancelPipeline() {
    requireProject();
    pipeline.cancel();
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
  list: (filters) => requireProject().list(filters),
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
    return project.export(result.filePaths[0]);
  },
};

app.whenReady().then(async () => {
  if (app.isPackaged) process.env.EYERIS_RESOURCE_DIR = process.resourcesPath;
  process.env.EYERIS_RSCRIPT = resolveRscript();
  const pageURL = new URL("../dist/index.html", import.meta.url).href;
  for (const [method, fn] of Object.entries(methods)) {
    ipcMain.handle(`review:${method}`, (event, ...args) => {
      if (
        event.sender !== window.webContents ||
        event.senderFrame !== window.webContents.mainFrame ||
        event.senderFrame.url !== pageURL
      )
        throw new Error("Unknown review window.");
      const result = queue.then(() => fn(...args));
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
});
app.on("window-all-closed", () => app.quit());
app.on("before-quit", () => {
  if (pipeline?.active) {
    project.db
      .prepare(
        "UPDATE jobs SET status='interrupted', phase='interrupted' WHERE id=?",
      )
      .run(pipeline.active.id);
    pipeline.disposed = true;
    pipeline.active.child.kill();
  }
  demoChild?.kill();
  worker.close();
  project?.close();
  project = null;
});
