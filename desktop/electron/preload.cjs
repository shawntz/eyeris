const { contextBridge, ipcRenderer } = require("electron");
window.addEventListener("DOMContentLoaded", () => {
  document.documentElement.dataset.platform = process.platform;
});
contextBridge.exposeInMainWorld(
  "eyeris",
  Object.fromEntries(
    [
      "init",
      "updateState",
      "checkForUpdates",
      "downloadUpdate",
      "installUpdate",
      "createProject",
      "openProject",
      "demo",
      "importFiles",
      "list",
      "trace",
      "decide",
      "undo",
      "exportData",
      "openRecent",
      "closeProject",
      "projectState",
      "addSubject",
      "addRecording",
      "startPipeline",
      "cancelPipeline",
      "pipelineLog",
      "showProjectFiles",
    ].map((method) => [
      method,
      (...args) => ipcRenderer.invoke(`review:${method}`, ...args),
    ]),
  ),
);
