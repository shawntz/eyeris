// Keep updater policy independent of Electron so failure and restart behavior
// can be tested without downloading or installing software.
export class DesktopUpdates {
  constructor({ updater, version, enabled, busy = () => false }) {
    this.updater = updater;
    this.busy = busy;
    this.state = {
      status: enabled ? "idle" : "unavailable",
      currentVersion: version,
    };
    updater.autoDownload = false;
    updater.autoInstallOnAppQuit = false;
    updater.allowDowngrade = false;
    updater.allowPrerelease = false;
    updater.on("checking-for-update", () => this.set("checking"));
    updater.on("update-available", (info) =>
      this.set("available", { version: info.version }),
    );
    updater.on("update-not-available", () => this.set("current"));
    updater.on("download-progress", (info) =>
      this.set("downloading", {
        version: this.state.version,
        percent: Math.min(100, Math.max(0, Math.round(info.percent))),
      }),
    );
    updater.on("update-downloaded", (info) =>
      this.set("downloaded", { version: info.version }),
    );
    updater.on("error", (error) => this.failure(error));
  }
  set(status, fields = {}) {
    this.state = {
      currentVersion: this.state.currentVersion,
      status,
      ...fields,
    };
  }
  snapshot() {
    return { ...this.state };
  }
  failure(error) {
    this.updater.logger?.warn(error);
    this.set("error", {
      message:
        "Updates are temporarily unavailable. Check your connection and try again.",
    });
  }
  check() {
    if (
      this.pending ||
      ["unavailable", "downloading", "downloaded", "installing"].includes(
        this.state.status,
      )
    )
      return this.snapshot();
    this.set("checking");
    this.pending = Promise.resolve()
      .then(() => this.updater.checkForUpdates())
      .catch((error) => this.failure(error))
      .finally(() => {
        this.pending = null;
      });
    return this.snapshot();
  }
  download() {
    if (this.pending || this.state.status !== "available")
      return this.snapshot();
    this.set("downloading", { version: this.state.version, percent: 0 });
    this.pending = Promise.resolve()
      .then(() => this.updater.downloadUpdate())
      .catch((error) => this.failure(error))
      .finally(() => {
        this.pending = null;
      });
    return this.snapshot();
  }
  install() {
    if (this.state.status !== "downloaded")
      throw new Error("Download an update before installing it.");
    if (this.busy())
      throw new Error(
        "Finish or cancel processing before restarting to install the update.",
      );
    this.set("installing", { version: this.state.version });
    setImmediate(() => {
      try {
        this.updater.quitAndInstall(false, true);
      } catch (error) {
        this.failure(error);
      }
    });
    return this.snapshot();
  }
  start() {
    if (this.state.status === "unavailable") return;
    this.initial = setTimeout(() => this.check(), 30_000);
    this.interval = setInterval(() => this.check(), 6 * 60 * 60 * 1000);
    this.initial.unref?.();
    this.interval.unref?.();
  }
  stop() {
    clearTimeout(this.initial);
    clearInterval(this.interval);
  }
}
