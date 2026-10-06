import { resolveRscript } from "./rscript.mjs";
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import path from "node:path";
import { rDirectory, rEnvironment } from "./runtime.mjs";

export class RWorker {
  constructor() {
    this.pending = new Map();
    this.counter = 0;
    this.logs = "";
  }
  start() {
    if (this.child) return;
    const child = spawn(
      resolveRscript(),
      ["--vanilla", path.join(rDirectory(), "worker.R")],
      {
        stdio: ["pipe", "pipe", "pipe"],
        env: rEnvironment(),
        windowsHide: true,
      },
    );
    this.child = child;
    const fail = (error) => {
      if (this.child !== child) return;
      this.child = null;
      for (const { reject, timer } of this.pending.values()) {
        clearTimeout(timer);
        reject(error);
      }
      this.pending.clear();
    };
    child.on("error", (e) =>
      fail(
        new Error(
          `Cannot start R. Install R and jsonlite, or set EYERIS_RSCRIPT to Rscript's full path. ${e.message}`,
        ),
      ),
    );
    child.on("exit", (code) =>
      fail(new Error(`R worker exited (${code}). ${this.logs.slice(-1500)}`)),
    );
    child.stderr.on("data", (chunk) => {
      this.logs = (this.logs + chunk).slice(-4000);
    });
    child.stdin.on("error", (e) => fail(e));
    createInterface({ input: child.stdout }).on("line", (line) => {
      try {
        const message = JSON.parse(line);
        const task = this.pending.get(message.id);
        if (!task) return;
        this.pending.delete(message.id);
        clearTimeout(task.timer);
        if (message.error) task.reject(new Error(message.error));
        else task.resolve(message.result);
      } catch {
        fail(new Error("R returned an invalid response."));
        child.kill();
      }
    });
  }
  request(method, params = {}) {
    this.start();
    const id = ++this.counter;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.child?.kill();
      }, 300_000);
      this.pending.set(id, { resolve, reject, timer });
      this.child.stdin.write(JSON.stringify({ id, method, params }) + "\n");
    });
  }
  close() {
    this.child?.kill();
  }
}
