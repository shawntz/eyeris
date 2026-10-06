import { execFile } from "node:child_process";
import { promisify } from "node:util";
import path from "node:path";
import { resolveRscript } from "./rscript.mjs";
import { rDirectory, rEnvironment } from "./runtime.mjs";
const execute = promisify(execFile);
let check;
export function ensureRuntime() {
  check ||= (async () => {
    try {
      const { stdout } = await execute(
        resolveRscript(),
        ["--vanilla", path.join(rDirectory(), "check-runtime.R")],
        {
          env: rEnvironment(),
          windowsHide: true,
          timeout: 120000,
          maxBuffer: 2 * 1024 * 1024,
        },
      );
      return JSON.parse(stdout.trim().split(/\r?\n/).at(-1));
    } catch (error) {
      const detail = (error.stderr || error.message).trim().slice(-1600);
      throw new Error(
        process.env.EYERIS_RESOURCE_DIR
          ? `The bundled processing tools could not start. Reinstall eyeris or contact support. ${detail}`
          : `R setup is incomplete. Install the eyeris dependencies, pkgload, duckdb, and Pandoc. ${detail}`,
      );
    }
  })();
  return check;
}
