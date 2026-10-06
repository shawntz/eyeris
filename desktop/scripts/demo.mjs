import { spawnSync } from "node:child_process";
import { resolveRscript } from "../electron/rscript.mjs";
import { rDirectory, rEnvironment } from "../electron/runtime.mjs";
import path from "node:path";

const result = spawnSync(
  resolveRscript(),
  [
    "--vanilla",
    path.join(rDirectory(), "make-demo.R"),
    ...process.argv.slice(2),
  ],
  {
    stdio: "inherit",
    env: rEnvironment(),
    windowsHide: true,
  },
);
if (result.error) console.error(result.error.message);
process.exit(result.status ?? 1);
