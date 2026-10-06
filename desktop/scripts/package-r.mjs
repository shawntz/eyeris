import { resolveRscript } from "../electron/rscript.mjs";
import { spawnSync } from "node:child_process";
import { mkdir } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
const root = fileURLToPath(new URL("../", import.meta.url));
const lib = path.join(root, "build/r-library");
await mkdir(lib, { recursive: true });
const result = spawnSync(
  resolveRscript(),
  [
    "--vanilla",
    "-e",
    'args <- commandArgs(TRUE); install.packages(args[1], repos=NULL, type="source", lib=args[2]); stopifnot(as.character(packageVersion("eyeris", lib.loc=args[2])) == read.dcf(file.path(args[1],"DESCRIPTION"))[1,"Version"])',
    path.resolve(root, ".."),
    lib,
  ],
  { stdio: "inherit" },
);
if (result.error)
  console.error(
    `Cannot start R: ${result.error.message}. Set EYERIS_RSCRIPT to Rscript's full path.`,
  );
if (result.status !== 0) process.exit(result.status || 1);
