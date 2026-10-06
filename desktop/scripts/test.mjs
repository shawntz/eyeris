import { readdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import path from "node:path";

// cmd.exe does not expand shell globs. Pass explicit filenames to Node.
const root = fileURLToPath(new URL("../", import.meta.url));
const files = readdirSync(path.join(root, "tests"))
  .filter((file) => file.endsWith(".test.mjs"))
  .map((file) => path.join(root, "tests", file));
const result = spawnSync(process.execPath, ["--test", ...files], {
  stdio: "inherit",
});
if (result.error) console.error(result.error.message);
process.exit(result.status ?? 1);
