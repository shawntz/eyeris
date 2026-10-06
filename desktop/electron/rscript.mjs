import { accessSync, constants, readdirSync, statSync } from "node:fs";
import { execFileSync } from "node:child_process";
import path from "node:path";

const unquote = (value) => value?.replace(/^"(.*)"$/, "$1");

// GUI launches do not necessarily inherit a shell PATH. Keep discovery shared
// by the app, packaging scripts and tests; always spawn directly, without a shell.
export function resolveRscript({
  platform = process.platform,
  env = process.env,
  available = (file) => {
    try {
      accessSync(file, constants.X_OK);
      return statSync(file).isFile();
    } catch {
      return false;
    }
  },
  list = readdirSync,
  exec = execFileSync,
} = {}) {
  if (env.EYERIS_RSCRIPT) return unquote(env.EYERIS_RSCRIPT);
  const windows = platform === "win32";
  const paths = windows ? path.win32 : path.posix;
  const executable = windows ? "Rscript.exe" : "Rscript";
  const atHome = (home) => {
    if (!home) return;
    return [
      paths.join(unquote(home), "bin", executable),
      ...(windows ? [paths.join(unquote(home), "bin", "x64", executable)] : []),
    ].find(available);
  };
  const home = atHome(env.R_HOME);
  if (home) return home;
  const pathKey = Object.keys(env).find((key) => key.toUpperCase() === "PATH");
  for (const directory of (env[pathKey] || "").split(windows ? ";" : ":")) {
    if (!directory) continue;
    const candidate = paths.join(unquote(directory), executable);
    if (available(candidate)) return candidate;
  }
  if (windows) {
    // R's installer can register per-user or system-wide installations.
    const reg = paths.join(
      env.SystemRoot || "C:\\Windows",
      "System32",
      "reg.exe",
    );
    for (const hive of ["HKCU", "HKLM"]) {
      for (const key of ["R", "R64"]) {
        try {
          const output = exec(
            reg,
            [
              "query",
              `${hive}\\Software\\R-core\\${key}`,
              "/v",
              "InstallPath",
              "/reg:64",
            ],
            {
              encoding: "utf8",
              windowsHide: true,
              timeout: 2000,
              stdio: ["ignore", "pipe", "ignore"],
            },
          );
          const found = atHome(
            output.match(/InstallPath\s+REG_SZ\s+([^\r\n]+)/i)?.[1].trim(),
          );
          if (found) return found;
        } catch {
          /* Registry entries are optional. */
        }
      }
    }
    for (const base of [
      env.LOCALAPPDATA && paths.join(env.LOCALAPPDATA, "Programs", "R"),
      env.ProgramW6432 && paths.join(env.ProgramW6432, "R"),
      paths.join(env.ProgramFiles || "C:\\Program Files", "R"),
    ].filter(Boolean)) {
      try {
        const versions = list(base)
          .filter((name) => /^R-\d/i.test(name))
          .sort((a, b) => b.localeCompare(a, "en", { numeric: true }));
        for (const version of versions) {
          const found = atHome(paths.join(base, version));
          if (found) return found;
        }
      } catch {
        /* Try the next installation directory. */
      }
    }
  } else {
    const found = (
      platform === "darwin"
        ? [
            "/opt/homebrew/bin/Rscript",
            "/usr/local/bin/Rscript",
            "/Library/Frameworks/R.framework/Resources/bin/Rscript",
          ]
        : ["/usr/local/bin/Rscript", "/usr/bin/Rscript"]
    ).find(available);
    if (found) return found;
  }
  // Let spawn report an actionable missing-runtime error if none was found.
  return executable;
}
