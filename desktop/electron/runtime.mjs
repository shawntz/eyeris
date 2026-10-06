import { fileURLToPath } from "node:url";
import path from "node:path";

export function rDirectory() {
  return process.env.EYERIS_RESOURCE_DIR
    ? path.join(process.env.EYERIS_RESOURCE_DIR, "r")
    : fileURLToPath(new URL("../r/", import.meta.url));
}
export function rEnvironment({
  env = process.env,
  platform = process.platform,
} = {}) {
  const resources = env.EYERIS_RESOURCE_DIR;
  if (!resources)
    return {
      ...env,
      EYERIS_PACKAGE_LIBRARY: "",
      EYERIS_SOURCE: fileURLToPath(new URL("../../", import.meta.url)),
    };
  const paths = platform === "win32" ? path.win32 : path.posix;
  const home = paths.join(resources, "runtime", "R");
  const library = paths.join(resources, "r-library");
  const pandoc = paths.join(resources, "runtime", "pandoc");
  const clean = Object.fromEntries(
    Object.entries(env).filter(
      ([key]) =>
        !/^(R_|RHOME$|RSTUDIO_|DYLD_|LD_|PATH$|EYERIS_SOURCE$|EYERIS_RSCRIPT$)/i.test(
          key,
        ),
    ),
  );
  const system =
    platform === "win32"
      ? [
          paths.join(env.SystemRoot || "C:\\Windows", "System32"),
          env.SystemRoot || "C:\\Windows",
        ]
      : ["/usr/bin", "/bin", "/usr/sbin", "/sbin"];
  return {
    ...clean,
    EYERIS_RESOURCE_DIR: resources,
    EYERIS_PACKAGE_LIBRARY: library,
    EYERIS_SOURCE: "",
    R_HOME: home,
    RHOME: home,
    R_SHARE_DIR: paths.join(home, "share"),
    R_INCLUDE_DIR: paths.join(home, "include"),
    R_DOC_DIR: paths.join(home, "doc"),
    R_LIBS: library,
    R_LIBS_USER: library,
    R_LIBS_SITE: library,
    R_ENVIRON_USER: paths.join(resources, "runtime", "disabled"),
    R_PROFILE_USER: paths.join(resources, "runtime", "disabled"),
    RSTUDIO_PANDOC: pandoc,
    PATH: [
      paths.join(home, "bin"),
      ...(platform === "win32" ? [paths.join(home, "bin", "x64")] : []),
      pandoc,
      ...system,
    ].join(platform === "win32" ? ";" : ":"),
  };
}
