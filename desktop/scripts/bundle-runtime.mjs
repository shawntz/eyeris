import { execFileSync } from "node:child_process";
import {
  cp,
  chmod,
  mkdir,
  rm,
  readFile,
  writeFile,
  realpath,
} from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { resolveRscript } from "../electron/rscript.mjs";
import { rEnvironment } from "../electron/runtime.mjs";
import {
  files,
  relocateNative,
  collectNativeNotices,
} from "./native-runtime.mjs";

const root = fileURLToPath(new URL("../", import.meta.url));
const resources = path.join(root, "build");
const runtime = path.join(resources, "runtime");
const home = path.join(runtime, "R");
const library = path.join(resources, "r-library");
const lockFile = path.join(root, "runtime-lock.json");
const lock = JSON.parse(await readFile(lockFile, "utf8"));
const hostR = resolveRscript();
const runR = (args, options = {}) =>
  execFileSync(hostR, ["--vanilla", ...args], { encoding: "utf8", ...options });
const info = JSON.parse(
  runR([
    "-e",
    "cat(jsonlite::toJSON(list(home=R.home(),version=as.character(getRversion()),arch=R.version$arch),auto_unbox=TRUE))",
  ]),
);
if (info.version !== lock.R)
  throw new Error(
    `Build requires R ${lock.R}; found ${info.version}. Use the pinned CI toolchain.`,
  );
const expectedArch = process.arch === "arm64" ? "aarch64" : "x86_64";
if (info.arch !== expectedArch)
  throw new Error(
    `R architecture ${info.arch} does not match Electron ${process.arch}`,
  );
if (process.env.EYERIS_BUILD_R_HOME)
  info.home = path.resolve(process.env.EYERIS_BUILD_R_HOME);
const pandoc = process.env.EYERIS_BUILD_PANDOC || "pandoc";
const pandocInfo = execFileSync(pandoc, ["--version"], { encoding: "utf8" });
if (pandocInfo.split(/\r?\n/)[0] !== `pandoc ${lock.pandoc}`)
  throw new Error(`Build requires Pandoc ${lock.pandoc}`);
const pandocSource =
  runR(["-e", "cat(Sys.which(commandArgs(TRUE)[1]))", pandoc]).trim() || pandoc;
console.log(
  `Bundling R ${lock.R}, Pandoc ${lock.pandoc}, and ${Object.keys(lock.packages).length} pinned R packages`,
);
await rm(runtime, { recursive: true, force: true });
await rm(library, { recursive: true, force: true });
await mkdir(runtime, { recursive: true });
await cp(info.home, home, {
  recursive: true,
  dereference: true,
  filter: (source) => {
    const relative = path.relative(info.home, source).split(path.sep).join("/");
    if (relative === "site-library" || source.endsWith(".dSYM")) return false;
    // The app uses offscreen/native graphics, never X11, Tcl/Tk, or Java.
    // Excluding these optional R GUI modules avoids an XQuartz prerequisite.
    return (
      process.platform !== "darwin" ||
      ![
        "fontconfig",
        "library/tcltk",
        "modules/R_X11.so",
        "modules/R_de.so",
        "library/grDevices/libs/cairo.so",
      ].includes(relative)
    );
  },
});
await mkdir(path.join(runtime, "pandoc"), { recursive: true });
const pandocTarget = path.join(
  runtime,
  "pandoc",
  process.platform === "win32" ? "pandoc.exe" : "pandoc",
);
await cp(await realpath(pandocSource), pandocTarget);
const origins = new Map([[pandocTarget, await realpath(pandocSource)]]);
for (const file of await files(home))
  origins.set(file, path.join(info.home, path.relative(home, file)));
if (process.platform !== "win32") {
  // R's Unix launcher normally resets R_HOME to its build-time installation.
  // Derive it from this script so moving/installing the app remains safe.
  const launcher = path.join(home, "bin", "R");
  let content = await readFile(launcher, "utf8");
  const marker = content.indexOf(
    "# Since this script can be called recursively",
  );
  if (marker < 0)
    throw new Error(
      "Unrecognized R launcher; refusing to ship an unrelocatable runtime",
    );
  content =
    '#!/bin/sh\nR_HOME_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"\nR_HOME="$R_HOME_DIR"\nR_SHARE_DIR="$R_HOME/share"\nR_INCLUDE_DIR="$R_HOME/include"\nR_DOC_DIR="$R_HOME/doc"\nexport R_HOME R_SHARE_DIR R_INCLUDE_DIR R_DOC_DIR\n' +
    content.slice(marker);
  await writeFile(launcher, content);
  const ldpaths = path.join(home, "etc", "ldpaths");
  await chmod(ldpaths, 0o644);
  const variable =
    process.platform === "darwin"
      ? "DYLD_FALLBACK_LIBRARY_PATH"
      : "LD_LIBRARY_PATH";
  await writeFile(
    ldpaths,
    `${variable}="\${R_HOME}/lib:\${R_HOME}/../native"\nexport ${variable}\n`,
  );
}
// Install the locked dependency closure into its own library, never into a
// developer's R installation. Binary dependencies are relocated before loading.
runR([path.join(root, "scripts", "prepare-library.R"), lockFile, library], {
  stdio: "inherit",
});
const native = await relocateNative(
  [home, library, path.join(runtime, "pandoc")],
  runtime,
  origins,
);
const bundledR = path.join(
  home,
  "bin",
  process.platform === "win32" ? "Rscript.exe" : "Rscript",
);
const env = rEnvironment({
  env: { ...process.env, EYERIS_RESOURCE_DIR: resources },
});
execFileSync(
  bundledR,
  [
    "--vanilla",
    "-e",
    'args<-commandArgs(TRUE); install.packages(args[1],repos=NULL,type="source",lib=args[2]); stopifnot(as.character(packageVersion("eyeris",lib.loc=args[2])) == read.dcf(file.path(args[1],"DESCRIPTION"))[1,"Version"])',
    path.resolve(root, ".."),
    library,
  ],
  { stdio: "inherit", env },
);
await cp(
  path.join(home, "share", "licenses", "GPL-2"),
  path.join(runtime, "pandoc", "COPYING"),
);
await collectNativeNotices(native, runtime);
const manifest = {
  ...lock,
  platform: process.platform,
  arch: process.arch,
  eyeris: (await readFile(path.resolve(root, "../DESCRIPTION"), "utf8")).match(
    /^Version: (.+)$/m,
  )[1],
  native,
};
await writeFile(
  path.join(runtime, "manifest.json"),
  JSON.stringify(manifest, null, 2) + "\n",
);
await writeFile(
  path.join(runtime, "THIRD-PARTY-NOTICES.txt"),
  `This application includes R ${lock.R} (GPL-2/GPL-3), Pandoc ${lock.pandoc} (GPL-2-or-later), and the R packages listed in manifest.json.\nR license: R/COPYING; R sources: https://cran.r-project.org/src/base/R-4/R-${lock.R}.tar.gz\nPandoc license and source: https://github.com/jgm/pandoc/tree/${lock.pandoc}\nR package licenses and copyright notices are retained in each r-library/<package>/DESCRIPTION and LICENSE/COPYING file where provided.\nCorresponding package sources are available from ${lock.repository}/src/contrib/<Package>_<Version>.tar.gz using manifest.json.\nNative dependencies and their build origins are recorded in manifest.json.\n`,
);
execFileSync(bundledR, ["--vanilla", path.join(root, "r", "check-runtime.R")], {
  stdio: "inherit",
  env,
});
console.log("Private runtime prepared and validated.");
