import { test } from "node:test";
import assert from "node:assert/strict";
import { rEnvironment } from "../electron/runtime.mjs";
import { resolveRscript } from "../electron/rscript.mjs";

for (const platform of ["darwin", "linux", "win32"]) {
  test(`packaged ${platform} runtime excludes host R, libraries, and Pandoc`, () => {
    const resources =
      platform === "win32"
        ? "C:\\Program Files\\eyeris\\resources"
        : "/Applications/eyeris.app/Contents/Resources";
    const env = rEnvironment({
      platform,
      env: {
        EYERIS_RESOURCE_DIR: resources,
        R_HOME: "/wrong/R",
        R_LIBS: "/wrong/library",
        R_LIBS_USER: "/wrong/user",
        R_LIBS_SITE: "/wrong/site",
        R_PROFILE: "/wrong/profile",
        RSTUDIO_PANDOC: "/wrong/pandoc",
        DYLD_LIBRARY_PATH: "/wrong/libs",
        LD_PRELOAD: "/wrong/inject.so",
        Path: "/wrong/bin",
        EYERIS_RSCRIPT: "/wrong/Rscript",
        EYERIS_SOURCE: "/wrong/source",
        SystemRoot: "C:\\Windows",
      },
    });
    assert.ok(env.R_HOME.startsWith(resources));
    assert.ok(env.R_LIBS_USER.startsWith(resources));
    assert.ok(env.RSTUDIO_PANDOC.startsWith(resources));
    assert.ok(!JSON.stringify(env).includes("/wrong/"));
    const executable = resolveRscript({
      platform,
      env: { ...env, EYERIS_RSCRIPT: "/wrong/Rscript" },
      available: () => true,
    });
    assert.ok(executable.startsWith(resources));
    assert.throws(
      () => resolveRscript({ platform, env, available: () => false }),
      /Reinstall eyeris/,
    );
  });
}
test("development retains explicit local R setup", () => {
  const env = rEnvironment({
    env: { EYERIS_RSCRIPT: "/custom/Rscript", R_LIBS: "/custom/library" },
  });
  assert.equal(env.EYERIS_RSCRIPT, "/custom/Rscript");
  assert.equal(env.R_LIBS, "/custom/library");
  assert.ok(env.EYERIS_SOURCE);
});
