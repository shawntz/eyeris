import { test } from "node:test";
import assert from "node:assert/strict";
import { resolveRscript } from "../electron/rscript.mjs";

function resolve(options, files = []) {
  return resolveRscript({
    platform: "win32",
    env: {},
    available: (file) => files.includes(file),
    list: () => [],
    exec: () => {
      throw new Error("No registry entry");
    },
    ...options,
  });
}

test("explicit Rscript override preserves spaces and takes precedence", () => {
  assert.equal(
    resolve({ env: { EYERIS_RSCRIPT: '"D:\\Custom R\\Rscript.exe"' } }),
    "D:\\Custom R\\Rscript.exe",
  );
});
test("Windows R_HOME supports the x64 bin layout", () => {
  const file = "D:\\R\\bin\\x64\\Rscript.exe";
  assert.equal(resolve({ env: { R_HOME: "D:\\R" } }, [file]), file);
});
test("Windows PATH is case insensitive and accepts quoted directories", () => {
  const file = "C:\\Program Files\\R\\bin\\Rscript.exe";
  assert.equal(
    resolve({ env: { Path: 'C:\\Other;"C:\\Program Files\\R\\bin"' } }, [file]),
    file,
  );
});
test("Windows registry discovers custom installations without a console", () => {
  const file = "D:\\Scientific tools\\R\\bin\\Rscript.exe";
  assert.equal(
    resolve(
      {
        exec: (_command, args, options) => {
          assert.equal(options.windowsHide, true);
          assert.equal(options.timeout, 2000);
          if (args[1].startsWith("HKCU"))
            throw new Error("Not registered for user");
          return "    InstallPath    REG_SZ    D:\\Scientific tools\\R\r\n";
        },
      },
      [file],
    ),
    file,
  );
});
test("Windows unregistered per-user installations select newest usable version", () => {
  const base = "C:\\Users\\Jane Doe\\AppData\\Local";
  const file = `${base}\\Programs\\R\\R-4.10.0\\bin\\Rscript.exe`;
  assert.equal(
    resolve(
      {
        env: { LOCALAPPDATA: base },
        list: () => ["R-4.9.0", "R-4.10.0", "R-4.11.0", "unrelated"],
      },
      [file],
    ),
    file,
  );
});
test("Windows falls back to machine-wide Program Files", () => {
  const file = "C:\\Program Files\\R\\R-4.6.0\\bin\\Rscript.exe";
  assert.equal(resolve({ list: () => ["R-4.6.0"] }, [file]), file);
});
test("macOS and Linux discover GUI runtimes without PATH", () => {
  for (const [platform, file] of [
    ["darwin", "/Library/Frameworks/R.framework/Resources/bin/Rscript"],
    ["linux", "/usr/bin/Rscript"],
  ]) {
    assert.equal(resolve({ platform }, [file]), file);
  }
});
test("missing installations fall back to the platform executable name", () => {
  assert.equal(resolve({}), "Rscript.exe");
  assert.equal(resolve({ platform: "linux" }), "Rscript");
});
