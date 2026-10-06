import { spawnSync } from "node:child_process";
const sign = process.argv.includes("--sign");
if (process.platform !== "darwin")
  throw new Error(
    "The signing/release command runs on macOS. Use desktop-build on other platforms.",
  );
if (!sign) {
  const api = ["APPLE_API_KEY", "APPLE_API_KEY_ID", "APPLE_API_ISSUER"];
  const profile = process.env.APPLE_KEYCHAIN_PROFILE;
  if (!profile && !api.every((k) => process.env[k])) {
    console.error(
      "Notarization is not configured. Set APPLE_API_KEY (path to .p8), APPLE_API_KEY_ID, and APPLE_API_ISSUER, or APPLE_KEYCHAIN_PROFILE. Do not commit credentials. Use make desktop-sign for a signed build without notarization.",
    );
    process.exit(1);
  }
}
const result = spawnSync("npm", ["run", "package:prepare"], {
  stdio: "inherit",
});
if (result.status !== 0) process.exit(result.status || 1);
const built = spawnSync(
  "npx",
  ["electron-builder", "--mac", "--publish", "never"],
  {
    stdio: "inherit",
    env: { ...process.env, EYERIS_SIGN: "1", EYERIS_RELEASE: sign ? "0" : "1" },
  },
);
if (built.error) console.error(built.error.message);
process.exit(built.status ?? 1);
