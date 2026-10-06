import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import { writeFile, rm, appendFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

function security(args) {
  try {
    return execFileSync("/usr/bin/security", args, {
      encoding: "utf8",
      stdio: ["ignore", "pipe", "pipe"],
    });
  } catch {
    // execFileSync errors include command arguments, which contain passwords.
    throw new Error(
      `macOS signing: security ${args[0]} failed. Check the certificate and signing configuration.`,
    );
  }
}
export async function prepareSigningKeychain({
  directory,
  certificate,
  certificatePassword,
  run = security,
  mask = () => {},
  password = randomBytes(32).toString("hex"),
}) {
  if (!directory || !certificate || certificatePassword === undefined)
    throw new Error("Missing macOS signing certificate configuration");
  const keychain = path.join(directory, "eyeris-signing.keychain-db");
  const file = path.join(directory, "eyeris-signing.p12");
  mask(password);
  await writeFile(file, Buffer.from(certificate, "base64"), { mode: 0o600 });
  try {
    run(["create-keychain", "-p", password, keychain]);
    run(["set-keychain-settings", "-lut", "21600", keychain]);
    run(["unlock-keychain", "-p", password, keychain]);
    run([
      "import",
      file,
      "-k",
      keychain,
      "-T",
      "/usr/bin/codesign",
      "-T",
      "/usr/bin/productbuild",
      "-P",
      certificatePassword,
    ]);
    // -P above decrypts the PKCS#12; -k here unlocks the KEYCHAIN. Keeping
    // these distinct avoids electron-builder 26.15.3's importCerts bug (#10066).
    run([
      "set-key-partition-list",
      "-S",
      "apple-tool:,apple:",
      "-s",
      "-k",
      password,
      keychain,
    ]);
    const existing = [
      ...run(["list-keychains", "-d", "user"]).matchAll(/"([^"\r\n]+)"/g),
    ].map((match) => match[1]);
    run([
      "list-keychains",
      "-d",
      "user",
      "-s",
      keychain,
      ...existing.filter((entry) => entry !== keychain),
    ]);
    const identities = run([
      "find-identity",
      "-v",
      "-p",
      "codesigning",
      keychain,
    ]);
    if (!/"Developer ID Application: /.test(identities))
      throw new Error(
        "The signing keychain has no valid Developer ID Application identity with its private key",
      );
    return keychain;
  } finally {
    await rm(file, { force: true });
  }
}
if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  const keychain = await prepareSigningKeychain({
    directory: process.env.RUNNER_TEMP,
    certificate: process.env.MAC_CERTIFICATE,
    certificatePassword: process.env.MAC_CERTIFICATE_PASSWORD,
    mask: (value) => console.log(`::add-mask::${value}`),
  });
  await appendFile(process.env.GITHUB_ENV, `CSC_KEYCHAIN=${keychain}\n`);
  console.log("Developer ID signing keychain prepared.");
}
