import { fileURLToPath } from "node:url";
import path from "node:path";

export function rDirectory() {
  return process.env.EYERIS_RESOURCE_DIR
    ? path.join(process.env.EYERIS_RESOURCE_DIR, "r")
    : fileURLToPath(new URL("../r/", import.meta.url));
}
export function rEnvironment() {
  return {
    ...process.env,
    EYERIS_PACKAGE_LIBRARY: process.env.EYERIS_RESOURCE_DIR
      ? path.join(process.env.EYERIS_RESOURCE_DIR, "r-library")
      : "",
    EYERIS_SOURCE: process.env.EYERIS_RESOURCE_DIR
      ? ""
      : fileURLToPath(new URL("../../", import.meta.url)),
  };
}
