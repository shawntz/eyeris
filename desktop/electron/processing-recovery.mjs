import { mkdir, rename, writeFile } from "node:fs/promises";
import path from "node:path";

// R runs in a separate process and writes only to this job's staging directory.
// A native Windows access violation can therefore be retried without publishing
// or mixing any of the interrupted attempt's outputs. Never retry R errors.
export async function runWithWindowsRecovery({
  run,
  directory,
  outputs,
  isCancelled,
  onRetry = () => {},
  platform = process.platform,
}) {
  const first = await run(1);
  if (
    platform !== "win32" ||
    !Number.isInteger(first.code) ||
    first.code >>> 0 !== 0xc0000005 ||
    first.signal ||
    first.spawnError ||
    isCancelled()
  ) {
    return first;
  }

  const archive = path.join(directory, "failed-attempt-1");
  await mkdir(archive);
  for (const name of [
    "bids",
    ...outputs.map((output) => path.basename(output)),
    "runtime.json",
    "process.log",
  ]) {
    await rename(path.join(directory, name), path.join(archive, name)).catch(
      (error) => {
        if (error.code !== "ENOENT") throw error;
      },
    );
  }
  await writeFile(
    path.join(directory, "recovery.json"),
    JSON.stringify(
      {
        reason: "Windows R access violation",
        exitCode: first.code,
        failedAttempt: "failed-attempt-1",
      },
      null,
      2,
    ) + "\n",
  );
  await mkdir(path.join(directory, "bids"));
  onRetry();
  if (isCancelled()) return first;
  // One recovery attempt only. Its failure is returned unchanged to the caller.
  return run(2);
}
