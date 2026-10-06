import sharp from "sharp";
import { mkdir, copyFile, mkdtemp, rm } from "node:fs/promises";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
const root = fileURLToPath(new URL("../", import.meta.url));
await mkdir(root + "build", { recursive: true });
await mkdir(root + "public", { recursive: true });
await copyFile(
  root + "../inst/figures/sticker.png",
  root + "public/sticker.png",
);
// Extend the sticker's red across the entire icon canvas. Keep its original
// white artwork; macOS supplies the native rounded-square mask for the bundle.
await sharp(root + "public/sticker.png")
  .resize(1024, 1024, {
    fit: "contain",
    background: "#820000",
  })
  .flatten({ background: "#820000" })
  .png()
  .toFile(root + "build/icon.png");
await copyFile(root + "build/icon.png", root + "public/app-icon.png");
// The mounted installer has its own icon: the original transparent hex sticker.
if (process.platform === "darwin") {
  const temp = await mkdtemp(path.join(tmpdir(), "eyeris-volume-icon-"));
  const iconset = path.join(temp, "sticker.iconset");
  try {
    await mkdir(iconset);
    for (const size of [16, 32, 128, 256, 512]) {
      for (const scale of [1, 2]) {
        await sharp(root + "public/sticker.png")
          .resize(size * scale, size * scale, {
            fit: "contain",
            background: { r: 0, g: 0, b: 0, alpha: 0 },
          })
          .png()
          .toFile(
            path.join(
              iconset,
              `icon_${size}x${size}${scale === 2 ? "@2x" : ""}.png`,
            ),
          );
      }
    }
    execFileSync("iconutil", [
      "-c",
      "icns",
      iconset,
      "-o",
      root + "build/dmg-icon.icns",
    ]);
  } finally {
    await rm(temp, { recursive: true, force: true });
  }
}
// Render the editable vector installer artwork at standard and Retina sizes.
for (const scale of [1, 2]) {
  await sharp(root + "build/dmg-background.svg", { density: 72 * scale })
    .png()
    .toFile(root + `build/dmg-background${scale === 2 ? "@2x" : ""}.png`);
}
