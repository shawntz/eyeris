const release = process.env.EYERIS_RELEASE === "1";
const sign = release || process.env.EYERIS_SIGN === "1";
module.exports = {
  appId: "com.shawnschwartz.eyeris",
  productName: "eyeris",
  directories: { output: "release", buildResources: "build" },
  files: ["dist/**/*", "electron/**/*", "package.json"],
  extraResources: [
    { from: "r", to: "r" },
    { from: "build/runtime", to: "runtime" },
    { from: "build/r-library", to: "r-library" },
  ],
  asar: true,
  artifactName: "eyeris-${version}-${os}-${arch}.${ext}",
  mac: {
    target: ["dmg", "zip"],
    category: "public.app-category.education",
    icon: "build/icon.png",
    identity: sign ? process.env.CSC_NAME || undefined : null,
    hardenedRuntime: true,
    entitlements: "build/entitlements.mac.plist",
    entitlementsInherit: "build/entitlements.mac.plist",
    notarize: release,
  },
  forceCodeSigning:
    sign &&
    (process.platform === "darwin" ||
      (process.platform === "win32" && Boolean(process.env.CSC_LINK))),
  dmg: {
    title: "Install eyeris",
    icon: "build/dmg-icon.icns",
    background: "build/dmg-background.png",
    window: { width: 660, height: 480 },
    iconSize: 96,
    iconTextSize: 13,
    contents: [
      { x: 170, y: 273, type: "file" },
      { x: 490, y: 273, type: "link", path: "/Applications" },
    ],
  },
  win: { target: [{ target: "nsis", arch: ["x64"] }], icon: "build/icon.png" },
  nsis: {
    oneClick: false,
    perMachine: false,
    allowToChangeInstallationDirectory: true,
    createDesktopShortcut: true,
    createStartMenuShortcut: true,
  },
  linux: {
    target: ["AppImage"],
    executableName: "eyeris",
    icon: "build/icon.png",
    category: "Science",
  },
  publish: {
    provider: "generic",
    url: "https://github.com/shawntz/eyeris/releases/download/desktop-latest/",
    useMultipleRangeRequest: false,
  },
};
