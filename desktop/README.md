# eyeris Desktop

Electron + React desktop application for the eyeris R package. The package runs
all scientific processing in a separate local R process.

## Install and run

Native installers include a private R 4.6.0 runtime, all 75 pinned R dependency
packages, the current eyeris package, DuckDB, and Pandoc 3.10 for HTML reports.
Users do not install R, R packages, Pandoc, Node.js, or compilers. Runtime
preparation happens during the build; the installed app does not download or
install dependencies on first launch.

Install the native artifact and launch eyeris. The app checks its runtime,
package versions, package-library isolation, and Pandoc before enabling project
creation/opening. A damaged or incomplete installation shows an error instead of
silently falling back to system R. Reinstall the app to repair the bundle.

Packaged builds always use their bundled R, even if `EYERIS_RSCRIPT`, `R_HOME`,
`R_LIBS`, `R_LIBS_USER`, or RStudio/Pandoc settings exist in the user's environment.
User R profiles and workspaces are skipped. The package search path is restricted
to the app's dependency library and its private base R library. Project data and
settings stay in their normal writable user/project locations; the installation
is read-only during use.

Windows uses an x64 NSIS installer for the current user, with an installation
folder chooser and Start menu/desktop shortcuts. macOS uses a DMG/ZIP; the pinned
Apple Silicon R package binaries require macOS Sonoma 14 or newer. Linux uses an
AppImage built on Ubuntu 22.04, retaining the host's standard glibc/GUI requirements.
Native Windows ARM64 and 32-bit Windows are not targeted. macOS distribution
requires signing/notarization. Unsigned Windows installers may show SmartScreen
warnings; Windows signing can be enabled when a certificate is available.

## Development

Development uses local R so changes to the R source load directly through pkgload.
Install Node.js 22.13+, R with the eyeris dependencies, pkgload, duckdb, and Pandoc.
From `desktop/`, these commands work in PowerShell and Unix shells:

```sh
npm ci
npm start
npm test
npm run test:e2e
```

Development R discovery checks `EYERIS_RSCRIPT`, `R_HOME`, PATH, standard macOS
locations, and Windows registry/install directories. `EYERIS_RSCRIPT` is a
development override only; it never overrides a packaged runtime.

## Projects and processing

The app always starts at the project splash. Create a new `.eyeris` project
folder, open an existing folder, or choose the recent project. Opening a project
does not automatically run any processing.

1. Create a subject using an alphanumeric ID, without the `sub-` prefix.
2. Enter the session and task, then select one EyeLink `.asc` file per run. The
   application copies each file into the project. Selecting several files adds
   consecutive runs in filename order, starting after the subject's existing runs
   of that session and task, or at **First run** if it is set. The run number is
   passed to `eyeris::bidsify(run_num = ...)`. As in the package, an ASC that
   contains several recording blocks has its blocks numbered as runs instead.
3. Configure the glassbox steps. The interface provides the package's default
   pipeline, individual step switches, common parameters, eye selection, and a
   random seed. Advanced JSON exposes additional supported step parameters.
   Binning and downsampling are mutually exclusive.
4. Optionally enable epoch extraction with an event pattern and time limits. Add
   baseline correction if needed. Epoching is optional for preprocessing but is
   required to add the result to the trial-review queue.
5. Select HTML reports and/or a DuckDB database, check the recordings to process,
   then run the pipeline. It calls `eyeris::glassbox()`, optional `eyeris::epoch()`,
   and `eyeris::bidsify()` directly for each recording, in run order, in one R
   session that writes one BIDS folder. Each session-level report and database
   therefore covers all of the runs processed together. Process all runs of a
   session in one job; the app notes when only some are selected. An ASC with
   several blocks cannot share a job with other runs of its task, because its
   blocks would overwrite those runs. Processing runs separately from the review
   worker, so existing epochs remain available for review. Progress reports the
   recording and package operation, and the log updates while R runs. Cancel
   stops the whole job before publication.
6. Open the completed job's files or go directly to epoch review, which opens on
   the subject with all of its runs. Each run's saved RDS and extracted epochs
   are indexed automatically.

Project layout:

```text
Study.eyeris/
  review.sqlite                     # review index, decisions, subjects, jobs
  sourcedata/sub-001/ses-01/task-memory/run-01/recording.asc
  sources/<sha256>.rds               # immutable review sources
  bids/                             # non-conflicting published package outputs
  processing/<run-id>/
    config.json                     # exact processing arguments
    runtime.json                    # R/package version and session information
    reproduce.R                     # script for rerunning outside the GUI
    process.log
    sub-001_ses-01_task-memory_run-01.rds  # one per processed recording
    bids/                           # complete BIDS output from this run
```

Every processing run retains its own complete output. The shared `bids/` tree is
populated only if that run has no conflicting files. Reprocessing with different
parameters never overwrites the first published dataset: the new complete BIDS
output remains in `processing/<run-id>/bids/`, and the UI explains this. Use
**Show run output** for the selected run's exact outputs. Failed/cancelled runs
retain logs and partial staging output in their run folder, but do not publish
that partial output to the common BIDS tree. Reopening a project marks unfinished
jobs as interrupted. Processing history includes reusable settings.

Projects created by earlier versions are upgraded when opened. Their existing
recordings keep block-based run numbering, and later files for the same session
and task are added as the next runs. Upgraded projects cannot be opened by
earlier versions of the app.

The replay script is intended to run from its processing folder. Its configuration
records absolute paths from the run; update those paths if the project is moved.

## Review

Use **Import processed RDS** to inspect saved, epoched eyeris objects from outside
the GUI. Select multiple files for a study. The `sub-` filename entity supplies
the participant label; otherwise the filename is used. `ses-`, `task-` and
`run-` entities label each epoch's session, task and run. Following
`eyeris::bidsify()`, the filename's run applies when the recording has one block;
otherwise, and when the name has no run, block numbers name the runs.

- Select the final available stage or an individual stored preprocessing column.
- Search events, trials, participants or epoch labels; filter by review status,
  participant or run. A participant's runs are listed together in session, task
  and run order. The queue pages 80 epochs at a time.
- Hover to inspect, drag horizontally to zoom, and reset or double-click to unzoom.
- `K` keeps, `X` excludes, arrow keys navigate, and `U` or Cmd/Ctrl-Z undoes the last
  decision outside text inputs. Decisions, reviewer, reason, and inspected stage
  are saved to SQLite immediately. Auto-advance can be disabled.
- Export creates retained, excluded, and unreviewed tables, plus `decisions.csv`
  (including session, task and run) and `manifest.json` with audit history. Only explicit keep decisions enter the
  retained data. Existing exports are never overwritten.

Decisions apply to an individual epoch across all stored stages, not other epoch
labels or the other eye. Epoch identities combine a source SHA-256 with eye,
label, block and row bounds. Repeated trial numbers and overlapping windows remain
distinct. Changed source bytes start a new review; decisions never silently carry
to changed preprocessing results. Source hashes are checked after reopening and
before export.

The plot uses the stored `timebin` axis (time from epoch start, not necessarily
from event onset). Display reduction preserves local extrema and missing-data
boundaries; zoom requests original stored samples. It does not reconstruct
pre-decimation epochs. Exports preserve all stored samples, columns and stages.
RDS exports are data frames, not complete eyeris objects; continuous signals,
baseline lists and confounds are not filtered by review exports. Each exported
frame adds `.review_epoch_id` for joining decisions. Use RDS to preserve R types
and numeric precision. CSV is provided alongside it.

## Build and sign

```sh
make desktop-build      # native-platform installer; unsigned on macOS
make desktop-sign       # macOS Developer ID signed DMG and ZIP
make desktop-release    # macOS signed, notarized DMG and ZIP
```

Artifacts are written to `desktop/release/`. Builds generate a solid `#820000`
red square icon with the original sticker's white artwork and install the current eyeris source
package into the application resources. The macOS bundle identifier is
`com.shawnschwartz.eyeris`. macOS uses DMG/ZIP, Windows uses NSIS, and Linux uses
AppImage; build on the target platform/architecture. The desktop CI workflow
builds native installers on Windows x64, macOS, and Linux. Local execution in this
workspace verifies macOS; Windows/Linux runtime validation requires the respective
CI jobs to pass.

The DMG uses a branded drag-to-install layout. Edit `build/dmg-background.svg`
to change its artwork; `npm run icons` renders standard and Retina backgrounds.
Icon positions and window dimensions live in `electron-builder.cjs`.
The mounted volume uses the original transparent hex sticker, generated as
`build/dmg-icon.icns` on macOS; the application retains its solid red square icon.

### Private runtime preparation

Build natively using exactly R 4.6.0 and Pandoc 3.10. Use the official CRAN R
distribution on macOS/Windows; Homebrew R is not ABI-compatible with every CRAN
macOS binary. `EYERIS_BUILD_R_HOME` can select an extracted official R home
without changing the developer's system installation. The repository's CI provisions
these versions. Run `npm run package`; `npm run runtime:prepare` prepares and
validates just the runtime. Build-time internet access is required for the pinned
package snapshot. Linux preparation currently targets Ubuntu 22.04 and needs
`patchelf`; macOS needs the command-line developer tools for dylib relocation.
Windows uses the official R installation's relocatable directory layout.
Optional macOS X11/Tcl/Tk/data-entry modules are omitted; the app uses native
offscreen graphics and does not require XQuartz.

`runtime-lock.json` pins the R/Pandoc versions and complete dependency closure to
a dated Posit package snapshot. Builds download exact native package binaries
(macOS/Windows) or the dated Ubuntu binary repository, verify every version, and
install the current repository's eyeris source into `build/r-library`. Missing
versions or native dependencies fail the build. No personal package library is
copied wholesale. Downloaded archives are cached under `build/runtime-cache`.

The builder copies R into `build/runtime/R`, bundles Pandoc, and relocates native
shared-library dependencies into the app. macOS load commands use relative paths
and receive ad-hoc signatures before the final app signing stage. Linux uses
relative RPATHs while retaining standard system glibc libraries. The generated
`runtime/manifest.json` records versions, architecture, and native dependency
origins; R/package copyright files and third-party notices accompany the bundle.

To intentionally update dependencies, run this from `desktop/`, review the lock
diff, and rebuild/test every platform:

```sh
Rscript scripts/update-runtime-lock.R YYYY-MM-DD
```

Update the R/Pandoc pins and corresponding CI toolchain versions together when
upgrading those tools. Package versions must be available for every target.

The macOS configuration defaults to the developer's existing Developer ID
Application identity. Override it with `CSC_NAME` (omit the `Developer ID
Application:` prefix). CI may instead supply electron-builder's `CSC_LINK` and
`CSC_KEY_PASSWORD`. Signing uses the macOS keychain; private keys are not stored
in this repository. The release target refuses to silently produce an unsigned
or unnotarized build.

For App Store Connect notarization, configure these in your shell or CI secret
store, never in a committed file:

```sh
export APPLE_API_KEY=/absolute/path/to/AuthKey_KEYID.p8
export APPLE_API_KEY_ID=YOUR_KEY_ID
export APPLE_API_ISSUER=YOUR_ISSUER_ID
make desktop-release
```

A stored notarytool profile can alternatively be selected with
`APPLE_KEYCHAIN_PROFILE` (and optionally `APPLE_KEYCHAIN`). The release command
submits to Apple's notarization service through electron-builder and staples the
app. It does not publish a GitHub release or upload public downloads. A signed
build from `desktop-sign` is **not notarized** and should not be described as a
Gatekeeper-ready public release.

References: [macOS signing](https://www.electron.build/v26/docs/mac/) and
[packaging resources](https://www.electron.build/v26/docs/contents/).

## Validation and remaining limits

```sh
make desktop-test
# Or on any platform, from desktop/:
npm test
npm run test:e2e
# After npm run package (defaults to the native unpacked executable):
npm run test:package
# Or verify a specific installed executable:
npm run test:package -- "C:\Users\you\AppData\Local\Programs\eyeris\eyeris.exe"
```

`.github/workflows/desktop.yml` runs backend tests, Electron UI tests, native
installer builds, and packaged-app smoke tests on all three systems. The Windows
job silently installs the NSIS artifact into a path containing spaces, then tests
the installed executable. Smoke tests poison host R/package settings and cover startup validation, project
creation, ASC processing, BIDS/HTML/DuckDB output, epoch indexing, and the bundled demo. Linux GUI tests use Xvfb.
Workflow artifacts retain installers and test screenshots. The workflow can also
be launched manually through GitHub Actions; adding it does not constitute a
successful Windows/Linux test run.

If Windows R terminates with access violation `0xC0000005`, processing retries
once with a clean BIDS output directory. The interrupted files and log remain
under `processing/<job>/failed-attempt-1`, with a `recovery.json` record beside
them. Only a successful attempt is published. Cancellation, ordinary R errors,
and a second native crash do not retry. CI retains recovered-crash evidence and
Windows native crash reports; this recovery does not resolve the underlying
intermittent native-library fault.

Runtime-discovery tests exercise Windows registry and filesystem layouts, custom
paths, missing R, and macOS/Linux fallbacks on every platform. Backend tests check real ASC processing through the current package, BIDS CSV,
HTML and DuckDB outputs, replay metadata, cancellation, failure recovery, reruns,
10,001-epoch paging, repeated trial identities, gap/spike preservation, autosave,
undo, and lossless RDS exports. Playwright launches Electron to exercise the
project splash, subject creation, processing, plot controls, keyboard review,
reopening, export, and page-boundary navigation.

RDS access still loads one whole source into the R worker. Large combined studies
should use per-participant RDS sources; a DuckDB/Parquet review input adapter and
out-of-core access remain future work. Review currently exports epoch tables;
applying decisions to every associated confound/baseline structure is not yet
implemented. Use one application process per project.

## Downloads and automatic updates

After the first desktop release is published, these links always offer the
current desktop installer (no GitHub account required):

| Platform            | Download                                                                                                 |
| ------------------- | -------------------------------------------------------------------------------------------------------- |
| Windows x64         | [Installer](https://github.com/shawntz/eyeris/releases/download/desktop-latest/eyeris-windows-x64.exe)   |
| macOS Apple Silicon | [DMG](https://github.com/shawntz/eyeris/releases/download/desktop-latest/eyeris-macos-arm64.dmg)         |
| Linux x64           | [AppImage](https://github.com/shawntz/eyeris/releases/download/desktop-latest/eyeris-linux-x64.AppImage) |

[Desktop downloads and release history](https://github.com/shawntz/eyeris/releases/tag/desktop-latest).
Intel Mac builds and downloads are temporarily paused because of recurring CI
failures. The macOS installer currently supports Apple Silicon only.
Move the macOS app into Applications before running it. On Linux, make the
AppImage executable and run the AppImage itself; an extracted app has no
AppImage installation for the updater to replace.

Installed apps check 30 seconds after startup and every six hours while open.
Use **Check for updates** to check immediately. A newer version offers **Download
update**, then **Restart and install**. Downloads do not begin without the user's
request, and updates never silently restart the app or install on normal exit.
Restart is blocked while processing, importing/exporting, or creating a demo.
Network errors leave the current app usable and can be retried. Automatic
checks are disabled in development and package smoke tests.

Updates replace the entire app bundle, including its pinned R runtime and
libraries. Projects and settings remain in their existing user directories.
Electron Updater validates downloaded artifacts against their SHA-512 metadata
and the platform's code-signature requirements. This PR's automated tests cover
feed preparation, version guards, updater behavior and UI; a real signed
upgrade between two published versions should be tested before announcing
production auto-updates.

## Publishing a desktop release

Desktop versions live **only in `desktop/package.json` and its lockfile**.
The R package's `DESCRIPTION`, CRAN `v3.x` tags, and GitHub's repository-wide
“Latest” release are independent. GitHub Packages is not used: installers and
update metadata are GitHub Release assets in this same repository.

The `Publish desktop release` workflow runs on **`desktop-v*` tag pushes**. It
requires an exact match with the desktop package version and stable semantic
versions (no prerelease channel yet). It builds Windows x64, macOS arm64,
and Ubuntu 22.04 x64 natively, runs backend/UI/packaged smoke tests, and publishes
only if every platform passes. PR builds never publish or receive signing
credentials. Release artifacts include installers, macOS ZIP updates, blockmaps,
update metadata, and SHA256SUMS.

Configure the Apple repository Actions secrets before the first release.
Windows certificate secrets are optional until a certificate is available:

| Secret                                 | Value                                                    |
| -------------------------------------- | -------------------------------------------------------- |
| `DESKTOP_MAC_CERTIFICATE`              | Base64-encoded Developer ID Application `.p12`           |
| `DESKTOP_MAC_CERTIFICATE_PASSWORD`     | Certificate export password                              |
| `DESKTOP_APPLE_API_KEY`                | Raw App Store Connect `.p8` key contents                 |
| `DESKTOP_APPLE_API_KEY_ID`             | App Store Connect key ID                                 |
| `DESKTOP_APPLE_API_ISSUER`             | App Store Connect issuer ID                              |
| `DESKTOP_WINDOWS_CERTIFICATE`          | Optional base64-encoded Authenticode `.pfx` usable by CI |
| `DESKTOP_WINDOWS_CERTIFICATE_PASSWORD` | Certificate export password                              |

Windows hardware-backed/cloud signing requires adapting the signing step to
that service; do not export a non-exportable key. macOS release builds must be signed
and notarized; missing Apple credentials fail the release. Windows signing is
optional initially: without its certificate, CI warns and publishes an unsigned
NSIS installer, which may show SmartScreen warnings. Such updates still verify
SHA-512 hashes from the HTTPS desktop feed, but do not provide Authenticode
publisher verification. Configure the Windows certificate secrets to enable
signing and publisher verification for subsequent installed versions. GitHub's built-in
`GITHUB_TOKEN` supplies release publishing permission; no PAT is embedded in the
app. Keep the same signing identity for subsequent updates.

For the initial release, tag the merged commit containing this workflow as
`desktop-v0.3.0`. For a subsequent release:

```sh
cd desktop
npm version patch --no-git-tag-version
# Commit package.json/package-lock.json and merge the release changes.
# From the merged checkout, create/push the matching tag, for example:
git tag desktop-v0.3.1
git push origin desktop-v0.3.1
```

The publisher creates the versioned `desktop-v<version>` release first, then
updates the `desktop-latest` download channel. Neither is marked as GitHub's
repository-wide Latest release. The dedicated generic updater feed reads only
`desktop-latest/latest*.yml`, with absolute download URLs pointing to immutable
versioned desktop assets. Publishing an R release cannot affect this feed.
Both Mac architectures are merged into one `latest-mac.yml` before promotion.
Checksums, required platform assets, version ordering, and tag/package agreement
are validated before publishing; an older version cannot replace a newer channel.

Do not overwrite published versioned binaries. If a published build needs a fix,
bump the desktop version and release again. A promotion retry may reuse identical
artifacts from the original successful build; differing bytes under an existing
version are rejected. The workflow serializes channel promotion and uploads
metadata last. A failed platform build leaves the existing channel untouched.
To roll back an application bug, publish the corrected code under a higher
desktop version; clients intentionally do not downgrade.

### Automatic patch releases from dev

After the initial `desktop-v0.3.0` release, merge the automatic-release workflow
follow-up PR. Each push to `dev` checks for changes under `desktop/` or to the
`.github/workflows/desktop*.yml` workflows since the latest desktop tag. Changes
only to the R package or root documentation do not release the desktop app.
Queued pushes are combined by checking the newest dev head, so a later unrelated
push cannot hide an earlier desktop change.

For a relevant change, the workflow:

1. Runs the complete native desktop CI matrix against the selected commit.
2. Increments the patch version in `desktop/package.json` and both root version
   fields in `desktop/package-lock.json` (for example, `0.3.0` to `0.3.1`).
3. Creates and merges a version-only PR into `dev`, respecting its existing
   requirement that commits arrive through PRs. No manual approval is needed
   under the current zero-required-approvals policy.
4. Tags the resulting version commit as `desktop-v0.3.1` and directly calls the
   signed release workflow, which builds and validates that exact tag before
   publishing its installers and update feeds.

This uses the built-in `GITHUB_TOKEN`; no extra PAT or GitHub App is needed.
GitHub does not start push workflows for commits/tags made by that token, so
version commits cannot recursively release. The direct reusable-workflow call
ensures the generated tag still produces a release. Validation artifacts and
signed artifacts use separate names within the same workflow run.

The repository must continue to allow Actions to create pull requests. If branch
rules later require additional reviews or checks, the version PR will remain
reviewable and the workflow will fail instead of bypassing those rules. New dev
commits arriving during validation cause obsolete candidates to be skipped;
changes racing with the version merge are never tagged without validation.

The automation activates when this workflow lands on `dev`; changes on feature
branches and PRs do not release. Merging this follow-up itself counts as a desktop
workflow change. If enabled before any desktop tag exists, the first automatic
release increments the manifest's current patch version; publish `0.3.0` first
if that should be the initial public version.

To resume a failed run, use **Re-run failed jobs** so the selected version and
validated source are retained. A failure after tagging can also be retried by
running `Publish desktop release` against the existing desktop tag. If artifacts
were already published, reuse the original artifacts when retrying promotion;
rebuilt binaries with different bytes still require a new version. No workflow
modifies `DESCRIPTION` or creates CRAN-style `v*` tags.

The CI signing setup imports the Developer ID certificate into a temporary
keychain using a separate random keychain password. It passes `CSC_KEYCHAIN` to
electron-builder with `CSC_LINK` unset, avoiding the certificate/keychain password
mix-up in electron-builder 26.15.3. Certificate files, the notarization key, and
the temporary keychain are removed even when a build fails. Existing secret names
and values do not change.

For release troubleshooting, manually run **Desktop cross-platform** on the fix
branch with **signing-check** enabled. This builds and notarizes installers using
the configured secrets and runs packaged smoke tests, but does not commit, tag,
publish, or update the download channel. Pull-request runs remain unsigned.
