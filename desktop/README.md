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
Native Windows ARM64 and 32-bit Windows are not targeted. Public distribution
still requires platform signing; bundling R does not replace signing/notarization.

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
2. Enter the session and task, then select an EyeLink `.asc` file. The application
   copies it into the project. Each session/task accepts one ASC per subject;
   multiple blocks in that ASC become BIDS runs using the package's existing rules.
3. Configure the glassbox steps. The interface provides the package's default
   pipeline, individual step switches, common parameters, eye selection, and a
   random seed. Advanced JSON exposes additional supported step parameters.
   Binning and downsampling are mutually exclusive.
4. Optionally enable epoch extraction with an event pattern and time limits. Add
   baseline correction if needed. Epoching is optional for preprocessing but is
   required to add the result to the trial-review queue.
5. Select HTML reports and/or a DuckDB database, then run the pipeline. It calls
   `eyeris::glassbox()`, optional `eyeris::epoch()`, and `eyeris::bidsify()` directly.
   Processing runs separately from the review worker, so existing epochs remain
   available for review. Progress reports the active package operation and the
   processing log updates while R runs. Cancel stops the R job before publication.
6. Open the completed run's files or go directly to epoch review. The saved RDS
   and any extracted epochs are indexed automatically.

Project layout:

```text
Study.eyeris/
  review.sqlite                     # review index, decisions, subjects, jobs
  sourcedata/sub-001/ses-01/task-memory/recording.asc
  sources/<sha256>.rds               # immutable review sources
  bids/                             # non-conflicting published package outputs
  processing/<run-id>/
    config.json                     # exact processing arguments
    runtime.json                    # R/package version and session information
    reproduce.R                     # script for rerunning outside the GUI
    process.log
    sub-001_ses-01_task-memory.rds
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

The replay script is intended to run from its processing folder. Its configuration
records absolute paths from the run; update those paths if the project is moved.

## Review

Use **Import processed RDS** to inspect saved, epoched eyeris objects from outside
the GUI. Select multiple files for a study. The `sub-` filename entity supplies
the participant label; otherwise the filename is used.

- Select the final available stage or an individual stored preprocessing column.
- Search events, trials, participants or epoch labels; filter by review status or
  participant. The queue pages 80 epochs at a time.
- Hover to inspect, drag horizontally to zoom, and reset or double-click to unzoom.
- `K` keeps, `X` excludes, arrow keys navigate, and `U` or Cmd/Ctrl-Z undoes the last
  decision outside text inputs. Decisions, reviewer, reason, and inspected stage
  are saved to SQLite immediately. Auto-advance can be disabled.
- Export creates retained, excluded, and unreviewed tables, plus `decisions.csv`
  and `manifest.json` with audit history. Only explicit keep decisions enter the
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
