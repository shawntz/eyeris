# eyeris Desktop

Electron + React desktop application for the eyeris R package. The package runs
all scientific processing in a separate local R process.

## Run

Requires Node.js 22.13+ (or a newer supported release), npm, and R with the eyeris
dependencies installed. Development also needs `pkgload`; it loads the current
repository rather than an older installed eyeris version. HTML reports require
Pandoc; DuckDB output requires the R `duckdb` package.

```sh
make desktop-deps
make desktop
```

The app discovers R using `EYERIS_RSCRIPT`, `R_HOME`, and PATH, then standard
installation locations. Windows also checks R's per-user and machine registry
entries, `%LOCALAPPDATA%\Programs\R`, and `%ProgramFiles%\R`; macOS checks
Homebrew and the R framework. Paths containing spaces are supported. R processes
run without opening console windows on Windows. Set `EYERIS_RSCRIPT` to select a
custom installation. The desktop app has no remote server or account requirement.

### Windows development and builds

Use 64-bit Windows 10/11 with 64-bit R and Node.js. Install the R dependencies
from an R console with the repository root as the working directory:

```r
install.packages(c("remotes", "pkgload", "duckdb"), repos = "https://cloud.r-project.org")
remotes::install_local(".", dependencies = NA, upgrade = "never", build_vignettes = FALSE)
```

Install Pandoc for HTML reports. If a dependency must be built from source,
install the Rtools version matching your R installation. Then use PowerShell;
GNU Make, WSL, and a Unix shell are not required:

```powershell
cd desktop
npm ci
npm start
npm test
npm run test:e2e
npm run package
```

The build creates `release/eyeris-<version>-win-x64.exe`, an NSIS installer with
an installation-folder chooser and Start menu/desktop shortcuts. It installs for
the current user by default. The current Windows target is x64; native Windows
ARM64 and 32-bit Windows are not covered. For a custom R installation, set this
before launching or building (substitute your actual path):

```powershell
$env:EYERIS_RSCRIPT = 'D:\Tools\R\bin\Rscript.exe'
npm start
```

For an installed app, persist that variable through Windows Environment Variables
and restart the app. End users need R and the eyeris R dependencies, but do not
need Node.js. The installer bundles the repository's eyeris package. Unsigned
Windows builds may display an unknown-publisher/SmartScreen prompt; public
Windows signing credentials must be configured separately.

See the [R for Windows FAQ](https://cran.r-project.org/bin/windows/base/rw-FAQ.html)
for R installation locations and Rtools requirements.

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

**The installer bundles eyeris, not the R runtime or its dependency library.**
Users still need a compatible local R installation with eyeris dependencies.
This is a downloadable desktop build, not a standalone R distribution. Full
runtime bundling is separate work; do not distribute it as requiring no setup.

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
the installed executable. Smoke tests cover project creation, ASC processing,
BIDS output, epoch indexing, and the bundled demo. Linux GUI tests use Xvfb.
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
