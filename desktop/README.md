# eyeris Desktop

Electron + React interface for local eyeris processing and epoch review.
Requires Node.js 22.13+, R with eyeris dependencies and pkgload, and Pandoc for reports.

```sh
cd desktop
npm ci
npm start
npm test
npm run test:e2e
```

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

