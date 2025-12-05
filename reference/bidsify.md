# Save out pupil time series data in a BIDS-like structure

This method provides a structured way to save out pupil data in a
BIDS-like structure. The method saves out epoched data as well as the
raw pupil time series, and formats the directory and filename structures
based on the metadata you provide.

## Usage

``` r
bidsify(
  eyeris,
  save_all = TRUE,
  epochs_list = NULL,
  bids_dir = NULL,
  participant_id = NULL,
  session_num = NULL,
  task_name = NULL,
  run_num = NULL,
  save_raw = TRUE,
  html_report = TRUE,
  report_seed = 0,
  report_epoch_grouping_var_col = "matched_event",
  verbose = TRUE,
  csv_enabled = TRUE,
  db_enabled = FALSE,
  db_path = "my-project",
  parallel_processing = FALSE,
  merge_epochs = deprecated(),
  merge_runs = deprecated(),
  pdf_report = deprecated()
)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- save_all:

  Logical flag indicating whether all epochs are to be saved or only a
  subset of them. Defaults to `TRUE`

- epochs_list:

  List of epochs to be saved. Defaults to `NULL`

- bids_dir:

  Base bids_directory. Defaults to `NULL`

- participant_id:

  BIDS subject ID. Defaults to `NULL`

- session_num:

  BIDS session ID. Defaults to `NULL`

- task_name:

  BIDS task ID. Defaults to `NULL`

- run_num:

  BIDS run ID. Optional override for the run number when there's only
  one block of data present in a given `.asc` file. This allows you to
  manually specify a run number (e.g., "03") instead of using the
  default block number in `.asc` files (1). This is especially useful if
  you have a single `.asc` file for a single run of a task and want your
  BIDSified derivatives to be labeled correctly. However, for files with
  multiple recording blocks embedded within the **same** `.asc` file,
  this parameter is ignored and blocks are automatically numbered as
  runs (block 1 = run-01, block 2 = run-02, etc.) in the order they
  appeared/were recorded. Defaults to `NULL` (no override)

- save_raw:

  Logical flag indicating whether to save_raw pupil data in addition to
  epoched data. Defaults to `TRUE`

- html_report:

  Logical flag indicating whether to save out the `eyeris` preprocessing
  summary report as an HTML file. Defaults to `TRUE`

- report_seed:

  Random seed for the plots that will appear in the report Defaults to
  `0`. See [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  a more detailed description

- report_epoch_grouping_var_col:

  String name of grouping column to use for epoch-by-epoch diagnostic
  plots in an interactive rendered HTML report. Column name must exist
  (i.e., be a custom grouping variable name set within the metadata
  template of your
  [`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md)
  call). Defaults to `"matched_event"`, which all epoched data frames
  have as a valid column name. To disable these epoch-level diagnostic
  plots, set to `NULL`

- verbose:

  A flag to indicate whether to print detailed logging messages.
  Defaults to `TRUE`. Set to `FALSE` to suppress messages about the
  current processing step and run silently

- csv_enabled:

  Logical flag indicating whether to write CSV output files. Defaults to
  `TRUE`. Set to `FALSE` to disable CSV file generation, useful for
  large-scale cloud compute environments when using database storage
  only

- db_enabled:

  Logical flag indicating whether to write data to a `DuckDB` database.
  Defaults to `FALSE`. When `TRUE`, creates or connects to a database
  for centralized data storage and querying

- db_path:

  Database filename or path. Defaults to `"eyeris-proj.eyerisdb"`. If
  just a filename, the database will be created in the `derivatives/`
  directory. If a full path is provided, it will be used as specified

- parallel_processing:

  Logical flag to manually enable parallel database processing. When
  `TRUE`, uses temporary databases to avoid concurrency issues. Defaults
  to `FALSE` (auto-detect based on environment variables)

- merge_epochs:

  **(Deprecated)** This parameter is deprecated and will be ignored. All
  epochs are now saved as separate files following BIDS conventions.
  This parameter will be removed in a future version

- merge_runs:

  **(Deprecated)** This parameter is deprecated and will be ignored. All
  runs are now saved as separate files following BIDS conventions. This
  parameter will be removed in a future version

- pdf_report:

  **(Deprecated)** Use `html_report = TRUE` instead

## Value

Invisibly returns `NULL`. Called for its side effects

## Details

In the future, we intend for this function to save out the data in an
official BIDS format for eyetracking data (see [the proposal currently
under review
here](https://github.com/bids-standard/bids-specification/pull/1128)).
At this time, however, this function instead takes a more BIDS-inspired
approach to organizing the output files for preprocessed pupil data.

## See also

[`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html)

## Examples

``` r
# bleed around blink periods just long enough to remove majority of
#  deflections due to eyelid movements
# \donttest{
demo_data <- eyelink_asc_demo_dataset()

# example with unepoched data
demo_data |>
  eyeris::glassbox() |>
  eyeris::bidsify(
    bids_dir = tempdir(), # <- MAKE SURE TO UPDATE TO YOUR DESIRED LOCAL PATH
    participant_id = "001",
    session_num = "01",
    task_name = "assocret",
    run_num = "01",
    save_raw = TRUE, # save out raw time series
    html_report = TRUE, # generate interactive report document
    report_seed = 0 # make randomly selected plot epochs reproducible
  )
#> ✔ [2025-12-05 19:56:42] [OKAY] Running eyeris::load_asc()
#> ℹ [2025-12-05 19:56:42] [INFO] Processing block: block_1
#> ✔ [2025-12-05 19:56:42] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2025-12-05 19:56:42] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2025-12-05 19:56:42] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2025-12-05 19:56:42] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2025-12-05 19:56:43] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2025-12-05 19:56:43] [WARN] Skipping eyeris::bin() for block_1
#> ! [2025-12-05 19:56:43] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2025-12-05 19:56:43] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2025-12-05 19:56:43] [INFO] Block processing summary:
#> ℹ [2025-12-05 19:56:43] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2025-12-05 19:56:43] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2025-12-05 19:56:43] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2025-12-05 19:56:43] [INFO] Only 1 block detected...
#> ℹ [2025-12-05 19:56:43] [INFO] Using run_num = 01 for single block data
#> ! [2025-12-05 19:56:43] [WARN] '/tmp/Rtmpu0YvH8' already exists. Skipping
#> creation...
#> ℹ [2025-12-05 19:56:43] [INFO] '/tmp/Rtmpu0YvH8/derivatives' does not exist.
#> Creating...
#> ✔ [2025-12-05 19:56:43] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives'
#> ℹ [2025-12-05 19:56:43] [INFO] '/tmp/Rtmpu0YvH8/derivatives/sub-001' does not
#> exist. Creating...
#> ✔ [2025-12-05 19:56:43] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001'
#> ℹ [2025-12-05 19:56:43] [INFO] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01'
#> does not exist. Creating...
#> ✔ [2025-12-05 19:56:43] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01'
#> ℹ [2025-12-05 19:56:43] [INFO] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye'
#> does not exist. Creating...
#> ✔ [2025-12-05 19:56:43] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye'
#> ℹ [2025-12-05 19:56:43] [INFO] Writing blinks data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2025-12-05 19:56:43] [OKAY] Wrote blinks data (1 rows) to CSV
#> ℹ [2025-12-05 19:56:43] [INFO] Writing events data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2025-12-05 19:56:43] [OKAY] Wrote events data (67 rows) to CSV
#> ✔ [2025-12-05 19:56:43] [OKAY] Wrote timeseries data (20767 rows) to CSV
#> ✔ [2025-12-05 19:56:43] [OKAY] Wrote run_confounds data (6 rows) to CSV
#> ℹ [2025-12-05 19:56:43] [INFO]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures' does not exist.
#> Creating...
#> ✔ [2025-12-05 19:56:43] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures'
#> ℹ [2025-12-05 19:56:43] [INFO]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' does not
#> exist. Creating...
#> ✔ [2025-12-05 19:56:43] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01'
#> ℹ [2025-12-05 19:56:43] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:43] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:43] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:44] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:45] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:45] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:45] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:45] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:45] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:45] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2025-12-05 19:56:45] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ✔ [2025-12-05 19:56:46] [OKAY] Created gaze heatmap for run-01
#> ! [2025-12-05 19:56:48] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a713f2c9c26.html 
#> 
#> Output created: sub-001.html
#> ℹ [2025-12-05 19:56:49] [INFO] Finished BIDSify for sub-001 (Duration: 6.61
#> seconds)

# example with epoched data
demo_data |>
  eyeris::glassbox() |>
  eyeris::epoch(
    events = "PROBE_{startstop}_{trial}",
    limits = c(-1, 1), # grab 1 second prior to and 1 second post event
    label = "prePostProbe" # custom epoch label name
  ) |>
  eyeris::bidsify(
    bids_dir = tempdir(), # <- MAKE SURE TO UPDATE TO YOUR DESIRED LOCAL PATH
    participant_id = "001",
    session_num = "01",
    task_name = "assocret",
    run_num = "01"
  )
#> ✔ [2025-12-05 19:56:49] [OKAY] Running eyeris::load_asc()
#> ℹ [2025-12-05 19:56:50] [INFO] Processing block: block_1
#> ✔ [2025-12-05 19:56:50] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2025-12-05 19:56:50] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2025-12-05 19:56:50] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2025-12-05 19:56:50] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2025-12-05 19:56:50] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2025-12-05 19:56:50] [WARN] Skipping eyeris::bin() for block_1
#> ! [2025-12-05 19:56:50] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2025-12-05 19:56:50] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2025-12-05 19:56:50] [INFO] Block processing summary:
#> ℹ [2025-12-05 19:56:50] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2025-12-05 19:56:50] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2025-12-05 19:56:50] [INFO] Epoching pupil data...
#> ℹ [2025-12-05 19:56:50] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2025-12-05 19:56:50] [OKAY] Done!
#> ✔ [2025-12-05 19:56:50] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2025-12-05 19:56:50] [OKAY] Pupil epoching completed in 0.22 seconds
#> ℹ [2025-12-05 19:56:50] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2025-12-05 19:56:50] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2025-12-05 19:56:50] [INFO] Only 1 block detected...
#> ℹ [2025-12-05 19:56:50] [INFO] Using run_num = 01 for single block data
#> ℹ [2025-12-05 19:56:50] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2025-12-05 19:56:50] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2025-12-05 19:56:50] [INFO] epoch_prePostProbe:
#> ℹ [2025-12-05 19:56:50] [INFO] block_1: data.frame with 20000 rows
#> ℹ [2025-12-05 19:56:50] [INFO] info: list with 1 elements
#> ! [2025-12-05 19:56:50] [WARN] '/tmp/Rtmpu0YvH8' already exists. Skipping
#> creation...
#> ! [2025-12-05 19:56:50] [WARN] '/tmp/Rtmpu0YvH8/derivatives' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:56:50] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2025-12-05 19:56:50] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2025-12-05 19:56:50] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2025-12-05 19:56:50] [INFO] Writing blinks data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2025-12-05 19:56:50] [OKAY] Wrote blinks data (1 rows) to CSV
#> ℹ [2025-12-05 19:56:50] [INFO] Writing events data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2025-12-05 19:56:50] [OKAY] Wrote events data (67 rows) to CSV
#> ℹ [2025-12-05 19:56:50] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2025-12-05 19:56:50] [INFO] Block block_1 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2025-12-05 19:56:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:56:50] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:56:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:56:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:56:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote epochs data (20000 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote timeseries data (20767 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote run_confounds data (6 rows) to CSV
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:56:51] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote epoch_summary data (1 rows) to CSV
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:56:51] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:56:51] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:56:51] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:56:51] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ! [2025-12-05 19:56:51] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:56:51] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ℹ [2025-12-05 19:56:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:53] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:53] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:53] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:56:53] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2025-12-05 19:56:53] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ✔ [2025-12-05 19:56:53] [OKAY] Created gaze heatmap for run-01
#> ! [2025-12-05 19:56:53] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ℹ [2025-12-05 19:56:53] [INFO]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2025-12-05 19:56:53] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe'
#> ✔ [2025-12-05 19:57:01] [OKAY] Created epoch images zip:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe/run-01.zip
#> (70 images)
#> ℹ [2025-12-05 19:57:01] [INFO] Using absolute zip file path:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe/run-01.zip
#> ✔ [2025-12-05 19:57:01] [OKAY] Embedded zip file as data URL (7903929 bytes)
#> 
#> 
#> processing file: sub-001_epoch-prePostProbe_run-01.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_epoch-prePostProbe_run-01.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001_epoch-prePostProbe_run-01.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_epoch-prePostProbe_run-01.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a718b90ea1.html 
#> 
#> Output created: sub-001_epoch-prePostProbe_run-01.html
#> ! [2025-12-05 19:57:08] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2025-12-05 19:57:08] [INFO] Removing duplicate plain epoch directory:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe
#> ! [2025-12-05 19:57:08] [WARN] Metadata file already exists for 1:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/logs/run-01_metadata.json
#> ! [2025-12-05 19:57:08] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a711b1db404.html 
#> 
#> Output created: sub-001.html
#> ℹ [2025-12-05 19:57:10] [INFO] Finished BIDSify for sub-001 (Duration: 19.3
#> seconds)

# example with run_num for single block data
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox() |>
  eyeris::epoch(
    events = "PROBE_{startstop}_{trial}",
    limits = c(-1, 1),
    label = "prePostProbe"
  ) |>
  eyeris::bidsify(
    bids_dir = tempdir(),
    participant_id = "001",
    session_num = "01",
    task_name = "assocret",
    run_num = "03" # override default run-01 (block_1) to use run-03 instead
  )
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::load_asc()
#> ℹ [2025-12-05 19:57:10] [INFO] Processing block: block_1
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2025-12-05 19:57:10] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2025-12-05 19:57:10] [WARN] Skipping eyeris::bin() for block_1
#> ! [2025-12-05 19:57:10] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2025-12-05 19:57:10] [INFO] Block processing summary:
#> ℹ [2025-12-05 19:57:10] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2025-12-05 19:57:10] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2025-12-05 19:57:10] [INFO] Epoching pupil data...
#> ℹ [2025-12-05 19:57:10] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2025-12-05 19:57:10] [OKAY] Done!
#> ✔ [2025-12-05 19:57:10] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2025-12-05 19:57:10] [OKAY] Pupil epoching completed in 0.11 seconds
#> ℹ [2025-12-05 19:57:10] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2025-12-05 19:57:10] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2025-12-05 19:57:10] [INFO] Only 1 block detected...
#> ℹ [2025-12-05 19:57:10] [INFO] Using run_num = 03 for single block data
#> ℹ [2025-12-05 19:57:10] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2025-12-05 19:57:10] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2025-12-05 19:57:10] [INFO] epoch_prePostProbe:
#> ℹ [2025-12-05 19:57:10] [INFO] block_1: data.frame with 20000 rows
#> ℹ [2025-12-05 19:57:10] [INFO] info: list with 1 elements
#> ! [2025-12-05 19:57:10] [WARN] '/tmp/Rtmpu0YvH8' already exists. Skipping
#> creation...
#> ! [2025-12-05 19:57:10] [WARN] '/tmp/Rtmpu0YvH8/derivatives' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:57:10] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2025-12-05 19:57:10] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2025-12-05 19:57:10] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2025-12-05 19:57:10] [INFO] Writing blinks data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-blinks.csv...
#> ✔ [2025-12-05 19:57:10] [OKAY] Wrote blinks data (1 rows) to CSV
#> ℹ [2025-12-05 19:57:10] [INFO] Writing events data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-events.csv...
#> ✔ [2025-12-05 19:57:10] [OKAY] Wrote events data (67 rows) to CSV
#> ℹ [2025-12-05 19:57:10] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2025-12-05 19:57:10] [INFO] Block block_1 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2025-12-05 19:57:10] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:10] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:57:10] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:10] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:10] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:57:10] [OKAY] Wrote epochs data (20000 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote timeseries data (20767 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote run_confounds data (6 rows) to CSV
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:11] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote epoch_summary data (1 rows) to CSV
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:11] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:11] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:11] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2025-12-05 19:57:11] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ! [2025-12-05 19:57:11] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ℹ [2025-12-05 19:57:11] [INFO]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03' does not
#> exist. Creating...
#> ✔ [2025-12-05 19:57:11] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03'
#> ℹ [2025-12-05 19:57:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:13] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:13] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2025-12-05 19:57:13] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ! [2025-12-05 19:57:13] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03' already
#> exists. Skipping creation...
#> ✔ [2025-12-05 19:57:13] [OKAY] Created gaze heatmap for run-03
#> ! [2025-12-05 19:57:13] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03' already
#> exists. Skipping creation...
#> ℹ [2025-12-05 19:57:13] [INFO]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2025-12-05 19:57:13] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03/epoch_prePostProbe'
#> ✔ [2025-12-05 19:57:21] [OKAY] Created epoch images zip:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03/epoch_prePostProbe/run-03.zip
#> (70 images)
#> ℹ [2025-12-05 19:57:21] [INFO] Using absolute zip file path:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03/epoch_prePostProbe/run-03.zip
#> ✔ [2025-12-05 19:57:21] [OKAY] Embedded zip file as data URL (7969936 bytes)
#> 
#> 
#> processing file: sub-001_epoch-prePostProbe_run-03.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_epoch-prePostProbe_run-03.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001_epoch-prePostProbe_run-03.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_epoch-prePostProbe_run-03.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a714d5e0eff.html 
#> 
#> Output created: sub-001_epoch-prePostProbe_run-03.html
#> ! [2025-12-05 19:57:28] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2025-12-05 19:57:28] [INFO] Removing duplicate plain epoch directory:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-03/epoch_prePostProbe
#> ! [2025-12-05 19:57:28] [WARN] Metadata file already exists for 1:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/logs/run-01_metadata.json
#> ! [2025-12-05 19:57:30] [WARN] No detrend data found for run-03
#> 
#> 
#> processing file: sub-001.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a711951c5e3.html 
#> 
#> Output created: sub-001.html
#> ℹ [2025-12-05 19:57:31] [INFO] Finished BIDSify for sub-001 (Duration: 21.24
#> seconds)

# example with database storage enabled
demo_data |>
  eyeris::glassbox() |>
  eyeris::epoch(
    events = "PROBE_{startstop}_{trial}",
    limits = c(-1, 1),
    label = "prePostProbe"
  ) |>
  eyeris::bidsify(
    bids_dir = tempdir(),
    participant_id = "001",
    session_num = "01",
    task_name = "assocret",
    db_enabled = TRUE,  # enable eyerisdb database storage
    db_path = "my-project"  # custom project database name
  )
#> ✔ [2025-12-05 19:57:31] [OKAY] Running eyeris::load_asc()
#> ℹ [2025-12-05 19:57:32] [INFO] Processing block: block_1
#> ✔ [2025-12-05 19:57:32] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2025-12-05 19:57:32] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2025-12-05 19:57:32] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2025-12-05 19:57:32] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2025-12-05 19:57:32] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2025-12-05 19:57:32] [WARN] Skipping eyeris::bin() for block_1
#> ! [2025-12-05 19:57:32] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2025-12-05 19:57:32] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2025-12-05 19:57:32] [INFO] Block processing summary:
#> ℹ [2025-12-05 19:57:32] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2025-12-05 19:57:32] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2025-12-05 19:57:32] [INFO] Epoching pupil data...
#> ℹ [2025-12-05 19:57:32] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2025-12-05 19:57:32] [OKAY] Done!
#> ✔ [2025-12-05 19:57:32] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2025-12-05 19:57:32] [OKAY] Pupil epoching completed in 0.11 seconds
#> ℹ [2025-12-05 19:57:32] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2025-12-05 19:57:32] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2025-12-05 19:57:32] [INFO] Only 1 block detected...
#> ℹ [2025-12-05 19:57:32] [INFO] Using run_num = 1 for single block data
#> ℹ [2025-12-05 19:57:32] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2025-12-05 19:57:32] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2025-12-05 19:57:32] [INFO] Parallel processing detected for job unknown
#> (PID: 6769), using temporary database
#> ✔ [2025-12-05 19:57:32] [OKAY] Created temporary database:
#> /tmp/Rtmpu0YvH8/derivatives/my-project_temp_6769_20251205_195732_758.eyerisdb
#> ℹ [2025-12-05 19:57:32] [INFO] epoch_prePostProbe:
#> ℹ [2025-12-05 19:57:32] [INFO] block_1: data.frame with 20000 rows
#> ℹ [2025-12-05 19:57:32] [INFO] info: list with 1 elements
#> ! [2025-12-05 19:57:32] [WARN] '/tmp/Rtmpu0YvH8' already exists. Skipping
#> creation...
#> ! [2025-12-05 19:57:32] [WARN] '/tmp/Rtmpu0YvH8/derivatives' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:57:32] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2025-12-05 19:57:32] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2025-12-05 19:57:32] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2025-12-05 19:57:32] [INFO] Writing blinks data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2025-12-05 19:57:32] [OKAY] Wrote blinks data (1 rows) to CSV and database
#> ℹ [2025-12-05 19:57:32] [INFO] Writing events data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2025-12-05 19:57:32] [OKAY] Wrote events data (67 rows) to CSV and database
#> ℹ [2025-12-05 19:57:32] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2025-12-05 19:57:32] [INFO] Block block_1 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2025-12-05 19:57:32] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:32] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote epochs data (20000 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote timeseries data (20767 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote run_confounds data (6 rows) to CSV and
#> database
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:33] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote epoch_summary data (1 rows) to CSV and
#> database
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:33] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2025-12-05 19:57:33] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2025-12-05 19:57:33] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2025-12-05 19:57:33] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ! [2025-12-05 19:57:33] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:57:33] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2025-12-05 19:57:36] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ✔ [2025-12-05 19:57:36] [OKAY] Created gaze heatmap for run-01
#> ! [2025-12-05 19:57:36] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ℹ [2025-12-05 19:57:36] [INFO]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2025-12-05 19:57:36] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe'
#> ✔ [2025-12-05 19:57:43] [OKAY] Created epoch images zip:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe/run-01.zip
#> (70 images)
#> ℹ [2025-12-05 19:57:43] [INFO] Using absolute zip file path:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe/run-01.zip
#> ✔ [2025-12-05 19:57:43] [OKAY] Embedded zip file as data URL (7903929 bytes)
#> 
#> 
#> processing file: sub-001_epoch-prePostProbe_run-01.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_epoch-prePostProbe_run-01.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001_epoch-prePostProbe_run-01.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_epoch-prePostProbe_run-01.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a714d7e4d50.html 
#> 
#> Output created: sub-001_epoch-prePostProbe_run-01.html
#> ! [2025-12-05 19:57:51] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2025-12-05 19:57:51] [INFO] Removing duplicate plain epoch directory:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01/epoch_prePostProbe
#> ! [2025-12-05 19:57:51] [WARN] Metadata file already exists for 1:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/logs/run-01_metadata.json
#> ! [2025-12-05 19:57:51] [WARN] Metadata file already exists for 3:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/logs/run-03_metadata.json
#> ! [2025-12-05 19:57:51] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a7143fb73c9.html 
#> 
#> Output created: sub-001.html
#> ℹ [2025-12-05 19:57:52] [INFO] Merging temporary database from job unknown
#> (PID: 6769) into main database
#> ℹ [2025-12-05 19:57:52] [INFO] Merging 8 tables from temporary database to main
#> database
#> ℹ [2025-12-05 19:57:52] [INFO] Created new table 'blinks_001_01_assocret_run01'
#> with 1 rows
#> ℹ [2025-12-05 19:57:52] [INFO] Created new table
#> 'confounds_events_001_01_assocret_run01_prepostprobe' with 60 rows
#> ℹ [2025-12-05 19:57:52] [INFO] Created new table
#> 'confounds_summary_001_01_assocret_run01_prepostprobe' with 10 rows
#> ℹ [2025-12-05 19:57:53] [INFO] Created new table
#> 'epoch_summary_001_01_assocret_run01' with 1 rows
#> ℹ [2025-12-05 19:57:53] [INFO] Created new table
#> 'epochs_001_01_assocret_run01_prepostprobe' with 20000 rows
#> ℹ [2025-12-05 19:57:53] [INFO] Created new table 'events_001_01_assocret_run01'
#> with 67 rows
#> ℹ [2025-12-05 19:57:53] [INFO] Created new table
#> 'run_confounds_001_01_assocret_run01' with 6 rows
#> ℹ [2025-12-05 19:57:53] [INFO] Created new table
#> 'timeseries_001_01_assocret_run01' with 20767 rows
#> ✔ [2025-12-05 19:57:53] [OKAY] Successfully merged 8/8 tables
#> ✔ [2025-12-05 19:57:53] [OKAY] Successfully merged job unknown (PID: 6769) data
#> into main database
#> ℹ [2025-12-05 19:57:53] [INFO] Disconnected from temporary database
#> ✔ [2025-12-05 19:57:53] [OKAY] Cleaned up temporary database file
#> ℹ [2025-12-05 19:57:53] [INFO] Finished BIDSify for sub-001 (Duration: 20.99
#> seconds)

# example for large-scale cloud compute (database only, no CSV files)
demo_data |>
  eyeris::glassbox() |>
  eyeris::bidsify(
    bids_dir = tempdir(),
    participant_id = "001",
    session_num = "01",
    task_name = "assocret",
    csv_enabled = FALSE,  # disable CSV files
    db_enabled = TRUE     # database storage only
  )
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::load_asc()
#> ℹ [2025-12-05 19:57:53] [INFO] Processing block: block_1
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2025-12-05 19:57:53] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2025-12-05 19:57:53] [WARN] Skipping eyeris::bin() for block_1
#> ! [2025-12-05 19:57:53] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2025-12-05 19:57:53] [INFO] Block processing summary:
#> ℹ [2025-12-05 19:57:53] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2025-12-05 19:57:53] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2025-12-05 19:57:54] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2025-12-05 19:57:54] [INFO] Only 1 block detected...
#> ℹ [2025-12-05 19:57:54] [INFO] Using run_num = 1 for single block data
#> ℹ [2025-12-05 19:57:54] [INFO] Parallel processing detected for job unknown
#> (PID: 6769), using temporary database
#> ✔ [2025-12-05 19:57:54] [OKAY] Created temporary database:
#> /tmp/Rtmpu0YvH8/derivatives/my-project_temp_6769_20251205_195754_013.eyerisdb
#> ! [2025-12-05 19:57:54] [WARN] '/tmp/Rtmpu0YvH8' already exists. Skipping
#> creation...
#> ! [2025-12-05 19:57:54] [WARN] '/tmp/Rtmpu0YvH8/derivatives' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:57:54] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2025-12-05 19:57:54] [WARN] '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ℹ [2025-12-05 19:57:54] [INFO] Writing blinks data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2025-12-05 19:57:54] [OKAY] Wrote blinks data (1 rows) to database
#> ℹ [2025-12-05 19:57:54] [INFO] Writing events data to
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2025-12-05 19:57:54] [OKAY] Wrote events data (67 rows) to database
#> ✔ [2025-12-05 19:57:54] [OKAY] Wrote timeseries data (20767 rows) to database
#> ✔ [2025-12-05 19:57:54] [OKAY] Wrote run_confounds data (6 rows) to database
#> ! [2025-12-05 19:57:54] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2025-12-05 19:57:54] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ℹ [2025-12-05 19:57:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2025-12-05 19:57:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2025-12-05 19:57:56] [WARN]
#> '/tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/figures/run-01' already
#> exists. Skipping creation...
#> ✔ [2025-12-05 19:57:56] [OKAY] Created gaze heatmap for run-01
#> ! [2025-12-05 19:57:56] [WARN] Metadata file already exists for 1:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/logs/run-01_metadata.json
#> ! [2025-12-05 19:57:56] [WARN] Metadata file already exists for 3:
#> /tmp/Rtmpu0YvH8/derivatives/sub-001/ses-01/source/logs/run-03_metadata.json
#> ! [2025-12-05 19:57:56] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS sub-001.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpu0YvH8/rmarkdown-str1a718d1728c.html 
#> 
#> Output created: sub-001.html
#> ℹ [2025-12-05 19:57:58] [INFO] Merging temporary database from job unknown
#> (PID: 6769) into main database
#> ℹ [2025-12-05 19:57:58] [INFO] Merging 4 tables from temporary database to main
#> database
#> ℹ [2025-12-05 19:57:58] [INFO] Merged 1 rows into existing table
#> 'blinks_001_01_assocret_run01'
#> ℹ [2025-12-05 19:57:58] [INFO] Merged 67 rows into existing table
#> 'events_001_01_assocret_run01'
#> ℹ [2025-12-05 19:57:58] [INFO] Merged 6 rows into existing table
#> 'run_confounds_001_01_assocret_run01'
#> ℹ [2025-12-05 19:57:58] [INFO] Merged 20767 rows into existing table
#> 'timeseries_001_01_assocret_run01'
#> ✔ [2025-12-05 19:57:58] [OKAY] Successfully merged 4/4 tables
#> ✔ [2025-12-05 19:57:58] [OKAY] Successfully merged job unknown (PID: 6769) data
#> into main database
#> ℹ [2025-12-05 19:57:58] [INFO] Disconnected from temporary database
#> ✔ [2025-12-05 19:57:58] [OKAY] Cleaned up temporary database file
#> ℹ [2025-12-05 19:57:58] [INFO] Finished BIDSify for sub-001 (Duration: 4.63
#> seconds)
# }
```
