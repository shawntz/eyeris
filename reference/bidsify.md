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
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

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
  [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md)
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
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::resample()
#> ℹ [2026-08-13 18:01:13] [INFO] Processing block: block_1
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-08-13 18:01:13] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-08-13 18:01:13] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-08-13 18:01:13] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-08-13 18:01:13] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-08-13 18:01:13] [INFO] Block processing summary:
#> ℹ [2026-08-13 18:01:13] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-08-13 18:01:13] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-08-13 18:01:13] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-08-13 18:01:13] [INFO] Only 1 block detected...
#> ℹ [2026-08-13 18:01:13] [INFO] Using run_num = 01 for single block data
#> ! [2026-08-13 18:01:13] [WARN] '/tmp/RtmpfyHCi2' already exists. Skipping
#> creation...
#> ℹ [2026-08-13 18:01:13] [INFO] '/tmp/RtmpfyHCi2/derivatives' does not exist.
#> Creating...
#> ✔ [2026-08-13 18:01:14] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives'
#> ℹ [2026-08-13 18:01:14] [INFO] '/tmp/RtmpfyHCi2/derivatives/sub-001' does not
#> exist. Creating...
#> ✔ [2026-08-13 18:01:14] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001'
#> ℹ [2026-08-13 18:01:14] [INFO] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:01:14] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01'
#> ℹ [2026-08-13 18:01:14] [INFO] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:01:14] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye'
#> ℹ [2026-08-13 18:01:14] [INFO] Writing blinks data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2026-08-13 18:01:14] [OKAY] Wrote blinks data (1 rows) to CSV
#> ℹ [2026-08-13 18:01:14] [INFO] Writing events data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2026-08-13 18:01:14] [OKAY] Wrote events data (67 rows) to CSV
#> ✔ [2026-08-13 18:01:14] [OKAY] Wrote timeseries data (20767 rows) to CSV
#> ✔ [2026-08-13 18:01:14] [OKAY] Wrote run_confounds data (6 rows) to CSV
#> ℹ [2026-08-13 18:01:14] [INFO]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures' does not exist.
#> Creating...
#> ✔ [2026-08-13 18:01:14] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures'
#> ℹ [2026-08-13 18:01:14] [INFO]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:01:14] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> ℹ [2026-08-13 18:01:14] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:15] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:16] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:16] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:17] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:17] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:18] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:19] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:19] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:20] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:21] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:21] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:22] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:22] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:22] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:23] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:23] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:23] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2026-08-13 18:01:23] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ✔ [2026-08-13 18:01:24] [OKAY] Created gaze heatmap for run-01
#> ! [2026-08-13 18:01:26] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a464086ae.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-08-13 18:01:28] [INFO] Finished BIDSify for sub-001 (Duration: 14.66
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
#> ✔ [2026-08-13 18:01:28] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-08-13 18:01:28] [OKAY] Running eyeris::resample()
#> ℹ [2026-08-13 18:01:28] [INFO] Processing block: block_1
#> ✔ [2026-08-13 18:01:28] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-08-13 18:01:28] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-08-13 18:01:28] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-08-13 18:01:28] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-08-13 18:01:29] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2026-08-13 18:01:29] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-08-13 18:01:29] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-08-13 18:01:29] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-08-13 18:01:29] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-08-13 18:01:29] [INFO] Block processing summary:
#> ℹ [2026-08-13 18:01:29] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-08-13 18:01:29] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-08-13 18:01:29] [INFO] Epoching pupil data...
#> ℹ [2026-08-13 18:01:29] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2026-08-13 18:01:29] [OKAY] Done!
#> ✔ [2026-08-13 18:01:29] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-08-13 18:01:29] [OKAY] Pupil epoching completed in 0.18 seconds
#> ℹ [2026-08-13 18:01:29] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2026-08-13 18:01:29] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-08-13 18:01:29] [INFO] Only 1 block detected...
#> ℹ [2026-08-13 18:01:29] [INFO] Using run_num = 01 for single block data
#> ℹ [2026-08-13 18:01:29] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2026-08-13 18:01:29] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2026-08-13 18:01:29] [INFO] epoch_prePostProbe:
#> ℹ [2026-08-13 18:01:29] [INFO] block_1: data.frame with 20000 rows
#> ℹ [2026-08-13 18:01:29] [INFO] info: list with 1 elements
#> ! [2026-08-13 18:01:29] [WARN] '/tmp/RtmpfyHCi2' already exists. Skipping
#> creation...
#> ! [2026-08-13 18:01:29] [WARN] '/tmp/RtmpfyHCi2/derivatives' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:01:29] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2026-08-13 18:01:29] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2026-08-13 18:01:29] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:01:29] [INFO] Writing blinks data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote blinks data (1 rows) to CSV
#> ℹ [2026-08-13 18:01:29] [INFO] Writing events data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote events data (67 rows) to CSV
#> ℹ [2026-08-13 18:01:29] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2026-08-13 18:01:29] [INFO] Block block_1 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:01:29] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote epochs data (20000 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote timeseries data (20767 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote run_confounds data (6 rows) to CSV
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:01:29] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote epoch_summary data (1 rows) to CSV
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:01:29] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:01:29] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:01:29] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:01:29] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:01:30] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ! [2026-08-13 18:01:30] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:01:30] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:01:30] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:30] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:31] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:31] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:32] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:32] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:33] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:33] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:36] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:36] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:37] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:37] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:37] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:38] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:01:38] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2026-08-13 18:01:38] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ✔ [2026-08-13 18:01:39] [OKAY] Created gaze heatmap for run-01
#> ! [2026-08-13 18:01:39] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:01:39] [INFO]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:01:39] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe'
#> ✔ [2026-08-13 18:01:54] [OKAY] Created epoch images zip:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe/task-assocret_run-01.zip
#> (70 images)
#> ℹ [2026-08-13 18:01:54] [INFO] Using absolute zip file path:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe/task-assocret_run-01.zip
#> ✔ [2026-08-13 18:01:54] [OKAY] Embedded zip file as data URL (11696718 bytes)
#> 
#> 
#> processing file: sub-001_task-assocret_epoch-prePostProbe_run-01.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret_epoch-prePostProbe_run-01.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret_epoch-prePostProbe_run-01.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret_epoch-prePostProbe_run-01.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a265fcaa.html 
#> 
#> Output created: sub-001_task-assocret_epoch-prePostProbe_run-01.html
#> ! [2026-08-13 18:02:05] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2026-08-13 18:02:05] [INFO] Removing duplicate plain epoch directory:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe
#> ! [2026-08-13 18:02:05] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a4711805e.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-08-13 18:02:07] [INFO] Finished BIDSify for sub-001 (Duration: 37.5
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
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::resample()
#> ℹ [2026-08-13 18:02:07] [INFO] Processing block: block_1
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-08-13 18:02:07] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2026-08-13 18:02:07] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-08-13 18:02:07] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-08-13 18:02:07] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-08-13 18:02:07] [INFO] Block processing summary:
#> ℹ [2026-08-13 18:02:07] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-08-13 18:02:07] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-08-13 18:02:07] [INFO] Epoching pupil data...
#> ℹ [2026-08-13 18:02:07] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2026-08-13 18:02:07] [OKAY] Done!
#> ✔ [2026-08-13 18:02:07] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-08-13 18:02:07] [OKAY] Pupil epoching completed in 0.11 seconds
#> ℹ [2026-08-13 18:02:07] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2026-08-13 18:02:07] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-08-13 18:02:07] [INFO] Only 1 block detected...
#> ℹ [2026-08-13 18:02:07] [INFO] Using run_num = 03 for single block data
#> ℹ [2026-08-13 18:02:07] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2026-08-13 18:02:07] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2026-08-13 18:02:07] [INFO] epoch_prePostProbe:
#> ℹ [2026-08-13 18:02:07] [INFO] block_3: data.frame with 20000 rows
#> ℹ [2026-08-13 18:02:07] [INFO] info: list with 1 elements
#> ! [2026-08-13 18:02:07] [WARN] '/tmp/RtmpfyHCi2' already exists. Skipping
#> creation...
#> ! [2026-08-13 18:02:07] [WARN] '/tmp/RtmpfyHCi2/derivatives' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:02:07] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2026-08-13 18:02:07] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2026-08-13 18:02:07] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:02:07] [INFO] Writing blinks data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-blinks.csv...
#> ✔ [2026-08-13 18:02:07] [OKAY] Wrote blinks data (1 rows) to CSV
#> ℹ [2026-08-13 18:02:07] [INFO] Writing events data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-events.csv...
#> ✔ [2026-08-13 18:02:07] [OKAY] Wrote events data (67 rows) to CSV
#> ℹ [2026-08-13 18:02:07] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2026-08-13 18:02:07] [INFO] Block block_3 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2026-08-13 18:02:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:07] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:02:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:02:07] [OKAY] Wrote epochs data (20000 rows) to CSV
#> ✔ [2026-08-13 18:02:07] [OKAY] Wrote timeseries data (20767 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote run_confounds data (6 rows) to CSV
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:08] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote epoch_summary data (1 rows) to CSV
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:08] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_summary data (1 rows) to CSV
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:08] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:08] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ✔ [2026-08-13 18:02:08] [OKAY] Wrote confounds_events data (6 rows) to CSV
#> ! [2026-08-13 18:02:08] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ℹ [2026-08-13 18:02:08] [INFO]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:02:08] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> ℹ [2026-08-13 18:02:08] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:08] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:09] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:09] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:10] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:10] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:13] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:14] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:14] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:15] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:15] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:15] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:15] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-08-13 18:02:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ! [2026-08-13 18:02:16] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ✔ [2026-08-13 18:02:16] [OKAY] Created gaze heatmap for run-03
#> ! [2026-08-13 18:02:16] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:02:16] [INFO]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:02:16] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe'
#> ✔ [2026-08-13 18:02:32] [OKAY] Created epoch images zip:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe/task-assocret_run-03.zip
#> (70 images)
#> ℹ [2026-08-13 18:02:32] [INFO] Using absolute zip file path:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe/task-assocret_run-03.zip
#> ✔ [2026-08-13 18:02:32] [OKAY] Embedded zip file as data URL (11755799 bytes)
#> 
#> 
#> processing file: sub-001_task-assocret_epoch-prePostProbe_run-03.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret_epoch-prePostProbe_run-03.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret_epoch-prePostProbe_run-03.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret_epoch-prePostProbe_run-03.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a6260fbe1.html 
#> 
#> Output created: sub-001_task-assocret_epoch-prePostProbe_run-03.html
#> ! [2026-08-13 18:02:43] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2026-08-13 18:02:43] [INFO] Removing duplicate plain epoch directory:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe
#> ! [2026-08-13 18:02:45] [WARN] No detrend data found for run-03
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a91a128.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-08-13 18:02:47] [INFO] Finished BIDSify for sub-001 (Duration: 39.53
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
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::resample()
#> ℹ [2026-08-13 18:02:47] [INFO] Processing block: block_1
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-08-13 18:02:47] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2026-08-13 18:02:47] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-08-13 18:02:47] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-08-13 18:02:47] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-08-13 18:02:47] [INFO] Block processing summary:
#> ℹ [2026-08-13 18:02:47] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-08-13 18:02:47] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-08-13 18:02:47] [INFO] Epoching pupil data...
#> ℹ [2026-08-13 18:02:47] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2026-08-13 18:02:47] [OKAY] Done!
#> ✔ [2026-08-13 18:02:47] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-08-13 18:02:47] [OKAY] Pupil epoching completed in 0.11 seconds
#> ℹ [2026-08-13 18:02:47] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2026-08-13 18:02:47] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-08-13 18:02:47] [INFO] Only 1 block detected...
#> ℹ [2026-08-13 18:02:47] [INFO] Using run_num = 1 for single block data
#> ℹ [2026-08-13 18:02:47] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2026-08-13 18:02:47] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2026-08-13 18:02:47] [INFO] Parallel processing detected for job unknown
#> (PID: 7226), using temporary database
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpfyHCi2/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> ✔ [2026-08-13 18:02:47] [OKAY] Created temporary database:
#> /tmp/RtmpfyHCi2/derivatives/my-project_temp_7226_20260813_180247_685.eyerisdb
#> ℹ [2026-08-13 18:02:47] [INFO] epoch_prePostProbe:
#> ℹ [2026-08-13 18:02:47] [INFO] block_1: data.frame with 20000 rows
#> ℹ [2026-08-13 18:02:47] [INFO] info: list with 1 elements
#> ! [2026-08-13 18:02:47] [WARN] '/tmp/RtmpfyHCi2' already exists. Skipping
#> creation...
#> ! [2026-08-13 18:02:47] [WARN] '/tmp/RtmpfyHCi2/derivatives' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:02:47] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2026-08-13 18:02:47] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2026-08-13 18:02:47] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:02:47] [INFO] Writing blinks data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2026-08-13 18:02:47] [OKAY] Wrote blinks data (1 rows) to CSV and database
#> ℹ [2026-08-13 18:02:47] [INFO] Writing events data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2026-08-13 18:02:47] [OKAY] Wrote events data (67 rows) to CSV and database
#> ℹ [2026-08-13 18:02:47] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2026-08-13 18:02:47] [INFO] Block block_1 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2026-08-13 18:02:47] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:47] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:02:47] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:47] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:47] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote epochs data (20000 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote timeseries data (20767 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote run_confounds data (6 rows) to CSV and
#> database
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:48] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote epoch_summary data (1 rows) to CSV and
#> database
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:48] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-08-13 18:02:48] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-08-13 18:02:48] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:48] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-08-13 18:02:49] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ! [2026-08-13 18:02:49] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:02:49] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:02:49] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:49] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:50] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:50] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:51] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:53] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:53] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:54] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:55] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:56] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:57] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:02:57] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2026-08-13 18:02:57] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ✔ [2026-08-13 18:02:57] [OKAY] Created gaze heatmap for run-01
#> ! [2026-08-13 18:02:57] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:02:57] [INFO]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2026-08-13 18:02:57] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe'
#> ✔ [2026-08-13 18:03:13] [OKAY] Created epoch images zip:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe/task-assocret_run-01.zip
#> (70 images)
#> ℹ [2026-08-13 18:03:13] [INFO] Using absolute zip file path:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe/task-assocret_run-01.zip
#> ✔ [2026-08-13 18:03:13] [OKAY] Embedded zip file as data URL (11696718 bytes)
#> 
#> 
#> processing file: sub-001_task-assocret_epoch-prePostProbe_run-01.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret_epoch-prePostProbe_run-01.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret_epoch-prePostProbe_run-01.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret_epoch-prePostProbe_run-01.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3ab1e2f2a.html 
#> 
#> Output created: sub-001_task-assocret_epoch-prePostProbe_run-01.html
#> ! [2026-08-13 18:03:23] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2026-08-13 18:03:23] [INFO] Removing duplicate plain epoch directory:
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01/epoch_prePostProbe
#> ! [2026-08-13 18:03:24] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a2149bb19.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-08-13 18:03:25] [INFO] Merging temporary database from job unknown
#> (PID: 7226) into main database
#> ℹ [2026-08-13 18:03:25] [INFO] Merging 8 tables from temporary database to main
#> database
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpfyHCi2/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> ℹ [2026-08-13 18:03:25] [INFO] Created new table 'blinks_001_01_assocret_run01'
#> with 1 rows
#> ℹ [2026-08-13 18:03:25] [INFO] Created new table
#> 'confounds_events_001_01_assocret_run01_prepostprobe' with 60 rows
#> ℹ [2026-08-13 18:03:25] [INFO] Created new table
#> 'confounds_summary_001_01_assocret_run01_prepostprobe' with 10 rows
#> ℹ [2026-08-13 18:03:26] [INFO] Created new table
#> 'epoch_summary_001_01_assocret_run01' with 1 rows
#> ℹ [2026-08-13 18:03:26] [INFO] Created new table
#> 'epochs_001_01_assocret_run01_prepostprobe' with 20000 rows
#> ℹ [2026-08-13 18:03:26] [INFO] Created new table 'events_001_01_assocret_run01'
#> with 67 rows
#> ℹ [2026-08-13 18:03:26] [INFO] Created new table
#> 'run_confounds_001_01_assocret_run01' with 6 rows
#> ℹ [2026-08-13 18:03:26] [INFO] Created new table
#> 'timeseries_001_01_assocret_run01' with 20767 rows
#> ✔ [2026-08-13 18:03:26] [OKAY] Successfully merged 8/8 tables
#> ✔ [2026-08-13 18:03:26] [OKAY] Successfully merged job unknown (PID: 7226) data
#> into main database
#> ℹ [2026-08-13 18:03:26] [INFO] Disconnected from temporary database
#> ✔ [2026-08-13 18:03:26] [OKAY] Cleaned up temporary database file
#> ℹ [2026-08-13 18:03:26] [INFO] Finished BIDSify for sub-001 (Duration: 39.13
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
#> ✔ [2026-08-13 18:03:26] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-08-13 18:03:26] [OKAY] Running eyeris::resample()
#> ℹ [2026-08-13 18:03:26] [INFO] Processing block: block_1
#> ✔ [2026-08-13 18:03:26] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-08-13 18:03:26] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-08-13 18:03:26] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-08-13 18:03:26] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-08-13 18:03:26] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2026-08-13 18:03:27] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-08-13 18:03:27] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-08-13 18:03:27] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-08-13 18:03:27] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-08-13 18:03:27] [INFO] Block processing summary:
#> ℹ [2026-08-13 18:03:27] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-08-13 18:03:27] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-08-13 18:03:27] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-08-13 18:03:27] [INFO] Only 1 block detected...
#> ℹ [2026-08-13 18:03:27] [INFO] Using run_num = 1 for single block data
#> ℹ [2026-08-13 18:03:27] [INFO] Parallel processing detected for job unknown
#> (PID: 7226), using temporary database
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpfyHCi2/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> ✔ [2026-08-13 18:03:27] [OKAY] Created temporary database:
#> /tmp/RtmpfyHCi2/derivatives/my-project_temp_7226_20260813_180327_076.eyerisdb
#> ! [2026-08-13 18:03:27] [WARN] '/tmp/RtmpfyHCi2' already exists. Skipping
#> creation...
#> ! [2026-08-13 18:03:27] [WARN] '/tmp/RtmpfyHCi2/derivatives' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:03:27] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2026-08-13 18:03:27] [WARN] '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:03:27] [INFO] Writing blinks data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/sub-001_ses-01_task-assocret_run-01_desc-blinks.csv...
#> ✔ [2026-08-13 18:03:27] [OKAY] Wrote blinks data (1 rows) to database
#> ℹ [2026-08-13 18:03:27] [INFO] Writing events data to
#> /tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/sub-001_ses-01_task-assocret_run-01_desc-events.csv...
#> ✔ [2026-08-13 18:03:27] [OKAY] Wrote events data (67 rows) to database
#> ✔ [2026-08-13 18:03:27] [OKAY] Wrote timeseries data (20767 rows) to database
#> ✔ [2026-08-13 18:03:27] [OKAY] Wrote run_confounds data (6 rows) to database
#> ! [2026-08-13 18:03:27] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2026-08-13 18:03:27] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ℹ [2026-08-13 18:03:27] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:27] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:28] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:29] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:29] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:30] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:30] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:31] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:32] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:32] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:33] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:33] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:34] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ℹ [2026-08-13 18:03:35] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
#> ! [2026-08-13 18:03:35] [WARN]
#> '/tmp/RtmpfyHCi2/derivatives/sub-001/ses-01/source/figures/task-assocret_run-01'
#> already exists. Skipping creation...
#> ✔ [2026-08-13 18:03:36] [OKAY] Created gaze heatmap for run-01
#> ! [2026-08-13 18:03:36] [WARN] No detrend data found for run-01
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpfyHCi2/rmarkdown-str1c3a647d07f.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-08-13 18:03:37] [INFO] Merging temporary database from job unknown
#> (PID: 7226) into main database
#> ℹ [2026-08-13 18:03:37] [INFO] Merging 4 tables from temporary database to main
#> database
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpfyHCi2/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> ℹ [2026-08-13 18:03:37] [INFO] Merged 1 rows into existing table
#> 'blinks_001_01_assocret_run01'
#> ℹ [2026-08-13 18:03:37] [INFO] Merged 67 rows into existing table
#> 'events_001_01_assocret_run01'
#> ℹ [2026-08-13 18:03:37] [INFO] Merged 6 rows into existing table
#> 'run_confounds_001_01_assocret_run01'
#> ℹ [2026-08-13 18:03:37] [INFO] Merged 20767 rows into existing table
#> 'timeseries_001_01_assocret_run01'
#> ✔ [2026-08-13 18:03:37] [OKAY] Successfully merged 4/4 tables
#> ✔ [2026-08-13 18:03:37] [OKAY] Successfully merged job unknown (PID: 7226) data
#> into main database
#> ℹ [2026-08-13 18:03:37] [INFO] Disconnected from temporary database
#> ✔ [2026-08-13 18:03:37] [OKAY] Cleaned up temporary database file
#> ℹ [2026-08-13 18:03:37] [INFO] Finished BIDSify for sub-001 (Duration: 10.88
#> seconds)
# }
```
