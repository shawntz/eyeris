# Extract and aggregate eyeris data across subjects from database

A comprehensive wrapper function that simplifies extracting `eyeris`
data from the database. Provides easy one-liner access to aggregate data
across multiple subjects for each data type, without requiring SQL
knowledge.

## Usage

``` r
eyeris_db_collect(
  bids_dir,
  db_path = "my-project",
  subjects = NULL,
  data_types = NULL,
  sessions = NULL,
  tasks = NULL,
  epoch_labels = NULL,
  eye_suffixes = NULL,
  verbose = TRUE
)
```

## Arguments

- bids_dir:

  Path to the BIDS directory containing the database

- db_path:

  Database name (defaults to "my-project", becomes
  "my-project.eyerisdb")

- subjects:

  Vector of subject IDs to include. If NULL (default), includes all
  subjects

- data_types:

  Vector of data types to extract. If NULL (default), extracts all
  available types. Valid types: "blinks", "events", "timeseries",
  "epochs", "epoch_summary", "run_confounds", "confounds_events",
  "confounds_summary"

- sessions:

  Vector of session IDs to include. If NULL (default), includes all
  sessions

- tasks:

  Vector of task names to include. If NULL (default), includes all tasks

- epoch_labels:

  Vector of epoch labels to include. If NULL (default), includes all
  epochs. Only applies to epoch-related data types

- eye_suffixes:

  Vector of eye suffixes to include. If NULL (default), includes all
  eyes. Typically c("eye-L", "eye-R") for binocular data

- verbose:

  Logical. Whether to print progress messages (default TRUE)

## Value

A named list of data frames, one per data type

## Examples

``` r
# \donttest{
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
  run_num = "03", # override default run-01 (block_1) to use run-03 instead
  db_enabled = TRUE # enable database storage
)
#> ✔ [2026-07-18 21:22:05] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-07-18 21:22:05] [INFO] Processing block: block_1
#> ✔ [2026-07-18 21:22:05] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-18 21:22:05] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-18 21:22:05] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-07-18 21:22:05] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-07-18 21:22:05] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-18 21:22:05] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-18 21:22:06] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-07-18 21:22:06] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-18 21:22:06] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-18 21:22:06] [INFO] Block processing summary:
#> ℹ [2026-07-18 21:22:06] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-07-18 21:22:06] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-18 21:22:06] [INFO] Epoching pupil data...
#> ℹ [2026-07-18 21:22:06] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2026-07-18 21:22:06] [OKAY] Done!
#> ✔ [2026-07-18 21:22:06] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-07-18 21:22:06] [OKAY] Pupil epoching completed in 0.12 seconds
#> ℹ [2026-07-18 21:22:06] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2026-07-18 21:22:06] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-07-18 21:22:06] [INFO] Only 1 block detected...
#> ℹ [2026-07-18 21:22:06] [INFO] Using run_num = 03 for single block data
#> ℹ [2026-07-18 21:22:06] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2026-07-18 21:22:06] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2026-07-18 21:22:06] [INFO] Parallel processing detected for job unknown
#> (PID: 6587), using temporary database
#> ✔ [2026-07-18 21:22:06] [OKAY] Created temporary database:
#> /tmp/Rtmp7KMyrS/derivatives/my-project_temp_6587_20260718_212206_450.eyerisdb
#> ℹ [2026-07-18 21:22:06] [INFO] epoch_prePostProbe:
#> ℹ [2026-07-18 21:22:06] [INFO] block_3: data.frame with 20000 rows
#> ℹ [2026-07-18 21:22:06] [INFO] info: list with 1 elements
#> ! [2026-07-18 21:22:06] [WARN] '/tmp/Rtmp7KMyrS' already exists. Skipping
#> creation...
#> ! [2026-07-18 21:22:06] [WARN] '/tmp/Rtmp7KMyrS/derivatives' already exists.
#> Skipping creation...
#> ! [2026-07-18 21:22:06] [WARN] '/tmp/Rtmp7KMyrS/derivatives/sub-001' already
#> exists. Skipping creation...
#> ! [2026-07-18 21:22:06] [WARN] '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01'
#> already exists. Skipping creation...
#> ! [2026-07-18 21:22:06] [WARN] '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/eye'
#> already exists. Skipping creation...
#> ℹ [2026-07-18 21:22:06] [INFO] Writing blinks data to
#> /tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-blinks.csv...
#> ✔ [2026-07-18 21:22:06] [OKAY] Wrote blinks data (1 rows) to CSV and database
#> ℹ [2026-07-18 21:22:06] [INFO] Writing events data to
#> /tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-events.csv...
#> ✔ [2026-07-18 21:22:06] [OKAY] Wrote events data (67 rows) to CSV and database
#> ℹ [2026-07-18 21:22:06] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2026-07-18 21:22:06] [INFO] Block block_3 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2026-07-18 21:22:06] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-07-18 21:22:06] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-07-18 21:22:06] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-07-18 21:22:06] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-07-18 21:22:06] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-07-18 21:22:06] [OKAY] Wrote epochs data (20000 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote timeseries data (20767 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote run_confounds data (6 rows) to CSV and
#> database
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-07-18 21:22:07] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote epoch_summary data (1 rows) to CSV and
#> database
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-07-18 21:22:07] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-07-18 21:22:07] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-07-18 21:22:07] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-07-18 21:22:07] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ! [2026-07-18 21:22:07] [WARN]
#> '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures' already exists.
#> Skipping creation...
#> ! [2026-07-18 21:22:07] [WARN]
#> '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ℹ [2026-07-18 21:22:07] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:08] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:09] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:09] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:10] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:11] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:12] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:13] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:14] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:14] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:15] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:16] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-07-18 21:22:17] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ! [2026-07-18 21:22:17] [WARN]
#> '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ✔ [2026-07-18 21:22:17] [OKAY] Created gaze heatmap for run-03
#> ! [2026-07-18 21:22:17] [WARN]
#> '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ℹ [2026-07-18 21:22:17] [INFO]
#> '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2026-07-18 21:22:17] [OKAY] BIDS directory successfully created at:
#> '/tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe'
#> ✔ [2026-07-18 21:22:35] [OKAY] Created epoch images zip:
#> /tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe/task-assocret_run-03.zip
#> (70 images)
#> ℹ [2026-07-18 21:22:35] [INFO] Using absolute zip file path:
#> /tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe/task-assocret_run-03.zip
#> ✔ [2026-07-18 21:22:35] [OKAY] Embedded zip file as data URL (11755799 bytes)
#> 
#> 
#> processing file: sub-001_task-assocret_epoch-prePostProbe_run-03.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret_epoch-prePostProbe_run-03.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret_epoch-prePostProbe_run-03.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret_epoch-prePostProbe_run-03.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmp7KMyrS/rmarkdown-str19bb1cca3e29.html 
#> 
#> Output created: sub-001_task-assocret_epoch-prePostProbe_run-03.html
#> ! [2026-07-18 21:22:45] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2026-07-18 21:22:45] [INFO] Removing duplicate plain epoch directory:
#> /tmp/Rtmp7KMyrS/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe
#> ! [2026-07-18 21:22:45] [WARN] No detrend data found for run-03
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmp7KMyrS/rmarkdown-str19bb634785a8.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-07-18 21:22:47] [INFO] Merging temporary database from job unknown
#> (PID: 6587) into main database
#> ℹ [2026-07-18 21:22:47] [INFO] Merging 8 tables from temporary database to main
#> database
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table 'blinks_001_01_assocret_run03'
#> with 1 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table
#> 'confounds_events_001_01_assocret_run03_prepostprobe' with 60 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table
#> 'confounds_summary_001_01_assocret_run03_prepostprobe' with 10 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table
#> 'epoch_summary_001_01_assocret_run03' with 1 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table
#> 'epochs_001_01_assocret_run03_prepostprobe' with 20000 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table 'events_001_01_assocret_run03'
#> with 67 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table
#> 'run_confounds_001_01_assocret_run03' with 6 rows
#> ℹ [2026-07-18 21:22:47] [INFO] Created new table
#> 'timeseries_001_01_assocret_run03' with 20767 rows
#> ✔ [2026-07-18 21:22:47] [OKAY] Successfully merged 8/8 tables
#> ✔ [2026-07-18 21:22:47] [OKAY] Successfully merged job unknown (PID: 6587) data
#> into main database
#> ℹ [2026-07-18 21:22:48] [INFO] Disconnected from temporary database
#> ✔ [2026-07-18 21:22:48] [OKAY] Cleaned up temporary database file
#> ℹ [2026-07-18 21:22:48] [INFO] Finished BIDSify for sub-001 (Duration: 41.64
#> seconds)

# extract all data for all subjects (returns list of data frames)
all_data <- eyeris_db_collect(tempdir())
#> ℹ [2026-07-18 21:22:48] [INFO] Connecting to eyeris database...
#> ✔ [2026-07-18 21:22:48] [OKAY] Connected to eyeris database:
#> /tmp/Rtmp7KMyrS/derivatives/my-project.eyerisdb
#> ℹ [2026-07-18 21:22:48] [INFO] Found 16 tables in database
#> ℹ [2026-07-18 21:22:48] [INFO] Extracting data types: blinks, events,
#> timeseries, epochs, epoch_summary, run_confounds, confounds_events,
#> confounds_summary
#> ℹ [2026-07-18 21:22:48] [INFO] Processing blinks...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "stime", "etime", "dur", "eye" FROM
#> "blinks_001_01_assocret_run01" UNION ALL SELECT "subject_id", "session_id",
#> "task_name", "data_type", "run_number", "created_timestamp", "block", "stime",
#> "etime", "dur", "eye" FROM "blinks_001_01_assocret_run03") as combined_data
#> WHERE 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing events...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "time", "text", "text_unique" FROM
#> "events_001_01_assocret_run01" UNION ALL SELECT "subject_id", "session_id",
#> "task_name", "data_type", "run_number", "created_timestamp", "block", "time",
#> "text", "text_unique" FROM "events_001_01_assocret_run03") as combined_data
#> WHERE 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing timeseries...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "time_orig", "time_secs", "time_scaled", "eye_x",
#> "eye_y", "eye", "hz", "type", "pupil_raw", "pupil_raw_deblink",
#> "pupil_raw_deblink_detransient", "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z" FROM
#> "timeseries_001_01_assocret_run01" UNION ALL SELECT "subject_id", "session_id",
#> "task_name", "data_type", "run_number", "created_timestamp", "block",
#> "time_orig", "time_secs", "time_scaled", "eye_x", "eye_y", "eye", "hz", "type",
#> "pupil_raw", "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z" FROM
#> "timeseries_001_01_assocret_run03") as combined_data WHERE 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing epochs...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "epoch_label", "created_timestamp", "block", "time_orig", "timebin",
#> "time_secs", "time_scaled", "eye_x", "eye_y", "eye", "hz", "type", "pupil_raw",
#> "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z", "text_unique",
#> "template", "matching_pattern", "matched_event", "startstop", "trial" FROM
#> "epochs_001_01_assocret_run01_prepostprobe" UNION ALL SELECT "subject_id",
#> "session_id", "task_name", "data_type", "run_number", "epoch_label",
#> "created_timestamp", "block", "time_orig", "timebin", "time_secs",
#> "time_scaled", "eye_x", "eye_y", "eye", "hz", "type", "pupil_raw",
#> "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z", "text_unique",
#> "template", "matching_pattern", "matched_event", "startstop", "trial" FROM
#> "epochs_001_01_assocret_run03_prepostprobe") as combined_data WHERE 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing epoch_summary...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "epoch_type", "calc_baseline", "apply_baseline",
#> "baseline_type", "baseline_events", "baseline_period", "epoch_events",
#> "epoch_limits", "n_epochs", "n_baseline_epochs" FROM
#> "epoch_summary_001_01_assocret_run01" UNION ALL SELECT "subject_id",
#> "session_id", "task_name", "data_type", "run_number", "created_timestamp",
#> "epoch_type", "calc_baseline", "apply_baseline", "baseline_type",
#> "baseline_events", "baseline_period", "epoch_events", "epoch_limits",
#> "n_epochs", "n_baseline_epochs" FROM "epoch_summary_001_01_assocret_run03") as
#> combined_data WHERE 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing run_confounds...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "step", "sampling_rate_hz", "total_time_ms",
#> "n_samples", "n_missing", "prop_missing", "n_invalid", "prop_invalid",
#> "n_gaps", "max_gap_n_samples", "max_gap_duration_ms", "min_gap_n_samples",
#> "min_gap_duration_ms", "mean_gap_n_samples", "mean_gap_duration_ms",
#> "screen_width", "screen_height", "gaze_x_var_px", "gaze_y_var_px",
#> "mean_gaze_distance_from_center_px", "mean_gaze_distance_from_center_norm",
#> "prop_clipped", "n_blinks", "blink_rate_hz", "min_blink_duration_ms",
#> "max_blink_duration_ms", "mean_blink_duration_ms", "total_blink_time_ms",
#> "prop_blink_time" FROM "run_confounds_001_01_assocret_run01" UNION ALL SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "step", "sampling_rate_hz", "total_time_ms",
#> "n_samples", "n_missing", "prop_missing", "n_invalid", "prop_invalid",
#> "n_gaps", "max_gap_n_samples", "max_gap_duration_ms", "min_gap_n_samples",
#> "min_gap_duration_ms", "mean_gap_n_samples", "mean_gap_duration_ms",
#> "screen_width", "screen_height", "gaze_x_var_px", "gaze_y_var_px",
#> "mean_gaze_distance_from_center_px", "mean_gaze_distance_from_center_norm",
#> "prop_clipped", "n_blinks", "blink_rate_hz", "min_blink_duration_ms",
#> "max_blink_duration_ms", "mean_blink_duration_ms", "total_blink_time_ms",
#> "prop_blink_time" FROM "run_confounds_001_01_assocret_run03") as combined_data
#> WHERE 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing confounds_events...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "epoch_label", "created_timestamp", "matched_event", "text_unique", "step",
#> "n_samples", "n_missing", "prop_missing", "range", "zscore_max", "zscore_min",
#> "prop_blink_time", "pre_epoch_pupil_sd", "epoch_pupil_sd" FROM
#> "confounds_events_001_01_assocret_run01_prepostprobe" UNION ALL SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "epoch_label", "created_timestamp", "matched_event", "text_unique", "step",
#> "n_samples", "n_missing", "prop_missing", "range", "zscore_max", "zscore_min",
#> "prop_blink_time", "pre_epoch_pupil_sd", "epoch_pupil_sd" FROM
#> "confounds_events_001_01_assocret_run03_prepostprobe") as combined_data WHERE
#> 1=1
#> ℹ [2026-07-18 21:22:48] [INFO] Processing confounds_summary...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "epoch_label", "created_timestamp", "matched_event", "text_unique",
#> "n_samples", "n_blinks_in_baseline", "time_to_first_blink_ms",
#> "epoch_duration_ms" FROM "confounds_summary_001_01_assocret_run01_prepostprobe"
#> UNION ALL SELECT "subject_id", "session_id", "task_name", "data_type",
#> "run_number", "epoch_label", "created_timestamp", "matched_event",
#> "text_unique", "n_samples", "n_blinks_in_baseline", "time_to_first_blink_ms",
#> "epoch_duration_ms" FROM
#> "confounds_summary_001_01_assocret_run03_prepostprobe") as combined_data WHERE
#> 1=1
#> ✔ [2026-07-18 21:22:48] [OKAY] Successfully extracted 8 data types
#> ℹ [2026-07-18 21:22:48] [INFO] blinks: 3 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] events: 201 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] timeseries: 62301 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] epochs: 40000 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] epoch_summary: 2 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] run_confounds: 18 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] confounds_events: 120 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] confounds_summary: 20 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] Disconnected from eyeris database

# view available data types
names(all_data)
#> [1] "blinks"            "events"            "timeseries"       
#> [4] "epochs"            "epoch_summary"     "run_confounds"    
#> [7] "confounds_events"  "confounds_summary"

# access specific data type
blinks_data <- all_data$blinks
epochs_data <- all_data$epochs

# extract specific subjects and data types
subset_data <- eyeris_db_collect(
  bids_dir = tempdir(),
  subjects = c("001"),
  data_types = c("blinks", "epochs", "timeseries")
)
#> ℹ [2026-07-18 21:22:48] [INFO] Connecting to eyeris database...
#> ✔ [2026-07-18 21:22:48] [OKAY] Connected to eyeris database:
#> /tmp/Rtmp7KMyrS/derivatives/my-project.eyerisdb
#> ℹ [2026-07-18 21:22:48] [INFO] Found 16 tables in database
#> ℹ [2026-07-18 21:22:48] [INFO] Extracting data types: blinks, epochs,
#> timeseries
#> ℹ [2026-07-18 21:22:48] [INFO] Processing blinks...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "stime", "etime", "dur", "eye" FROM
#> "blinks_001_01_assocret_run01" UNION ALL SELECT "subject_id", "session_id",
#> "task_name", "data_type", "run_number", "created_timestamp", "block", "stime",
#> "etime", "dur", "eye" FROM "blinks_001_01_assocret_run03") as combined_data
#> WHERE 1=1 AND subject_id = '001'
#> ℹ [2026-07-18 21:22:48] [INFO] Processing epochs...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "epoch_label", "created_timestamp", "block", "time_orig", "timebin",
#> "time_secs", "time_scaled", "eye_x", "eye_y", "eye", "hz", "type", "pupil_raw",
#> "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z", "text_unique",
#> "template", "matching_pattern", "matched_event", "startstop", "trial" FROM
#> "epochs_001_01_assocret_run01_prepostprobe" UNION ALL SELECT "subject_id",
#> "session_id", "task_name", "data_type", "run_number", "epoch_label",
#> "created_timestamp", "block", "time_orig", "timebin", "time_secs",
#> "time_scaled", "eye_x", "eye_y", "eye", "hz", "type", "pupil_raw",
#> "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z", "text_unique",
#> "template", "matching_pattern", "matched_event", "startstop", "trial" FROM
#> "epochs_001_01_assocret_run03_prepostprobe") as combined_data WHERE 1=1 AND
#> subject_id = '001'
#> ℹ [2026-07-18 21:22:48] [INFO] Processing timeseries...
#> ℹ [2026-07-18 21:22:48] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "time_orig", "time_secs", "time_scaled", "eye_x",
#> "eye_y", "eye", "hz", "type", "pupil_raw", "pupil_raw_deblink",
#> "pupil_raw_deblink_detransient", "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z" FROM
#> "timeseries_001_01_assocret_run01" UNION ALL SELECT "subject_id", "session_id",
#> "task_name", "data_type", "run_number", "created_timestamp", "block",
#> "time_orig", "time_secs", "time_scaled", "eye_x", "eye_y", "eye", "hz", "type",
#> "pupil_raw", "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z" FROM
#> "timeseries_001_01_assocret_run03") as combined_data WHERE 1=1 AND subject_id =
#> '001'
#> ✔ [2026-07-18 21:22:48] [OKAY] Successfully extracted 3 data types
#> ℹ [2026-07-18 21:22:48] [INFO] blinks: 3 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] epochs: 40000 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] timeseries: 62301 rows across 1 subjects
#> ℹ [2026-07-18 21:22:48] [INFO] Disconnected from eyeris database

# extract epoch data for specific epoch label
epoch_data <- eyeris_db_collect(
  bids_dir = tempdir(),
  data_types = "epochs",
  epoch_labels = "prepostprobe"
)
#> ℹ [2026-07-18 21:22:48] [INFO] Connecting to eyeris database...
#> ✔ [2026-07-18 21:22:49] [OKAY] Connected to eyeris database:
#> /tmp/Rtmp7KMyrS/derivatives/my-project.eyerisdb
#> ℹ [2026-07-18 21:22:49] [INFO] Found 16 tables in database
#> ℹ [2026-07-18 21:22:49] [INFO] Extracting data types: epochs
#> ℹ [2026-07-18 21:22:49] [INFO] Processing epochs...
#> ℹ [2026-07-18 21:22:49] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "epoch_label", "created_timestamp", "block", "time_orig", "timebin",
#> "time_secs", "time_scaled", "eye_x", "eye_y", "eye", "hz", "type", "pupil_raw",
#> "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z", "text_unique",
#> "template", "matching_pattern", "matched_event", "startstop", "trial" FROM
#> "epochs_001_01_assocret_run01_prepostprobe" UNION ALL SELECT "subject_id",
#> "session_id", "task_name", "data_type", "run_number", "epoch_label",
#> "created_timestamp", "block", "time_orig", "timebin", "time_secs",
#> "time_scaled", "eye_x", "eye_y", "eye", "hz", "type", "pupil_raw",
#> "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z", "text_unique",
#> "template", "matching_pattern", "matched_event", "startstop", "trial" FROM
#> "epochs_001_01_assocret_run03_prepostprobe") as combined_data WHERE 1=1
#> ✔ [2026-07-18 21:22:49] [OKAY] Successfully extracted 1 data types
#> ℹ [2026-07-18 21:22:49] [INFO] epochs: 40000 rows across 1 subjects
#> ℹ [2026-07-18 21:22:49] [INFO] Disconnected from eyeris database

# }
```
