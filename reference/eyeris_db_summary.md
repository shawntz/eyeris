# Get summary statistics for eyeris database

Provides a quick overview of the contents of an `eyeris` database,
including available subjects, sessions, tasks, and data types.

## Usage

``` r
eyeris_db_summary(bids_dir, db_path = "my-project", verbose = TRUE)
```

## Arguments

- bids_dir:

  Path to the BIDS directory containing the database

- db_path:

  Database name (defaults to "my-project", becomes
  "my-project.eyerisdb")

- verbose:

  Logical. Whether to print detailed output (default TRUE)

## Value

A named list containing summary information about the database contents

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
    bids_dir = file.path(tempdir(), "my-cool-memory-project"),
    participant_id = "001",
    session_num = "01",
    task_name = "assocret",
    run_num = "03", # override default run-01 (block_1) to use run-03 instead
    db_enabled = TRUE,
    db_path = "my-cool-memory-study",
  )
#> ✔ [2026-06-13 06:08:48] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-13 06:08:48] [INFO] Processing block: block_1
#> ✔ [2026-06-13 06:08:48] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-13 06:08:48] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-13 06:08:48] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-13 06:08:48] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-06-13 06:08:49] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-13 06:08:49] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-13 06:08:49] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-13 06:08:49] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-13 06:08:49] [INFO] Block processing summary:
#> ℹ [2026-06-13 06:08:49] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-06-13 06:08:49] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-06-13 06:08:49] [INFO] Epoching pupil data...
#> ℹ [2026-06-13 06:08:49] [INFO] Block 1: found 10 matching events for
#> PROBEstartstoptrial
#> ✔ [2026-06-13 06:08:49] [OKAY] Done!
#> ✔ [2026-06-13 06:08:49] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-06-13 06:08:49] [OKAY] Pupil epoching completed in 0.12 seconds
#> ℹ [2026-06-13 06:08:49] [INFO] Recalculating epoched confounds for new
#> epochs...
#> ℹ [2026-06-13 06:08:49] [INFO] Starting BIDSify for sub-001 (monocular)
#> ℹ [2026-06-13 06:08:49] [INFO] Only 1 block detected...
#> ℹ [2026-06-13 06:08:49] [INFO] Using run_num = 03 for single block data
#> ℹ [2026-06-13 06:08:49] [INFO] Filtered epochs: epoch_prePostProbe
#> ℹ [2026-06-13 06:08:49] [INFO] Epoch names to save: epoch_prePostProbe
#> ℹ [2026-06-13 06:08:49] [INFO] Parallel processing detected for job unknown
#> (PID: 6794), using temporary database
#> ✔ [2026-06-13 06:08:49] [OKAY] Created temporary database:
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/my-cool-memory-study_temp_6794_20260613_060849_459.eyerisdb
#> ℹ [2026-06-13 06:08:49] [INFO] epoch_prePostProbe:
#> ℹ [2026-06-13 06:08:49] [INFO] block_1: data.frame with 20000 rows
#> ℹ [2026-06-13 06:08:49] [INFO] info: list with 1 elements
#> ! [2026-06-13 06:08:49] [WARN] '/tmp/RtmptU1Rvl/my-cool-memory-project' already
#> exists. Skipping creation...
#> ! [2026-06-13 06:08:49] [WARN]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives' already exists. Skipping
#> creation...
#> ℹ [2026-06-13 06:08:49] [INFO]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001' does not exist.
#> Creating...
#> ✔ [2026-06-13 06:08:49] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001'
#> ℹ [2026-06-13 06:08:49] [INFO]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01' does not
#> exist. Creating...
#> ✔ [2026-06-13 06:08:49] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01'
#> ℹ [2026-06-13 06:08:49] [INFO]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/eye' does
#> not exist. Creating...
#> ✔ [2026-06-13 06:08:49] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/eye'
#> ℹ [2026-06-13 06:08:49] [INFO] Writing blinks data to
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-blinks.csv...
#> ✔ [2026-06-13 06:08:49] [OKAY] Wrote blinks data (1 rows) to CSV and database
#> ℹ [2026-06-13 06:08:49] [INFO] Writing events data to
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/eye/sub-001_ses-01_task-assocret_run-03_desc-events.csv...
#> ✔ [2026-06-13 06:08:49] [OKAY] Wrote events data (67 rows) to CSV and database
#> ℹ [2026-06-13 06:08:49] [INFO] Processing single-run epoch: epoch_prePostProbe
#> (label: prePostProbe)
#> ℹ [2026-06-13 06:08:49] [INFO] Block block_1 for epoch epoch_prePostProbe has
#> 20000 rows
#> ! [2026-06-13 06:08:49] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-06-13 06:08:49] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-06-13 06:08:49] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-06-13 06:08:49] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-06-13 06:08:49] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-06-13 06:08:49] [OKAY] Wrote epochs data (20000 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote timeseries data (20767 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote run_confounds data (6 rows) to CSV and
#> database
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-06-13 06:08:50] [INFO] Created epoch summary for epoch_prePostProbe
#> with 9 fields
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote epoch_summary data (1 rows) to CSV and
#> database
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-06-13 06:08:50] [INFO] Found epoch events in structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_summary data (1 rows) to CSV and
#> database
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ℹ [2026-06-13 06:08:50] [INFO] Found epoch events in epoch structure:
#> PROBE_{startstop}_{trial}
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ! [2026-06-13 06:08:50] [WARN] No baseline structure found for epoch label:
#> prePostProbe
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ✔ [2026-06-13 06:08:50] [OKAY] Wrote confounds_events data (6 rows) to CSV and
#> database
#> ℹ [2026-06-13 06:08:50] [INFO]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures'
#> does not exist. Creating...
#> ✔ [2026-06-13 06:08:50] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures'
#> ℹ [2026-06-13 06:08:50] [INFO]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> does not exist. Creating...
#> ✔ [2026-06-13 06:08:50] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> ℹ [2026-06-13 06:08:50] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:50] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:50] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:51] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ℹ [2026-06-13 06:08:52] [INFO] Plotting block 3 with sampling rate 1000 Hz from
#> possible blocks: 3
#> ! [2026-06-13 06:08:52] [WARN]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ✔ [2026-06-13 06:08:53] [OKAY] Created gaze heatmap for run-03
#> ! [2026-06-13 06:08:53] [WARN]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03'
#> already exists. Skipping creation...
#> ℹ [2026-06-13 06:08:53] [INFO]
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe'
#> does not exist. Creating...
#> ✔ [2026-06-13 06:08:53] [OKAY] BIDS directory successfully created at:
#> '/tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe'
#> ✔ [2026-06-13 06:09:00] [OKAY] Created epoch images zip:
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe/task-assocret_run-03.zip
#> (70 images)
#> ℹ [2026-06-13 06:09:00] [INFO] Using absolute zip file path:
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe/task-assocret_run-03.zip
#> ✔ [2026-06-13 06:09:00] [OKAY] Embedded zip file as data URL (7969936 bytes)
#> 
#> 
#> processing file: sub-001_task-assocret_epoch-prePostProbe_run-03.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret_epoch-prePostProbe_run-03.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret_epoch-prePostProbe_run-03.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret_epoch-prePostProbe_run-03.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmptU1Rvl/rmarkdown-str1a8a51212b74.html 
#> 
#> Output created: sub-001_task-assocret_epoch-prePostProbe_run-03.html
#> ! [2026-06-13 06:09:08] [WARN] Skipping block info for epoch 1 - no valid data
#> ℹ [2026-06-13 06:09:08] [INFO] Removing duplicate plain epoch directory:
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/sub-001/ses-01/source/figures/task-assocret_run-03/epoch_prePostProbe
#> ! [2026-06-13 06:09:09] [WARN] No detrend data found for run-03
#> 
#> 
#> processing file: sub-001_task-assocret.Rmd
#> 1/5               
#> 2/5 [citation]    
#> 3/5               
#> 4/5 [session-info]
#> 5/5               
#> output file: sub-001_task-assocret.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS sub-001_task-assocret.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output sub-001_task-assocret.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 6 --variable toc_float=1 --variable toc_selectors=h1,h2,h3,h4,h5,h6 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --syntax-highlighting none --variable highlightjs=1 --variable theme=bootstrap --css /home/runner/work/_temp/Library/eyeris/rmarkdown/css/report.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmptU1Rvl/rmarkdown-str1a8a3f1bb4cf.html 
#> 
#> Output created: sub-001_task-assocret.html
#> ℹ [2026-06-13 06:09:10] [INFO] Merging temporary database from job unknown
#> (PID: 6794) into main database
#> ℹ [2026-06-13 06:09:10] [INFO] Merging 8 tables from temporary database to main
#> database
#> ℹ [2026-06-13 06:09:10] [INFO] Created new table 'blinks_001_01_assocret_run03'
#> with 1 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table
#> 'confounds_events_001_01_assocret_run03_prepostprobe' with 60 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table
#> 'confounds_summary_001_01_assocret_run03_prepostprobe' with 10 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table
#> 'epoch_summary_001_01_assocret_run03' with 1 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table
#> 'epochs_001_01_assocret_run01_prepostprobe' with 20000 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table 'events_001_01_assocret_run03'
#> with 67 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table
#> 'run_confounds_001_01_assocret_run03' with 6 rows
#> ℹ [2026-06-13 06:09:11] [INFO] Created new table
#> 'timeseries_001_01_assocret_run03' with 20767 rows
#> ✔ [2026-06-13 06:09:11] [OKAY] Successfully merged 8/8 tables
#> ✔ [2026-06-13 06:09:11] [OKAY] Successfully merged job unknown (PID: 6794) data
#> into main database
#> ℹ [2026-06-13 06:09:11] [INFO] Disconnected from temporary database
#> ✔ [2026-06-13 06:09:11] [OKAY] Cleaned up temporary database file
#> ℹ [2026-06-13 06:09:11] [INFO] Finished BIDSify for sub-001 (Duration: 22.24
#> seconds)

# get database summary
summary <- eyeris_db_summary(
             file.path(
               tempdir(),
               "my-cool-memory-project"
             ),
             db_path = "my-cool-memory-study"
           )
#> ℹ [2026-06-13 06:09:11] [INFO] Connecting to eyeris database...
#> ✔ [2026-06-13 06:09:11] [OKAY] Connected to eyeris database:
#> /tmp/RtmptU1Rvl/my-cool-memory-project/derivatives/my-cool-memory-study.eyerisdb
#> ✔ [2026-06-13 06:09:11] [OKAY] Database summary:
#> ℹ [2026-06-13 06:09:11] [INFO] Total tables: 8
#> ℹ [2026-06-13 06:09:11] [INFO] Subjects: 1 (sub-01)
#> ℹ [2026-06-13 06:09:11] [INFO] Sessions: 1 (ses-01)
#> ℹ [2026-06-13 06:09:11] [INFO] Tasks: 1 (assocret)
#> ℹ [2026-06-13 06:09:11] [INFO] Data types: 8 (blinks, confounds_events,
#> confounds_summary, epoch_summary, epochs, events, run_confounds, timeseries)
#> ℹ [2026-06-13 06:09:11] [INFO] Epoch labels: 1 (prepostprobe)
#> ℹ [2026-06-13 06:09:11] [INFO] Total rows: 40912
#> ℹ [2026-06-13 06:09:11] [INFO] Disconnected from eyeris database

# view available subjects
summary$subjects
#> [1] "sub-01"

# view available data types
summary$data_types
#> [1] "blinks"            "confounds_events"  "confounds_summary"
#> [4] "epoch_summary"     "epochs"            "events"           
#> [7] "run_confounds"     "timeseries"       

# view table counts
summary$table_counts
#>                         blinks_001_01_assocret_run03 
#>                                                    1 
#>  confounds_events_001_01_assocret_run03_prepostprobe 
#>                                                   60 
#> confounds_summary_001_01_assocret_run03_prepostprobe 
#>                                                   10 
#>                  epoch_summary_001_01_assocret_run03 
#>                                                    1 
#>            epochs_001_01_assocret_run01_prepostprobe 
#>                                                20000 
#>                         events_001_01_assocret_run03 
#>                                                   67 
#>                  run_confounds_001_01_assocret_run03 
#>                                                    6 
#>                     timeseries_001_01_assocret_run03 
#>                                                20767 
# }
```
