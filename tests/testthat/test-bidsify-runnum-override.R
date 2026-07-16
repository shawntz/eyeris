# Regression tests for the single-run `run_num`-override bug:
# "separate single-block .asc files bidsified into a shared bids_dir with a
#  per-file run_num override overwrite each other's epoch CSV and metadata"
#
# Workflow that triggered it (one .asc per run, looped into one bids_dir):
#
#   for (i in seq_along(asc_files))
#     glassbox(asc_files[i]) |> epoch(...) |> bidsify(run_num = i, ...)
#
# load_asc() always keys a single-file object as "block_1", so bidsify()
# renames that block to "block_<run_num>" when a run_num override is supplied.
# Two defects broke that rename for a subset of outputs:
#
#   1. .bidsify() renamed the epoch block only in a *local copy* of the
#      block-name vector and never assigned it back onto the epoch list, so the
#      epoch element stayed keyed to "block_1". The single-run epoch writer
#      derived its run number from that key, so every run's
#      `..._desc-preproc_pupil_epoch-<label>.csv` collapsed onto run-01
#      (last-run-wins overwrite).
#
#   2. make_report() enumerated run directories from disk (which accumulate
#      across loop iterations sharing one bids_dir) and regenerated *every*
#      run's `<run>_metadata.json` from the current in-memory object, clobbering
#      each earlier run's source_file/call_stack with the latest run's.

# Copy the single-block demo into `n` distinct .asc files so each loop
# iteration has its own source path (mirrors real separate-run recordings).
copy_demo_runs <- function(n) {
  demo <- eyelink_asc_demo_dataset()
  dir <- tempfile("runnum_srcs_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, sprintf("sub-AM22_t%d.asc", seq_len(n)))
  file.copy(demo, paths)
  paths
}

bidsify_run <- function(asc, bids_dir, run_num, html_report = FALSE) {
  glassbox(asc, verbose = FALSE) |>
    epoch(
      events = "PROBE_{type}_{trial}",
      limits = c(-1, 1),
      label = "trialEpochs",
      verbose = FALSE
    ) |>
    bidsify(
      bids_dir = bids_dir,
      run_num = run_num,
      participant_id = "AM22",
      session_num = "01",
      task_name = "assocmem",
      save_raw = TRUE,
      html_report = html_report,
      verbose = FALSE
    )
}

test_that("single-run run_num override writes a distinct epoch CSV per run", {
  skip_on_cran()

  asc_files <- copy_demo_runs(3)
  on.exit(unlink(dirname(asc_files[1]), recursive = TRUE), add = TRUE)

  bids_dir <- tempfile("bids_runnum_epoch_")
  dir.create(bids_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bids_dir, recursive = TRUE), add = TRUE)

  for (i in seq_along(asc_files)) {
    bidsify_run(asc_files[i], bids_dir, run_num = i, html_report = FALSE)
  }

  deriv <- file.path(bids_dir, "derivatives", "sub-AM22", "ses-01")
  all_files <- list.files(deriv, recursive = TRUE)

  # the epoch label is lowercased by sanitize_event_tag() on its way to disk
  epoch_csvs <- grep(
    "desc-preproc_pupil_epoch-trialepochs\\.csv$",
    all_files,
    value = TRUE
  )

  # one preproc_pupil epoch CSV per run_num, none collapsed onto run-01
  expect_true(any(grepl("run-01_desc-preproc_pupil_epoch-", epoch_csvs)))
  expect_true(any(grepl("run-02_desc-preproc_pupil_epoch-", epoch_csvs)))
  expect_true(any(grepl("run-03_desc-preproc_pupil_epoch-", epoch_csvs)))
  expect_length(unique(epoch_csvs), 3)

  # the in-file `block` column tracks the override too (not stuck at 1)
  read_block <- function(rn) {
    f <- file.path(
      deriv,
      "eye",
      sprintf(
        "sub-AM22_ses-01_task-assocmem_run-%s_desc-preproc_pupil_epoch-trialepochs.csv",
        rn
      )
    )
    unique(utils::read.csv(f)$block)
  }
  expect_equal(read_block("01"), 1)
  expect_equal(read_block("02"), 2)
  expect_equal(read_block("03"), 3)
})

test_that("per-run metadata.json is not clobbered by later runs in a shared bids_dir", {
  skip_on_cran()
  # make_report() writes the metadata json; a full html_report also renders it
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  asc_files <- copy_demo_runs(3)
  on.exit(unlink(dirname(asc_files[1]), recursive = TRUE), add = TRUE)

  bids_dir <- tempfile("bids_runnum_meta_")
  dir.create(bids_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bids_dir, recursive = TRUE), add = TRUE)

  for (i in seq_along(asc_files)) {
    bidsify_run(asc_files[i], bids_dir, run_num = i, html_report = TRUE)
  }

  logs <- file.path(
    bids_dir,
    "derivatives",
    "sub-AM22",
    "ses-01",
    "source",
    "logs"
  )

  for (i in seq_along(asc_files)) {
    rn <- sprintf("%02d", i)
    meta_path <- file.path(
      logs,
      sprintf("task-assocmem_run-%s_metadata.json", rn)
    )
    expect_true(file.exists(meta_path))
    meta <- jsonlite::read_json(meta_path)
    # each run's metadata must reference its OWN source file and run number,
    # not the last run processed into the shared bids_dir
    expect_equal(meta$run, i)
    expect_equal(basename(meta$source_file), basename(asc_files[i]))
  }
})
