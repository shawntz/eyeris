# Regression tests for the multi-run epoch CSV bug:
# "*_desc-preproc_pupil_epoch-<label>.csv are not generated for each run"
#
# In run_bidsify(), the multi-run (`has_multiple_runs`) epoch-writing branch
# built its output filename with `run_num = run_num` (the function argument,
# which is explicitly ignored/NULL when an object contains multiple blocks) and
# `desc = paste0("preproc_pupil_", current_label)` (no BIDS `run-`/`epoch-`
# tokens). As a result every run wrote to the *same* filename, so each run
# silently overwrote the previous one (last-run-wins) and the expected
# per-run `..._run-NN_desc-preproc_pupil_epoch-<label>.csv` files never
# appeared. The fix derives the run number from the block being written and
# routes the epoch label through `make_bids_fname(epoch_name = ...)`, exactly
# mirroring the single-run branch.
#
# Two related fixes are also exercised here:
#   * the multi-run raw-timeseries writers now key the `run-NN` token off the
#     block's own `block` column rather than a positional `lapply()` index, and
#   * the epoch/baseline metadata helpers are no longer mis-called with
#     `verbose` in the `block_name` position (which crashed `bidsify()` outright
#     under `verbose = FALSE`).

# Build a genuine two-block (two-run) eyeris object from the single-block demo
# by deep-copying block_1 into a deliberately NON-sequential block key across
# every block-keyed structure that bidsify() touches. The second block is
# numbered 7 (not 2) on purpose: a positional-index regression (keying the
# `run-NN` token off the lapply() position instead of the block's own number)
# would still emit run-02 and pass a sequential fixture, so the second block
# must resolve to run-07 for the test to actually exercise the fix.
make_two_block <- function(obj, new_block = 7L) {
  new_key <- paste0("block_", new_block)
  setb <- function(df, b) {
    if (is.data.frame(df) && "block" %in% colnames(df)) {
      df$block <- b
    }
    df
  }

  obj$timeseries[[new_key]] <- setb(obj$timeseries$block_1, new_block)
  obj$events[[new_key]] <- setb(obj$events$block_1, new_block)
  obj$blinks[[new_key]] <- setb(obj$blinks$block_1, new_block)
  obj$latest[[new_key]] <- obj$latest$block_1

  for (en in grep("^epoch_", names(obj), value = TRUE)) {
    obj[[en]][[new_key]] <- setb(obj[[en]]$block_1, new_block)
    if (!is.null(obj[[en]]$info)) {
      obj[[en]]$info[[new_key]] <- obj[[en]]$info$block_1
    }
  }

  cf <- obj$confounds
  if (!is.null(cf$unepoched_timeseries)) {
    cf$unepoched_timeseries[[new_key]] <- setb(
      cf$unepoched_timeseries$block_1,
      new_block
    )
  }
  for (en in names(cf$epoched_timeseries)) {
    cf$epoched_timeseries[[en]][[new_key]] <- setb(
      cf$epoched_timeseries[[en]]$block_1,
      new_block
    )
  }
  for (en in names(cf$epoched_epoch_wide)) {
    cf$epoched_epoch_wide[[en]][[new_key]] <- setb(
      cf$epoched_epoch_wide[[en]]$block_1,
      new_block
    )
  }
  obj$confounds <- cf

  obj
}

build_two_run_epoched <- function() {
  eyelink_asc_demo_dataset() |>
    glassbox(verbose = FALSE) |>
    epoch(
      events = "PROBE_{type}_{trial}",
      limits = c(-1, 1),
      label = "prePostProbe",
      verbose = FALSE
    ) |>
    make_two_block()
}

test_that("multi-run bidsify writes a distinct epoch CSV for each run", {
  skip_on_cran()

  obj <- build_two_run_epoched()
  expect_length(obj$timeseries, 2) # truly multi-block

  bids_dir <- tempfile("bids_multirun_epoch_")
  dir.create(bids_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bids_dir, recursive = TRUE), add = TRUE)

  # verbose = FALSE also guards the previously-crashing helper mis-call
  bidsify(
    obj,
    bids_dir = bids_dir,
    participant_id = "001",
    session_num = "01",
    task_name = "demo",
    html_report = FALSE,
    verbose = FALSE
  )

  deriv <- file.path(bids_dir, "derivatives", "sub-001", "ses-01")
  all_files <- list.files(deriv, recursive = TRUE)

  # the epoch label is lowercased by sanitize_event_tag() on its way to disk
  epoch_csvs <- grep(
    "desc-preproc_pupil_epoch-prepostprobe\\.csv$",
    all_files,
    value = TRUE
  )

  # one file per run, keyed to each block's own number (1 and 7, not 1 and 2),
  # and they are distinct (no silent overwrite)
  expect_true(any(grepl("run-01_desc-preproc_pupil_epoch-", epoch_csvs)))
  expect_true(any(grepl("run-07_desc-preproc_pupil_epoch-", epoch_csvs)))
  expect_length(unique(epoch_csvs), 2)
})

test_that("multi-run bidsify keys raw timeseries CSVs to the true block number", {
  skip_on_cran()

  obj <- build_two_run_epoched()

  bids_dir <- tempfile("bids_multirun_ts_")
  dir.create(bids_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bids_dir, recursive = TRUE), add = TRUE)

  bidsify(
    obj,
    bids_dir = bids_dir,
    participant_id = "001",
    session_num = "01",
    task_name = "demo",
    html_report = FALSE,
    verbose = FALSE
  )

  deriv <- file.path(bids_dir, "derivatives", "sub-001", "ses-01")
  all_files <- list.files(deriv, recursive = TRUE)

  # block 7 must surface as run-07 (a positional index would wrongly yield run-02)
  ts_csvs <- grep("desc-timeseries\\.csv$", all_files, value = TRUE)
  expect_true(any(grepl("run-01_desc-timeseries", ts_csvs)))
  expect_true(any(grepl("run-07_desc-timeseries", ts_csvs)))
  expect_length(unique(ts_csvs), 2)
})
