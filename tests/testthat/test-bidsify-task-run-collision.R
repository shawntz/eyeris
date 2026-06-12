# Regression tests for issue #293:
# "Same run number across different task names causes conflict in sub-xyz.html"
#
# BIDS allows different task names to share the same run number within a block
# (e.g., a study phase and a test phase can both be run-01). Before the fix,
# the HTML report (sub-xyz.html), the source/figures/run-XX directories, and the
# per-run metadata were keyed by run number alone, so a second task with the
# same run number silently overwrote the first. The fix keys all report/figure
# artifacts by task + run via make_run_dir_name().

test_that("make_run_dir_name keys by task + run with legacy fallback", {
  expect_equal(make_run_dir_name(1, "study"), "task-study_run-01")
  expect_equal(make_run_dir_name(12, "test"), "task-test_run-12")
  expect_equal(make_run_dir_name(1, NULL), "run-01")
  expect_equal(make_run_dir_name(1, ""), "run-01")
  # task names containing underscores are preserved verbatim
  expect_equal(make_run_dir_name(3, "study_a"), "task-study_a_run-03")
})

test_that("filter_task_run_dirs scopes run directories to a single task", {
  dirs <- c(
    "task-study_run-01",
    "task-test_run-01",
    "task-study_run-02",
    "run-01", # legacy / task-less
    "not-a-run-dir"
  )
  expect_setequal(
    filter_task_run_dirs(dirs, "study"),
    c("task-study_run-01", "task-study_run-02")
  )
  expect_setequal(filter_task_run_dirs(dirs, "test"), "task-test_run-01")
  # NULL/empty task keeps only legacy task-less run dirs
  expect_setequal(filter_task_run_dirs(dirs, NULL), "run-01")
  # task with an underscore is matched by exact string equality, not regex
  expect_setequal(
    filter_task_run_dirs(
      c("task-study_a_run-01", "task-study_run-01"),
      "study_a"
    ),
    "task-study_a_run-01"
  )
})

test_that("same run number across different tasks does not collide (#293)", {
  skip_on_cran()
  # rendering the HTML report requires pandoc; the path/figure logic under test
  # runs regardless, but the final report file only materializes with pandoc
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  bids_dir <- tempfile("bids293_")
  dir.create(bids_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bids_dir, recursive = TRUE), add = TRUE)

  demo <- eyeris::eyelink_asc_demo_dataset()

  run_one <- function(task) {
    demo |>
      eyeris::glassbox(verbose = FALSE) |>
      eyeris::bidsify(
        bids_dir = bids_dir,
        participant_id = "001",
        session_num = "01",
        task_name = task,
        run_num = "01",
        html_report = TRUE,
        verbose = FALSE
      )
  }

  run_one("study")
  run_one("test") # same sub/ses/run, different task -> must NOT overwrite "study"

  deriv <- file.path(bids_dir, "derivatives", "sub-001", "ses-01")

  # 1. both task-specific HTML reports exist
  expect_true(file.exists(file.path(deriv, "sub-001_task-study.html")))
  expect_true(file.exists(file.path(deriv, "sub-001_task-test.html")))

  # 2. figure run-dirs are task-namespaced and both present
  figs <- file.path(deriv, "source", "figures")
  expect_true(dir.exists(file.path(figs, "task-study_run-01")))
  expect_true(dir.exists(file.path(figs, "task-test_run-01")))

  # 3. there is NO task-less run-01 dir collision (regression guard)
  expect_false(dir.exists(file.path(figs, "run-01")))

  # 4. step figures inside each task dir carry the task + run prefix
  study_jpgs <- list.files(
    file.path(figs, "task-study_run-01"),
    pattern = "^task-study_run-01_fig-.*\\.jpg$"
  )
  test_jpgs <- list.files(
    file.path(figs, "task-test_run-01"),
    pattern = "^task-test_run-01_fig-.*\\.jpg$"
  )
  expect_gt(length(study_jpgs), 0)
  expect_gt(length(test_jpgs), 0)

  # 5. per-task metadata json does not collide
  logs <- file.path(deriv, "source", "logs")
  expect_true(file.exists(file.path(logs, "task-study_run-01_metadata.json")))
  expect_true(file.exists(file.path(logs, "task-test_run-01_metadata.json")))

  # 6. data files remain task-safe (already worked; guards against regression)
  all_files <- list.files(deriv, recursive = TRUE)
  expect_true(any(grepl("task-study", all_files)))
  expect_true(any(grepl("task-test", all_files)))
})
