test_that("compute_run_data_loss returns a sensible percentage from confounds", {
  sample_data <- eyelink_asc_demo_dataset() |> glassbox()

  pct <- compute_run_data_loss(sample_data, run_num = 1)

  expect_true(is.numeric(pct))
  expect_length(pct, 1)
  expect_false(is.na(pct))
  expect_gte(pct, 0)
  expect_lte(pct, 100)

  # should match the canonical confounds metric on the raw signal
  expected <- sample_data$confounds$unepoched_timeseries$block_1$pupil_raw$prop_invalid *
    100
  expect_equal(pct, expected)
})

test_that("compute_run_data_loss handles zero-padded run identifiers", {
  sample_data <- eyelink_asc_demo_dataset() |> glassbox()

  expect_equal(
    compute_run_data_loss(sample_data, run_num = "01"),
    compute_run_data_loss(sample_data, run_num = 1)
  )
})

test_that("compute_run_data_loss falls back to the raw timeseries", {
  sample_data <- eyelink_asc_demo_dataset() |> glassbox()

  # strip confounds to force the fallback path
  no_confounds <- sample_data
  no_confounds$confounds <- NULL

  pct <- compute_run_data_loss(no_confounds, run_num = 1)

  expect_false(is.na(pct))
  expect_equal(
    pct,
    mean(is.na(no_confounds$timeseries$block_1$pupil_raw)) * 100
  )
})

test_that("compute_run_data_loss returns NA for missing/invalid input", {
  expect_true(is.na(compute_run_data_loss(NULL, run_num = 1)))
  expect_true(is.na(compute_run_data_loss(list(), run_num = 99)))
})

test_that("compute_run_data_loss resolves binocular eyes", {
  # build a minimal binocular object with distinct per-eye data loss
  make_eye <- function(prop_invalid) {
    list(
      binocular_mode = "both",
      confounds = list(
        unepoched_timeseries = list(
          block_1 = list(pupil_raw = data.frame(prop_invalid = prop_invalid))
        )
      )
    )
  }

  bino <- list(left = make_eye(0.10), right = make_eye(0.40))
  class(bino) <- "eyeris"

  expect_true(is_binocular_object(bino))
  expect_equal(
    compute_run_data_loss(bino, run_num = 1, eye_suffix = "eye-L"),
    10
  )
  expect_equal(
    compute_run_data_loss(bino, run_num = 1, eye_suffix = "eye-R"),
    40
  )
})
