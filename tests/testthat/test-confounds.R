test_that("get_confounds_for_step() reports n_missing and prop_missing", {
  # build a minimal pupil data frame with a known number of NA samples
  set.seed(1)
  n <- 100
  pupil_vec <- rnorm(n, mean = 1000, sd = 50)
  na_idx <- c(3, 10, 11, 12, 77) # 5 missing samples
  pupil_vec[na_idx] <- NA

  pupil_df <- data.frame(eye_x = runif(n, 0, 800), eye_y = runif(n, 0, 600))

  step_confounds <- eyeris:::get_confounds_for_step(
    pupil_df = pupil_df,
    pupil_vec = pupil_vec,
    screen_width = 800,
    screen_height = 600,
    hz = 1000
  )

  expect_true(all(c("n_missing", "prop_missing") %in% names(step_confounds)))
  expect_equal(step_confounds$n_missing, length(na_idx))
  expect_equal(step_confounds$prop_missing, length(na_idx) / n)
  expect_gte(step_confounds$prop_missing, 0)
  expect_lte(step_confounds$prop_missing, 1)
})

test_that("summarize_confounds() exposes prop_missing at block and epoch levels", {
  demo_data <- eyeris::eyelink_asc_demo_dataset()

  result <- demo_data |>
    eyeris::glassbox(verbose = FALSE) |>
    eyeris::epoch(
      events = "PROBE_{type}_{trial}",
      limits = c(-1, 1),
      label = "prePostProbe",
      verbose = FALSE
    ) |>
    eyeris::summarize_confounds()

  # --- block level (unepoched_timeseries) ---------------------------------
  unepoched <- result$confounds$unepoched_timeseries
  expect_true(length(unepoched) > 0)

  block_name <- names(unepoched)[1]
  block <- unepoched[[block_name]]
  expect_true(length(block) > 0)

  # prop_missing/n_missing must be present and internally consistent, and
  # must match the actual NA proportion of the underlying step column
  for (step_name in names(block)) {
    step_confounds <- block[[step_name]]
    expect_true(all(
      c("n_missing", "prop_missing", "n_samples") %in% names(step_confounds)
    ))
    expect_gte(step_confounds$prop_missing, 0)
    expect_lte(step_confounds$prop_missing, 1)
    expect_equal(
      step_confounds$prop_missing,
      step_confounds$n_missing / step_confounds$n_samples
    )

    col <- result$timeseries[[block_name]][[step_name]]
    expect_equal(step_confounds$n_missing, sum(is.na(col)))
    expect_equal(step_confounds$prop_missing, mean(is.na(col)))
  }

  # --- epoch / trial level (epoched_timeseries) ---------------------------
  epoched <- result$confounds$epoched_timeseries
  expect_true(length(epoched) > 0)

  epoch_name <- names(epoched)[1]
  epoch_block_name <- names(epoched[[epoch_name]])[1]
  epoch_block <- epoched[[epoch_name]][[epoch_block_name]]

  expect_true(all(
    c("n_missing", "prop_missing", "n_samples") %in% names(epoch_block)
  ))
  expect_true(all(
    epoch_block$prop_missing >= 0 & epoch_block$prop_missing <= 1
  ))
  expect_equal(
    epoch_block$prop_missing,
    epoch_block$n_missing / epoch_block$n_samples
  )
})

test_that("fully-missing epochs report prop_missing = 1 (remain filterable)", {
  hz <- 100
  n <- 50

  # a recording block whose only pupil step is entirely NA for this epoch
  block_ts <- data.frame(
    time_orig = seq_len(n) * 10,
    eye_x = 400,
    eye_y = 300,
    pupil_raw_deblink = rep(NA_real_, n)
  )

  epoch_df <- data.frame(
    time_orig = block_ts$time_orig,
    matched_event = "trial_1",
    eye_x = 400,
    eye_y = 300,
    pupil_raw_deblink = rep(NA_real_, n)
  )

  eyeris_obj <- list(
    timeseries = list(block_1 = block_ts),
    blinks = list(block_1 = data.frame(stime = numeric(0), etime = numeric(0))),
    info = list(screen.x = 800, screen.y = 600, sample.rate = hz),
    epoch_trial = list(block_1 = epoch_df)
  )
  class(eyeris_obj) <- "eyeris"

  # a fully-missing epoch must still surface a row (not be silently dropped)
  expect_no_warning(
    res <- eyeris:::calculate_epoched_confounds(
      eyeris_obj,
      epoch_names = "epoch_trial",
      hz = hz,
      verbose = FALSE
    )
  )

  ets <- res$confounds$epoched_timeseries$epoch_trial$block_1
  expect_false(is.null(ets))
  expect_true(all(c("n_missing", "prop_missing") %in% names(ets)))

  row <- ets[ets$step == "raw_deblink", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$prop_missing, 1)
  expect_equal(row$n_missing, n)
})
