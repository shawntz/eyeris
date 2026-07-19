# tests for detrend() linear (default) and spline detrending methods

# preprocess the bundled demo recording up to (but not including) detrend, so
# each test can attach a detrend step of either method. When `gap_idx` is
# supplied, those raw samples are set to NA to create a long missing-data gap
# that survives interpolation (mirrors the helper used in test-interpolate.R).
preprocess_for_detrend <- function(gap_idx = NULL) {
  d <- eyeris::load_asc(eyeris::eyelink_asc_demo_dataset())
  if (!is.null(gap_idx)) {
    ts <- d$timeseries$block_1
    ts$pupil_raw[gap_idx] <- NA
    d$timeseries$block_1 <- ts
  }
  d |>
    eyeris::deblink(extend = 50) |>
    eyeris::detransient() |>
    eyeris::interpolate(verbose = FALSE)
}

test_that("detrend() defaults to linear detrending", {
  base <- preprocess_for_detrend()
  out <- eyeris::detrend(base)

  # the default method is recorded as linear
  expect_equal(out$params$detrend$parameters$method, "linear")

  col <- grep("_detrend$", colnames(out$timeseries$block_1), value = TRUE)
  expect_length(col, 1)

  # a linear model has two coefficients (intercept + slope on time)
  expect_length(out$detrend_coefs$block_1, 2)

  # fitted trend is stored and aligned with the input length
  expect_true("detrend_fitted_values" %in% colnames(out$timeseries$block_1))
  expect_equal(
    length(out$timeseries$block_1$detrend_fitted_values),
    nrow(out$timeseries$block_1)
  )
})

test_that("detrend(method = 'spline') fits a natural spline of time", {
  base <- preprocess_for_detrend()
  out <- eyeris::detrend(base, method = "spline", spline_df = 5)

  expect_equal(out$params$detrend$parameters$method, "spline")
  expect_equal(out$params$detrend$parameters$spline_df, 5L)

  col <- grep("_detrend$", colnames(out$timeseries$block_1), value = TRUE)
  expect_length(col, 1)

  # a natural spline with df = 5 yields intercept + 5 basis coefficients
  expect_length(out$detrend_coefs$block_1, 6)

  expect_equal(
    length(out$timeseries$block_1$detrend_fitted_values),
    nrow(out$timeseries$block_1)
  )
})

test_that("linear and spline detrending produce different fitted trends", {
  base <- preprocess_for_detrend()
  lin <- eyeris::detrend(base, method = "linear")
  spl <- eyeris::detrend(base, method = "spline", spline_df = 5)

  lin_fit <- lin$timeseries$block_1$detrend_fitted_values
  spl_fit <- spl$timeseries$block_1$detrend_fitted_values

  expect_false(isTRUE(all.equal(lin_fit, spl_fit)))
})

test_that("spline detrend removes a nonlinear trend linear detrend cannot", {
  # a smooth, purely nonlinear trend (one full sine period => ~zero linear slope)
  n <- 300
  df <- data.frame(time_secs = seq(0, 20, length.out = n))
  df$pupil_prev <- 1000 + 50 * sin(2 * pi * df$time_secs / 20)

  lin <- eyeris:::detrend_pupil(df, "pupil_prev", method = "linear")
  spl <- eyeris:::detrend_pupil(
    df,
    "pupil_prev",
    method = "spline",
    spline_df = 8
  )

  expect_equal(length(lin$fitted_values), n)
  expect_equal(length(spl$fitted_values), n)

  # the spline captures the nonlinear trend, leaving far smaller residuals
  expect_lt(
    sd(spl$residuals, na.rm = TRUE),
    sd(lin$residuals, na.rm = TRUE) / 5
  )
})

test_that("detrend() validates method and spline_df", {
  base <- preprocess_for_detrend()

  # unknown method rejected by match.arg
  expect_error(eyeris::detrend(base, method = "quadratic"))

  # spline_df must be a single whole number >= 1
  expect_error(
    eyeris::detrend(base, method = "spline", spline_df = 0),
    "spline_df"
  )
  expect_error(
    eyeris::detrend(base, method = "spline", spline_df = 2.5),
    "spline_df"
  )
  expect_error(
    eyeris::detrend(base, method = "spline", spline_df = c(3, 5)),
    "spline_df"
  )

  # spline_df is ignored (not validated) for linear detrending
  expect_no_error(eyeris::detrend(base, method = "linear", spline_df = 0))
})

test_that("spline detrend fits around long gaps and preserves them", {
  gap <- 6000:6399 # 400 ms gap @ 1000 Hz (> 250 ms default max_gap_ms)
  out <- preprocess_for_detrend(gap) |>
    eyeris::detrend(method = "spline", spline_df = 6)

  cols <- colnames(out$timeseries$block_1)
  col <- grep("_interpolate_detrend$", cols, value = TRUE)
  expect_length(col, 1)

  v <- out$timeseries$block_1[[col]]
  # output length aligned with input (na.exclude pads), gap preserved as NA
  expect_equal(length(v), nrow(out$timeseries$block_1))
  expect_true(any(is.na(v[6100:6300])))
  expect_gt(mean(is.finite(v)), 0.9)
})

test_that("glassbox() runs spline detrending via detrend = list(method)", {
  demo <- eyeris::eyelink_asc_demo_dataset()
  out <- eyeris::glassbox(
    demo,
    lpfilt = FALSE,
    detrend = list(method = "spline", spline_df = 5),
    zscore = FALSE,
    verbose = FALSE
  )

  expect_equal(out$params$detrend$parameters$method, "spline")
  col <- grep("_detrend$", colnames(out$timeseries$block_1), value = TRUE)
  expect_length(col, 1)
})

test_that("boilerplate methods text reflects the detrend method", {
  lin <- eyeris:::describe_boilerplate_step(
    "detrend",
    list(method = "linear"),
    list()
  )
  spl <- eyeris:::describe_boilerplate_step(
    "detrend",
    list(method = "spline", spline_df = 7),
    list()
  )

  expect_match(lin, "linear detrending")
  expect_match(spl, "spline detrending")
  expect_match(spl, "7")
})
