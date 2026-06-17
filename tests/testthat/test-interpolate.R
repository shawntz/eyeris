# helper: build a minimal data frame for interpolate_pupil() with a uniform
# 1000 Hz time base (1 ms per sample) so that `max_gap_ms` maps 1:1 to samples
make_pupil_df <- function(pupil, hz = 1000) {
  data.frame(
    time_orig = seq(0, by = 1000 / hz, length.out = length(pupil)),
    p = pupil
  )
}

test_that("interpolate_pupil() fills short gaps but leaves long gaps as NA", {
  # baseline of 1s, with a short (10 ms) and a long (300 ms) gap
  pupil <- rep(1000, 2000)
  pupil[101:110] <- NA # 10 ms gap (short)
  pupil[1001:1300] <- NA # 300 ms gap (long)
  df <- make_pupil_df(pupil)

  out <- interpolate_pupil(df, "p", verbose = FALSE, max_gap_ms = 250)

  # short gap interpolated
  expect_false(any(is.na(out[101:110])))
  # long gap left untouched (still NA)
  expect_true(all(is.na(out[1001:1300])))
  # only the long gap remains missing
  expect_equal(sum(is.na(out)), 300)
})

test_that("max_gap_ms = Inf (and NULL) interpolates across all gaps", {
  pupil <- rep(1000, 2000)
  pupil[1001:1500] <- NA # 500 ms gap
  df <- make_pupil_df(pupil)

  out_inf <- interpolate_pupil(df, "p", verbose = FALSE, max_gap_ms = Inf)
  out_null <- interpolate_pupil(df, "p", verbose = FALSE, max_gap_ms = NULL)

  expect_false(any(is.na(out_inf)))
  expect_false(any(is.na(out_null)))
})

test_that("max_gap_ms threshold is enforced at the sample boundary", {
  # gap of exactly 250 samples (250 ms @ 1000 Hz) -> filled
  pupil_at <- rep(1000, 1000)
  pupil_at[301:550] <- NA # 250 samples
  out_at <- interpolate_pupil(
    make_pupil_df(pupil_at), "p",
    verbose = FALSE, max_gap_ms = 250
  )
  expect_false(any(is.na(out_at)))

  # gap of 251 samples (> 250 ms) -> left as NA
  pupil_over <- rep(1000, 1000)
  pupil_over[301:551] <- NA # 251 samples
  out_over <- interpolate_pupil(
    make_pupil_df(pupil_over), "p",
    verbose = FALSE, max_gap_ms = 250
  )
  expect_true(all(is.na(out_over[301:551])))
})

test_that("max_gap_ms scales with the sampling rate (500 Hz)", {
  # at 500 Hz, 1 sample = 2 ms; a 200-sample gap = 400 ms (> 250) -> NA
  pupil <- rep(1000, 1000)
  pupil[301:500] <- NA # 200 samples = 400 ms @ 500 Hz
  df <- make_pupil_df(pupil, hz = 500)

  out <- interpolate_pupil(df, "p", verbose = FALSE, max_gap_ms = 250)
  expect_true(all(is.na(out[301:500])))

  # a 100-sample gap = 200 ms (< 250) -> filled
  pupil2 <- rep(1000, 1000)
  pupil2[301:400] <- NA # 100 samples = 200 ms @ 500 Hz
  out2 <- interpolate_pupil(
    make_pupil_df(pupil2, hz = 500), "p",
    verbose = FALSE, max_gap_ms = 250
  )
  expect_false(any(is.na(out2)))
})

test_that("leading and trailing long gaps are left as NA", {
  pupil <- rep(1000, 1000)
  pupil[1:300] <- NA # long leading gap (300 ms)
  pupil[701:1000] <- NA # long trailing gap (300 ms)
  df <- make_pupil_df(pupil)

  out <- interpolate_pupil(df, "p", verbose = FALSE, max_gap_ms = 250)
  expect_true(all(is.na(out[1:300])))
  expect_true(all(is.na(out[701:1000])))

  # short leading/trailing gaps are filled (rule = 2 boundary extension)
  pupil2 <- rep(1000, 1000)
  pupil2[1:10] <- NA
  pupil2[991:1000] <- NA
  out2 <- interpolate_pupil(
    make_pupil_df(pupil2), "p",
    verbose = FALSE, max_gap_ms = 250
  )
  expect_false(any(is.na(out2)))
})

test_that("interpolate_pupil() skips when there are no NAs", {
  pupil <- rep(1000, 100)
  out <- interpolate_pupil(make_pupil_df(pupil), "p", verbose = FALSE)
  expect_equal(out, pupil)
})

test_that("validate_max_gap_ms() validates input", {
  expect_equal(validate_max_gap_ms(NULL), Inf)
  expect_equal(validate_max_gap_ms(250), 250)
  expect_equal(validate_max_gap_ms(Inf), Inf)

  expect_error(validate_max_gap_ms(0)) # 0 -> use interpolate = FALSE instead
  expect_error(validate_max_gap_ms(-1))
  expect_error(validate_max_gap_ms(NA))
  expect_error(validate_max_gap_ms("250"))
  expect_error(validate_max_gap_ms(c(100, 200)))
})

test_that("interpolate() leaves long gaps as NA end-to-end (default 250 ms)", {
  demo_data <- eyeris::eyelink_asc_demo_dataset()
  data <- eyeris::load_asc(demo_data)

  # inject an isolated 300 ms gap into the raw pupil signal of block_1
  ts <- data$timeseries$block_1
  gap_idx <- 5000:5299
  ts$pupil_raw[gap_idx] <- NA
  data$timeseries$block_1 <- ts

  out_default <- eyeris::interpolate(data, verbose = FALSE)
  col <- "pupil_raw_interpolate"
  expect_true(all(is.na(out_default$timeseries$block_1[[col]][gap_idx])))

  # records max_gap_ms in the stored call parameters
  expect_equal(out_default$params$interpolate$parameters$max_gap_ms, 250)

  # Inf restores legacy behavior (fills the injected gap)
  out_inf <- eyeris::interpolate(data, max_gap_ms = Inf, verbose = FALSE)
  expect_false(any(is.na(out_inf$timeseries$block_1[[col]][gap_idx])))
})

test_that("glassbox() threads max_gap_ms through the interpolate step", {
  demo_data <- eyeris::eyelink_asc_demo_dataset()

  # default (interpolate = TRUE) -> max_gap_ms = 250
  g_default <- eyeris::glassbox(
    demo_data,
    lpfilt = FALSE,
    zscore = FALSE,
    verbose = FALSE
  )
  expect_equal(g_default$params$interpolate$parameters$max_gap_ms, 250)

  # list form overrides the threshold
  g_custom <- eyeris::glassbox(
    demo_data,
    interpolate = list(max_gap_ms = 75),
    lpfilt = FALSE,
    zscore = FALSE,
    verbose = FALSE
  )
  expect_equal(g_custom$params$interpolate$parameters$max_gap_ms, 75)

  # disabling the step entirely skips interpolation
  g_off <- eyeris::glassbox(
    demo_data,
    interpolate = FALSE,
    lpfilt = FALSE,
    zscore = FALSE,
    verbose = FALSE
  )
  expect_false(
    any(grepl("interpolate", names(g_off$timeseries$block_1)))
  )
})

# helper: load demo data and inject an isolated long gap (in samples) into the
# raw pupil signal of block_1, returning the modified eyeris object
load_demo_with_gap <- function(gap_idx = 6000:6399) {
  d <- eyeris::load_asc(eyeris::eyelink_asc_demo_dataset())
  ts <- d$timeseries$block_1
  ts$pupil_raw[gap_idx] <- NA
  d$timeseries$block_1 <- ts
  d
}

test_that("lpfilt filters around long interpolation gaps and preserves them", {
  withr::local_pdf(tempfile()) # absorb lpfilt's par() restore device calls
  gap <- 6000:6399 # 400 ms gap @ 1000 Hz (> 250 ms default)
  out <- load_demo_with_gap(gap) |>
    eyeris::deblink(extend = 50) |>
    eyeris::detransient() |>
    eyeris::interpolate(verbose = FALSE) |> # default 250 -> gap stays NA
    eyeris::lpfilt(plot_freqz = FALSE)

  cols <- colnames(out$timeseries$block_1)
  col <- grep("_interpolate_lpfilt$", cols, value = TRUE)
  expect_length(col, 1)

  v <- out$timeseries$block_1[[col]]
  # the long gap is preserved as NA after filtering
  expect_true(any(is.na(v[6100:6300])))
  # lpfilt actually ran: the vast majority of the signal is finite (filtered)
  expect_gt(mean(is.finite(v)), 0.9)
  expect_true(all(is.finite(v[100:1000])))
})

test_that("full default glassbox steps run end-to-end despite a long gap", {
  # mirrors the default glassbox order: deblink -> detransient -> interpolate
  # -> lpfilt -> zscore; a >250 ms gap must NOT cause lpfilt/zscore to be
  # skipped (regression test for silent step-skipping)
  withr::local_pdf(tempfile()) # absorb lpfilt's par() restore device calls
  gap <- 6000:6399
  out <- load_demo_with_gap(gap) |>
    eyeris::deblink(extend = 50) |>
    eyeris::detransient() |>
    eyeris::interpolate(verbose = FALSE) |>
    eyeris::lpfilt(plot_freqz = FALSE) |>
    eyeris::zscore()

  cols <- colnames(out$timeseries$block_1)
  zcol <- grep("_lpfilt_z$", cols, value = TRUE)
  # zscore ran on the lpfilt'd column (not the pre-filter interpolate column)
  expect_length(zcol, 1)

  v <- out$timeseries$block_1[[zcol]]
  expect_true(any(is.na(v[6100:6300]))) # gap preserved through to final output
  expect_gt(mean(is.finite(v)), 0.9)
})

test_that("downsample resamples around long interpolation gaps", {
  gap <- 6000:6399
  out <- load_demo_with_gap(gap) |>
    eyeris::deblink(extend = 50) |>
    eyeris::detransient() |>
    eyeris::interpolate(verbose = FALSE) |>
    eyeris::downsample(target_fs = 100, plot_freqz = FALSE)

  cols <- colnames(out$timeseries$block_1)
  col <- grep("_interpolate_downsample$", cols, value = TRUE)
  expect_length(col, 1)

  v <- out$timeseries$block_1[[col]]
  expect_true(any(is.na(v))) # gap preserved in decimated output
  expect_gt(sum(is.finite(v)), 0) # rest of signal resampled
})

test_that("bin aggregates around long interpolation gaps", {
  gap <- 6000:6399
  out <- load_demo_with_gap(gap) |>
    eyeris::deblink(extend = 50) |>
    eyeris::detransient() |>
    eyeris::interpolate(verbose = FALSE) |>
    eyeris::bin(bins_per_second = 10, method = "mean")

  cols <- colnames(out$timeseries$block_1)
  col <- grep("_interpolate_bin$", cols, value = TRUE)
  expect_length(col, 1)

  v <- out$timeseries$block_1[[col]]
  expect_true(any(is.na(v))) # fully-missing bins remain NA
  expect_false(any(is.nan(v))) # NA sentinel, not NaN (consistent across methods)
  expect_gt(sum(is.finite(v)), 0)
})

test_that("detrend fits around long interpolation gaps and preserves them", {
  gap <- 6000:6399
  out <- load_demo_with_gap(gap) |>
    eyeris::deblink(extend = 50) |>
    eyeris::detransient() |>
    eyeris::interpolate(verbose = FALSE) |>
    eyeris::detrend()

  cols <- colnames(out$timeseries$block_1)
  col <- grep("_interpolate_detrend$", cols, value = TRUE)
  expect_length(col, 1)

  v <- out$timeseries$block_1[[col]]
  # output length is aligned with input (na.exclude pads), gap preserved as NA
  expect_equal(length(v), nrow(out$timeseries$block_1))
  expect_true(any(is.na(v[6100:6300])))
  expect_gt(mean(is.finite(v)), 0.9)
})

test_that("lpfilt still guards against NAs when interpolation was not run", {
  withr::local_pdf(tempfile()) # absorb lpfilt's par() restore device calls
  # deblink leaves NA blinks; without interpolation upstream, lpfilt should
  # still tell the user to interpolate first (prev_op has no 'interpolate')
  out <- load_demo_with_gap() |>
    eyeris::deblink(extend = 50)
  expect_error(eyeris::lpfilt(out, plot_freqz = FALSE), "interpolate")
})

test_that("glassbox interpolate=list(max_gap_ms=NULL) disables the limit", {
  demo_data <- eyeris::eyelink_asc_demo_dataset()
  g_null <- eyeris::glassbox(
    demo_data,
    interpolate = list(max_gap_ms = NULL),
    lpfilt = FALSE,
    zscore = FALSE,
    verbose = FALSE
  )
  # NULL is normalized to Inf (interpolate across all gaps)
  expect_equal(g_null$params$interpolate$parameters$max_gap_ms, Inf)
})

test_that("behavior-change notice is emitted only once per session", {
  # the session state lives in an internal package environment (mutated by
  # reference); bind it locally to avoid assigning into a namespaced object
  sess <- eyeris:::.eyeris_session
  sess$max_gap_notified <- NULL

  demo_data <- eyeris::eyelink_asc_demo_dataset()
  data <- eyeris::load_asc(demo_data)

  # verbose = FALSE should not set the flag
  invisible(eyeris::interpolate(data, verbose = FALSE))
  expect_null(sess$max_gap_notified)

  # first verbose call sets the flag (notice shown once)
  invisible(eyeris::interpolate(data, verbose = TRUE))
  expect_true(isTRUE(sess$max_gap_notified))

  # cleanup so the notice can fire again in interactive sessions / other tests
  sess$max_gap_notified <- NULL
})
