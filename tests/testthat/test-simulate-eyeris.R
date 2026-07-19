test_that("sim_params() validates inputs and is toggle-able", {
  p <- sim_params()
  expect_s3_class(p, "eyeris_sim_params")
  expect_true(p$blinks)
  expect_false(p$drift)

  # fs must divide 1000 evenly
  expect_error(sim_params(fs = 300), "divide 1000")
  # noise must be > 0 when enabled (else detransient MAD == 0)
  expect_error(sim_params(noise = TRUE, noise_sd = 0), "noise_sd")
  # increasing length-2 ranges
  expect_error(sim_params(phasic_amp = c(300, 100)), "phasic_amp")
  # line frequency must be representable
  expect_error(sim_params(line = TRUE, line_freq = 600, fs = 1000), "line_freq")

  expect_s3_class(
    sim_params(drift = TRUE, drift_slope = 12),
    "eyeris_sim_params"
  )
})

test_that("simulate_eyeris() returns a byte-compatible eyeris object", {
  sim <- simulate_eyeris(
    seed = 1,
    params = sim_params(duration_secs = 15),
    verbose = FALSE
  )
  real <- eyeris::load_asc(
    eyeris::eyelink_asc_demo_dataset(),
    block = 1,
    verbose = FALSE
  )

  expect_s3_class(sim, "eyeris")
  # same top-level structure as a real loaded object
  expect_identical(names(sim), names(real))

  # exact time series column order
  expect_identical(
    names(sim$timeseries$block_1),
    c(
      "block",
      "time_orig",
      "time_secs",
      "time_scaled",
      "eye_x",
      "eye_y",
      "eye",
      "hz",
      "type",
      "pupil_raw"
    )
  )
  # column classes match a real object
  expect_identical(
    sapply(sim$timeseries$block_1, class),
    sapply(real$timeseries$block_1, class)
  )
  # info structure matches a real object
  expect_identical(names(sim$info), names(real$info))
  expect_identical(
    unname(sapply(sim$info, class)),
    unname(sapply(real$info, class))
  )
})

test_that("simulate_eyeris() produces well-formed content", {
  fs <- 1000
  sim <- simulate_eyeris(
    seed = 1,
    params = sim_params(duration_secs = 15, fs = fs),
    verbose = FALSE
  )
  b1 <- sim$timeseries$block_1

  expect_true(is.integer(b1$time_orig))
  expect_true(all(diff(b1$time_orig) > 0))
  expect_equal(b1$time_secs[1], 0)
  expect_identical(unique(b1$type), "diameter")
  expect_identical(sim$info$pupil.dtype, "DIAMETER")
  expect_equal(sim$info$sample.rate, fs)
  expect_equal(unique(b1$hz), fs)
  expect_identical(sim$latest[["block_1"]], "pupil_raw")

  # blink cores are encoded as NA (not literal 0), matching real EyeLink data
  truth <- attr(sim, "sim_truth")
  expect_true(all(is.na(b1$pupil_raw[truth$core_mask])))
  expect_false(any(b1$pupil_raw == 0, na.rm = TRUE))
  # some data is missing, but not most of it
  expect_gt(mean(is.na(b1$pupil_raw)), 0)
  expect_lt(mean(is.na(b1$pupil_raw)), 0.2)
  # blink rows recorded
  expect_equal(nrow(sim$blinks$block_1), 6)
})

test_that("simulate_eyeris() is deterministic and confines the RNG", {
  p <- sim_params(duration_secs = 12)
  a <- simulate_eyeris(seed = 1, params = p, verbose = FALSE)
  b <- simulate_eyeris(seed = 1, params = p, verbose = FALSE)
  d <- simulate_eyeris(seed = 2, params = p, verbose = FALSE)

  expect_identical(
    a$timeseries$block_1$pupil_raw,
    b$timeseries$block_1$pupil_raw
  )
  expect_false(identical(
    a$timeseries$block_1$pupil_raw,
    d$timeseries$block_1$pupil_raw
  ))

  # global RNG state is left untouched
  set.seed(99)
  before <- .Random.seed
  invisible(simulate_eyeris(seed = 7, params = p, verbose = FALSE))
  expect_identical(before, .Random.seed)
})

test_that("simulate_eyeris() degrades gracefully on tiny recordings", {
  # regression: pathologically short recordings must not crash while placing
  # transients (previously threw cryptic base-R sampling/subscript errors)
  expect_no_error(simulate_eyeris(
    seed = 1,
    params = sim_params(duration_secs = 0.1, fs = 1000),
    verbose = FALSE
  ))
  expect_no_error(simulate_eyeris(
    seed = 1,
    params = sim_params(duration_secs = 0.002, fs = 1000),
    verbose = FALSE
  ))

  # non-1000 sampling rates and toggled-off components still work
  expect_s3_class(
    simulate_eyeris(
      seed = 1,
      params = sim_params(duration_secs = 8, fs = 500),
      verbose = FALSE
    ),
    "eyeris"
  )
  expect_no_error(simulate_eyeris(
    seed = 1,
    params = sim_params(
      duration_secs = 8,
      blinks = FALSE,
      transients = FALSE,
      phasic = FALSE,
      hippus = FALSE,
      dropout_frac = 0
    ),
    verbose = FALSE
  ))
})

test_that("simulated data flows through the full eyeris pipeline", {
  sim <- simulate_eyeris(
    seed = 1,
    params = sim_params(duration_secs = 15),
    verbose = FALSE
  )

  # MAD guard: detransient does not abort (noise keeps speed-MAD > 0)
  expect_no_error(suppressWarnings(eyeris::detransient(eyeris::deblink(sim))))

  # canonical chain, including the optional downsample + detrend steps, which
  # require the delegated `block` column to be present
  expect_no_error(suppressWarnings(suppressMessages(
    sim |>
      eyeris::deblink() |>
      eyeris::detransient() |>
      eyeris::interpolate() |>
      eyeris::lpfilt(plot_freqz = FALSE) |>
      eyeris::downsample(target_fs = 100, plot_freqz = FALSE) |>
      eyeris::detrend() |>
      eyeris::zscore()
  )))

  # confound summary + plotting both work on the synthetic object
  out <- suppressWarnings(suppressMessages(
    sim |>
      eyeris::deblink() |>
      eyeris::detransient() |>
      eyeris::interpolate() |>
      eyeris::lpfilt(plot_freqz = FALSE) |>
      eyeris::zscore()
  ))
  expect_no_error(suppressWarnings(eyeris::summarize_confounds(out)))

  pdf(tempfile(fileext = ".pdf"))
  on.exit(dev.off())
  expect_no_error(suppressWarnings(suppressMessages(plot(
    out,
    seed = 1,
    verbose = FALSE
  ))))
})
