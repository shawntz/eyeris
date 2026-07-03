test_that("all pitfalls run and return a well-formed showcase object", {
  for (nm in eyeris:::.pitfall_names()) {
    res <- suppressWarnings(suppressMessages(sim_pitfall(
      nm,
      seed = 1,
      plot = FALSE,
      verbose = FALSE
    )))
    expect_s3_class(res, "eyeris_showcase")
    expect_identical(res$name, nm)
    expect_true(is.data.frame(res$metrics))
    expect_true(all(
      c("metric", "wrong", "right", "better") %in% names(res$metrics)
    ))
    expect_length(res$window, 2)
  }
})

test_that("interp_before_deblink leaves large blink-flank residuals", {
  m <- suppressWarnings(suppressMessages(showcase_metric(
    "interp_before_deblink",
    seed = 1
  )))
  # wrong order preserves the flank spikes; right order removes them
  expect_gt(m$wrong, 150)
  expect_lt(m$right, 50)
  expect_gt(m$wrong, m$right)
})

test_that("lpfilt_before_interp is a hard crash guardrail", {
  res <- suppressWarnings(suppressMessages(sim_pitfall(
    "lpfilt_before_interp",
    seed = 1,
    plot = FALSE,
    verbose = FALSE
  )))
  expect_true(inherits(res$wrong, "condition"))
  expect_match(conditionMessage(res$wrong), "interpolate", ignore.case = TRUE)
  expect_s3_class(res$right, "eyeris")
  expect_equal(res$metrics$wrong, 0)
  expect_equal(res$metrics$right, 1)
})

test_that("lpfilt_before_detransient defeats the transient detector", {
  res <- suppressWarnings(suppressMessages(sim_pitfall(
    "lpfilt_before_detransient",
    seed = 1,
    plot = FALSE,
    verbose = FALSE
  )))
  n_tr <- res$extra$n_transients
  # right order (detransient on raw) flags every transient; wrong order
  # (detransient after low-pass) flags far fewer
  expect_equal(res$metrics$right, n_tr)
  expect_lt(res$metrics$wrong, res$metrics$right)
})

test_that("naive_downsample aliases high-frequency content into the band", {
  m <- suppressWarnings(suppressMessages(showcase_metric(
    "naive_downsample",
    seed = 1
  )))
  # naive decimation leaves a large aliased peak; eyeris downsample removes it
  expect_gt(m$wrong, 10 * m$right)
})

test_that("omit_detrend leaves a residual trend that detrending removes", {
  res <- suppressWarnings(suppressMessages(sim_pitfall(
    "omit_detrend",
    seed = 1,
    plot = FALSE,
    verbose = FALSE
  )))
  # residual slope: wrong (no detrend) is clearly nonzero, right (detrend) ~0
  expect_gt(abs(res$metrics$wrong), abs(res$metrics$right))
  expect_lt(abs(res$metrics$right), 1e-3)
  # detrend recovers roughly the injected drift slope (-8 a.u./s)
  expect_equal(res$extra$recovered_slope, -8, tolerance = 2)

  # control: with no drift, the residual slope is far smaller than when a real
  # drift is present, so detrend is nearly a no-op
  m0 <- suppressWarnings(suppressMessages(showcase_metric(
    "omit_detrend",
    seed = 1,
    params = sim_params(duration_secs = 40, drift = FALSE)
  )))
  expect_lt(abs(m0$wrong), 0.05)
  expect_lt(abs(m0$wrong), abs(res$metrics$wrong))
})

test_that("showcase plotting and step showcase run without error", {
  pdf(tempfile(fileext = ".pdf"))
  on.exit(dev.off())
  expect_no_error(suppressWarnings(suppressMessages(sim_pitfall(
    "interp_before_deblink",
    seed = 1,
    verbose = FALSE
  ))))
  expect_no_error(suppressWarnings(suppressMessages(sim_pitfall(
    "lpfilt_before_interp",
    seed = 1,
    verbose = FALSE
  ))))
  expect_no_error(suppressWarnings(suppressMessages(sim_step_showcase(
    "deblink",
    seed = 1,
    verbose = FALSE
  ))))
})
