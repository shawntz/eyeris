test_that("bin() works as expected", {
  options(warn = 1) # treat warnings as errors to replicate R CMD check behavior
  on.exit(options(warn = 0)) # reset after test

  demo_data <- eyeris::eyelink_asc_demo_dataset()
  data <- eyeris::load_asc(demo_data)
  data <- eyeris::interpolate(data)

  # test bin() --------------------------------------------------------------
  bins_per_second <- 10
  binned <- eyeris::bin(
    data,
    bins_per_second = bins_per_second,
    method = "mean"
  )
  expect_equal(binned$decimated.sample.rate, bins_per_second)
  expect_lt(nrow(binned$timeseries$block_1), nrow(data$timeseries$block_1))
  expect_true(!any(is.na(binned$timeseries$block_1$pupil_raw_interpolate_bin)))

  # test glassbox() with bin() ----------------------------------------------
  expect_no_warning({
    gbox_bin <- eyeris::glassbox(
      demo_data,
      deblink = FALSE,
      detransient = FALSE,
      lpfilt = FALSE,
      bin = list(bins_per_second = 10, method = "mean"),
      detrend = FALSE,
      zscore = FALSE,
      verbose = FALSE
    )
  })
  expect_lt(nrow(gbox_bin$timeseries$block_1), nrow(data$timeseries$block_1))
  expect_equal(gbox_bin$decimated.sample.rate, 10)
})
