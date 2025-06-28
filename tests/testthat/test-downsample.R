test_that("downsample() works as expected", {
  options(warn = 1) # treat warnings as errors to replicate R CMD check behavior
  on.exit(options(warn = 0)) # reset after test

  demo_data <- eyeris::eyelink_asc_demo_dataset()
  data <- eyeris::load_asc(demo_data)
  data <- eyeris::interpolate(data)

  # test downsample() -------------------------------------------------------
  target_fs <- 500
  downsampled <- eyeris::downsample(
    data,
    target_fs = target_fs,
    plot_freqz = FALSE
  )
  expect_equal(downsampled$decimated.sample.rate, target_fs)
  expect_lt(nrow(downsampled$timeseries$block_1), nrow(data$timeseries$block_1))
  expect_true(
    !any(is.na(downsampled$timeseries$block_1$pupil_raw_interpolate_downsample))
  )

  # test glassbox() with downsample() ---------------------------------------
  expect_no_warning({
    gbox_down <- eyeris::glassbox(
      demo_data,
      deblink = FALSE,
      detransient = FALSE,
      lpfilt = FALSE,
      downsample = list(target_fs = 500),
      detrend = FALSE,
      zscore = FALSE,
      verbose = FALSE
    )
  })
  expect_lt(nrow(gbox_down$timeseries$block_1), nrow(data$timeseries$block_1))
  expect_equal(gbox_down$decimated.sample.rate, 500)
})
