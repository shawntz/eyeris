test_that("downsample and bin work as expected", {
  demo_data <- eyeris::eyelink_asc_demo_dataset()
  data <- eyeris::load_asc(demo_data)
  data <- eyeris::interpolate(data)

  # test downsampling
  target_fs <- 500
  downsampled <- eyeris::downsample(
    data,
    target_fs = target_fs,
    plot_freqz = FALSE
  )
  expect_equal(downsampled$info$decimated.sample.rate, target_fs)
  expect_lt(nrow(downsampled$timeseries$block_1), nrow(data$timeseries$block_1))
  expect_true(!any(is.na(downsampled$timeseries$block_1)))

  # test binning
  bins_per_second <- 10
  binned <- eyeris::bin(
    data,
    bins_per_second = bins_per_second,
    method = "mean"
  )
  expect_equal(binned$info$binning.sample.rate, bins_per_second)
  expect_lt(nrow(binned$timeseries$block_1), nrow(data$timeseries$block_1))
  expect_true(!any(is.na(binned$timeseries$block_1)))

  # test glassbox with downsampling
  expect_silent({
    gbox_down <- eyeris::glassbox(
      demo_data,
      downsample = list(target_fs = 500, plot_freqz = FALSE),
      detrend = FALSE,
      zscore = FALSE
    )
    expect_lt(nrow(gbox_down$timeseries$block_1), nrow(data$timeseries$block_1))
    expect_equal(gbox_down$info$decimated.sample.rate, 500)
  })

  # test glassbox with binning
  expect_silent({
    gbox_bin <- eyeris::glassbox(
      demo_data,
      bin = list(bins_per_second = 10, method = "mean"),
      detrend = FALSE,
      zscore = FALSE
    )
    expect_lt(nrow(gbox_bin$timeseries$block_1), nrow(data$timeseries$block_1))
    expect_equal(gbox_bin$info$binning.sample.rate, 10)
  })
})
