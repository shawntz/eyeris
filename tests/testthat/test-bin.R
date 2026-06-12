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

test_that("bin() preserves full-resolution data for diagnostic plots", {
  # regression test for issue #294: diagnostic plots of pipeline steps that
  # precede binning must be shown at the original (full) sampling rate
  demo_data <- eyeris::eyelink_asc_demo_dataset()

  gbox_bin <- eyeris::glassbox(
    demo_data,
    bin = list(bins_per_second = 50, method = "mean"),
    verbose = FALSE
  )

  decimated <- gbox_bin$timeseries$block_1
  full_res <- gbox_bin$timeseries_pre_decimation$block_1

  expect_true(is.data.frame(full_res))
  expect_gt(nrow(full_res), nrow(decimated))

  # a pre-bin step (deblink) keeps more samples at full resolution
  deblink_col <- "pupil_raw_deblink"
  expect_true(deblink_col %in% names(full_res))
  expect_gt(
    sum(is.finite(full_res[[deblink_col]])),
    sum(is.finite(decimated[[deblink_col]]))
  )

  # the bin column itself is only present in the decimated data
  bin_col <- grep("_bin$", names(decimated), value = TRUE)
  expect_length(bin_col, 1)
  expect_false(bin_col %in% names(full_res))

  # plotting a pre-bin step does not error
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(
    gbox_bin,
    steps = 2,
    preview_window = c(0, max(decimated$time_secs)),
    verbose = FALSE
  ))
})
