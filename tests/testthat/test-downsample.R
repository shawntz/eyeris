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

test_that("downsample() preserves full-resolution data for diagnostic plots", {
  # regression test for issue #294: diagnostic plots of pipeline steps that
  # precede downsampling must be shown at the original (full) sampling rate,
  # not at the decimated rate
  demo_data <- eyeris::eyelink_asc_demo_dataset()

  gbox_down <- eyeris::glassbox(
    demo_data,
    downsample = list(target_fs = 100),
    verbose = FALSE
  )

  decimated <- gbox_down$timeseries$block_1
  full_res <- gbox_down$timeseries_pre_decimation$block_1

  # full-resolution copy should exist and retain the original sampling rate
  expect_true(is.data.frame(full_res))
  expect_gt(nrow(full_res), nrow(decimated))
  expect_equal(
    round(nrow(full_res) / nrow(decimated)),
    gbox_down$info$sample.rate / 100
  )

  # a pre-downsample step (deblink) keeps far more samples at full resolution
  deblink_col <- "pupil_raw_deblink"
  expect_true(deblink_col %in% names(full_res))
  expect_gt(
    sum(is.finite(full_res[[deblink_col]])),
    sum(is.finite(decimated[[deblink_col]]))
  )

  # the downsample column itself is only present in the decimated data
  ds_col <- grep("_downsample$", names(decimated), value = TRUE)
  expect_length(ds_col, 1)
  expect_false(ds_col %in% names(full_res))

  # when plotting, pre-downsample steps are drawn at full resolution while
  # the downsample step is drawn at the decimated resolution
  rec <- new.env()
  rec$lens <- numeric(0)
  testthat::local_mocked_bindings(robust_plot = function(y, x = NULL, ...) {
    rec$lens <- c(rec$lens, length(y))
    invisible(NULL)
  })

  pupil_steps <- grep("^pupil_", names(decimated), value = TRUE)
  deblink_idx <- which(pupil_steps == deblink_col)
  ds_idx <- grep("_downsample$", pupil_steps)
  max_t <- max(decimated$time_secs)

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  rec$lens <- numeric(0)
  plot(
    gbox_down,
    steps = deblink_idx,
    preview_window = c(0, max_t),
    verbose = FALSE
  )
  deblink_points <- utils::tail(rec$lens, 1)

  rec$lens <- numeric(0)
  plot(gbox_down, steps = ds_idx, preview_window = c(0, max_t), verbose = FALSE)
  ds_points <- utils::tail(rec$lens, 1)

  expect_gt(deblink_points, ds_points * 5)
})
