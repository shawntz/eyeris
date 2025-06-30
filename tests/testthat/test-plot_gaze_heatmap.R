test_that("gaze heatmap function works correctly", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("viridis")

  sample_data <- data.frame(
    eye_x = rnorm(1000, mean = 500, sd = 100),
    eye_y = rnorm(1000, mean = 300, sd = 80),
    time_secs = seq(0, 10, length.out = 1000),
    pupil_raw = rnorm(1000, mean = 1000, sd = 200)
  )

  # test plot_gaze_heatmap function
  expect_silent({
    plot_gaze_heatmap(
      eyeris = sample_data,
      screen_width = 1024,
      screen_height = 768,
      n_bins = 30,
      col_palette = "viridis"
    )
  })

  # test with different color palettes
  expect_silent({
    plot_gaze_heatmap(
      eyeris = sample_data,
      screen_width = 1024,
      screen_height = 768,
      col_palette = "plasma"
    )
  })

  # test with missing eye coordinates
  sample_data_no_coords <- sample_data[, c("time_secs", "pupil_raw")]
  expect_warning({
    plot_gaze_heatmap(
      eyeris = sample_data_no_coords,
      screen_width = 1024,
      screen_height = 768
    )
  })

  # test with all NA coordinates
  sample_data_na <- sample_data
  sample_data_na$eye_x <- NA
  sample_data_na$eye_y <- NA
  expect_warning({
    plot_gaze_heatmap(
      eyeris = sample_data_na,
      screen_width = 1024,
      screen_height = 768
    )
  })
})
