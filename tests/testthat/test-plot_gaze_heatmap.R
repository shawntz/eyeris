test_that("gaze heatmap function works correctly", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("viridis")

  sample_data <- eyelink_asc_demo_dataset() |>
    glassbox()

  # test plot_gaze_heatmap function on monocular data
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

  sample_data <- eyelink_asc_binocular_demo_dataset() |>
    glassbox()

  # test plot_gaze_heatmap function on binocular data
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
})
