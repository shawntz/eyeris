test_that("load_asc function returns list with expected objects", {
  eye_file <- system.file("extdata", "memory.asc", package = "eyeris")
  result <- eyeris::load_asc(eye_file)

  expected_objects <- c(
    "file",
    "timeseries",
    "events",
    "blinks",
    "info",
    "latest"
  )
  actual_objects <- names(result)

  expect_true(
    all(expected_objects %in% actual_objects),
    info = paste(
      "Not all expected objects in `eyeris`",
      "object are present in the loaded data."
    )
  )
})
