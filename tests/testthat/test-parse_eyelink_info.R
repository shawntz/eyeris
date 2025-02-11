test_that("parse_eyelink_info handles new format correctly", {
  result <- parse_eyelink_info("5.50 Jun 16 2022 (EyeLink 1000", NA)
  expect_equal(result$version, "5.50 Jun 16 2022")
  expect_equal(result$model, "EyeLink 1000")
})

test_that("parse_eyelink_info handles non-NA model with EyeLink version", {
  result <- parse_eyelink_info("4.594", "EyeLink 1000")
  expect_equal(result$version, "4.594")
  expect_equal(result$model, "EyeLink 1000")
})
