# test that is_binocular_object function exists
test_that("is_binocular_object function exists", {
  expect_true(exists("is_binocular_object", where = asNamespace("eyeris"), inherits = FALSE))
})

test_that("deblink function uses binocular detection", {
  deblink_source <- capture.output(eyeris:::deblink)
  expect_true(
    any(grepl("is_binocular_object", deblink_source)),
    info = "deblink function should use is_binocular_object for binocular support"
  )
})

test_that("is_binocular_object correctly identifies binocular and non-binocular objects", {
  # mock regular eyeris object
  regular_obj <- list(
    timeseries = list(block_1 = data.frame(pupil_raw = 1:10)),
    info = list(sample.rate = 1000)
  )

  # mock binocular object
  binocular_obj <- list(
    left = list(
      timeseries = list(block_1 = data.frame(pupil_raw = 1:10)),
      info = list(sample.rate = 1000),
      binocular_mode = "both"
    ),
    right = list(
      timeseries = list(block_1 = data.frame(pupil_raw = 1:10)),
      info = list(sample.rate = 1000),
      binocular_mode = "both"
    ),
    original_file = "test.asc"
  )

  regular_result <- eyeris:::is_binocular_object(regular_obj)
  binocular_result <- eyeris:::is_binocular_object(binocular_obj)

  expect_false(regular_result, info = "Regular object should not be identified as binocular")
  expect_true(binocular_result, info = "Binocular object should be correctly identified")
})
