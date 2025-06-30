test_that("bin() validates time series monotonicity", {
  # create test data with valid monotonically increasing time series
  valid_data <- data.frame(
    time_orig = seq(0, 10000, by = 100), # original time in ms
    time_secs = seq(0, 10, by = 0.1), # converted time in seconds
    time_scaled = seq(0, 10, by = 0.1), # scaled time
    block = rep(1, 101), # block identifier
    pupil_raw = rnorm(101),
    stringsAsFactors = FALSE
  )

  # create mock eyeris object for testing
  mock_eyeris <- list(
    timeseries = list(block_1 = valid_data),
    latest = list(block_1 = "pupil_raw"),
    info = list(sample.rate = 10)
  )
  class(mock_eyeris) <- "eyeris"

  # test that valid data passes
  expect_no_error({
    result <- eyeris::bin(mock_eyeris, bins_per_second = 5, method = "mean")
  })

  # test with non-monotonic time series (should fail)
  non_monotonic_data <- data.frame(
    time_orig = c(0, 1000, 500, 2000, 3000), # decreasing at index 3
    time_secs = c(0, 1, 0.5, 2, 3), # decreasing at index 3
    time_scaled = c(0, 1, 0.5, 2, 3), # decreasing at index 3
    block = rep(1, 5),
    pupil_raw = rnorm(5),
    stringsAsFactors = FALSE
  )

  mock_eyeris_bad <- list(
    timeseries = list(block_1 = non_monotonic_data),
    latest = list(block_1 = "pupil_raw"),
    info = list(sample.rate = 10)
  )
  class(mock_eyeris_bad) <- "eyeris"

  expect_error(
    {
      eyeris::bin(mock_eyeris_bad, bins_per_second = 5, method = "mean")
    },
    "Time series is not monotonically increasing"
  )

  # test the check_time_monotonic function directly
  # test with empty time vector (should fail)
  expect_error(
    {
      eyeris:::check_time_monotonic(numeric(0))
    },
    "Time vector is NULL or empty"
  )

  # test with single time point (should fail)
  expect_error(
    {
      eyeris:::check_time_monotonic(1)
    },
    "Insufficient non-NA time points to validate monotonicity"
  )

  # test with all NA time values (should fail)
  expect_error(
    {
      eyeris:::check_time_monotonic(rep(NA_real_, 5))
    },
    "Insufficient non-NA time points to validate monotonicity"
  )

  # test with some NA values but valid monotonic sequence
  expect_no_error({
    eyeris:::check_time_monotonic(c(0, NA, 1, 2, NA, 3))
  })

  # test with valid monotonically increasing sequence
  expect_no_error({
    eyeris:::check_time_monotonic(c(0, 1, 2, 3, 4, 5))
  })

  # test with non-monotonic sequence
  expect_error(
    {
      eyeris:::check_time_monotonic(c(0, 1, 0.5, 2, 3))
    },
    "Time series is not monotonically increasing"
  )
})
