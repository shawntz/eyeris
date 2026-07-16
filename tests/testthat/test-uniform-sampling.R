# capture cli warning output and collapse wrapping whitespace so that content
# assertions are robust to the console width used when the message is rendered
captured_warning <- function(expr) {
  msgs <- testthat::capture_messages(expr)
  gsub("\\s+", " ", paste(msgs, collapse = " "))
}

test_that("check_uniform_sampling_intervals passes on uniformly sampled data", {
  # 1000 Hz: consecutive timestamps spaced exactly 1 ms apart
  res_1000 <- eyeris:::check_uniform_sampling_intervals(
    seq(0, 1000, by = 1),
    hz = 1000
  )
  expect_true(res_1000$uniform)
  expect_equal(res_1000$expected_interval, 1)
  expect_equal(res_1000$n_irregular, 0L)
  expect_equal(res_1000$n_missing_samples, 0L)

  # 500 Hz: consecutive timestamps spaced exactly 2 ms apart
  res_500 <- eyeris:::check_uniform_sampling_intervals(
    seq(0, 500, by = 2),
    hz = 500
  )
  expect_true(res_500$uniform)
  expect_equal(res_500$expected_interval, 2)
  expect_equal(res_500$n_irregular, 0L)

  # uniform data should not emit any warning message
  msgs <- testthat::capture_messages(eyeris:::check_uniform_sampling_intervals(
    seq(0, 1000, by = 1),
    hz = 1000
  ))
  expect_length(msgs, 0)
})

test_that("check_uniform_sampling_intervals detects dropped samples", {
  # samples at 5 ms and 6 ms relative to a 1000 Hz grid are missing, leaving a
  # 3 ms gap where two 1 ms samples should have been (dropped-sample quirk)
  dropped <- c(0, 1, 2, 5, 6)

  res <- eyeris:::check_uniform_sampling_intervals(dropped, hz = 1000)
  expect_false(res$uniform)
  expect_equal(res$expected_interval, 1)
  expect_equal(res$n_intervals, 4L)
  expect_equal(res$n_irregular, 1L)
  # one 3 ms gap == two missing 1 ms samples
  expect_equal(res$n_missing_samples, 2L)
  expect_equal(res$prop_irregular, 0.25)

  # informative warning is emitted
  warning_text <- captured_warning(eyeris:::check_uniform_sampling_intervals(
    dropped,
    hz = 1000
  ))
  expect_match(warning_text, "Non-uniform sampling intervals detected")
  expect_match(warning_text, "Estimated 2 dropped sample")
})

test_that("check_uniform_sampling_intervals counts multiple dropped samples", {
  # two separate gaps: one 3 ms gap (2 missing) and one 4 ms gap (3 missing)
  multi_gap <- c(0, 1, 2, 5, 6, 7, 11, 12)

  res <- eyeris:::check_uniform_sampling_intervals(multi_gap, hz = 1000)
  expect_false(res$uniform)
  expect_equal(res$n_irregular, 2L)
  expect_equal(res$n_missing_samples, 5L)
})

test_that("check_uniform_sampling_intervals detects short irregular intervals", {
  # 250 Hz: expected spacing is 4 ms. Shorter intervals still violate the
  # uniform sampling grid, even though they are not dropped-sample gaps.
  short_intervals <- c(0, 4, 7, 11, 13, 17)

  res <- eyeris:::check_uniform_sampling_intervals(short_intervals, hz = 250)
  expect_false(res$uniform)
  expect_equal(res$expected_interval, 4)
  expect_equal(res$n_irregular, 2L)
  expect_equal(res$n_missing_samples, 0L)

  warning_text <- captured_warning(eyeris:::check_uniform_sampling_intervals(
    short_intervals,
    hz = 250
  ))
  expect_match(warning_text, "Non-uniform sampling intervals detected")
  expect_match(warning_text, "0 longer")
  expect_match(warning_text, "2 shorter")
})

test_that("check_uniform_sampling_intervals infers interval without hz", {
  # no hz supplied: expected interval is inferred from the modal interval
  dropped <- c(0, 1, 2, 5, 6)

  res <- eyeris:::check_uniform_sampling_intervals(dropped)
  expect_false(res$uniform)
  expect_equal(res$expected_interval, 1)
  expect_equal(res$n_missing_samples, 2L)

  # nominal rate is reported as inferred when hz is absent
  warning_text <- captured_warning(eyeris:::check_uniform_sampling_intervals(
    dropped
  ))
  expect_match(warning_text, "inferred from data")
})

test_that("check_uniform_sampling_intervals checks blocks independently", {
  # a large gap that coincides with a recording-segment boundary must NOT be
  # flagged as a dropped sample when the segments are checked independently
  time_vector <- c(0, 1, 2, 3, 1000, 1001, 1002)
  blocks <- c(1, 1, 1, 1, 2, 2, 2)

  # without block grouping, the boundary gap is (incorrectly) flagged
  res_no_blocks <- eyeris:::check_uniform_sampling_intervals(
    time_vector,
    hz = 1000
  )
  expect_false(res_no_blocks$uniform)

  # with block grouping, each uniformly sampled segment passes
  res_blocks <- eyeris:::check_uniform_sampling_intervals(
    time_vector,
    hz = 1000,
    blocks = blocks
  )
  expect_length(res_blocks, 2)
  expect_true(all(vapply(res_blocks, function(z) z$uniform, logical(1))))

  # a dropped sample *within* a block is still flagged, and the warning names
  # the offending block
  dropped_in_block <- c(0, 1, 2, 3, 1000, 1001, 1004)
  res_drop <- eyeris:::check_uniform_sampling_intervals(
    dropped_in_block,
    hz = 1000,
    blocks = blocks
  )
  expect_true(res_drop[[1]]$uniform)
  expect_false(res_drop[[2]]$uniform)

  warning_text <- captured_warning(eyeris:::check_uniform_sampling_intervals(
    dropped_in_block,
    hz = 1000,
    blocks = blocks
  ))
  expect_match(warning_text, "block_2")
})

test_that("check_uniform_sampling_intervals errors on mismatched blocks length", {
  expect_error(
    eyeris:::check_uniform_sampling_intervals(
      c(0, 1, 2),
      hz = 1000,
      blocks = c(1, 1)
    ),
    "must be the same length"
  )
})

test_that("check_uniform_sampling_intervals handles degenerate inputs", {
  # too few points to form an interval
  expect_true(eyeris:::check_uniform_sampling_intervals(numeric(0))$uniform)
  expect_true(eyeris:::check_uniform_sampling_intervals(5)$uniform)
  expect_true(
    eyeris:::check_uniform_sampling_intervals(rep(NA_real_, 5))$uniform
  )

  # NA timestamps are dropped before the check
  res_na <- eyeris:::check_uniform_sampling_intervals(
    c(0, 1, NA, 3, 4, 5),
    hz = 1000
  )
  # removing the NA at t=2 leaves a 2 ms gap == one missing sample
  expect_false(res_na$uniform)
  expect_equal(res_na$n_missing_samples, 1L)

  # verbose = FALSE suppresses the warning message
  msgs <- testthat::capture_messages(eyeris:::check_uniform_sampling_intervals(
    c(0, 1, 2, 5, 6),
    hz = 1000,
    verbose = FALSE
  ))
  expect_length(msgs, 0)
})

test_that("check_uniform_sampling_intervals errors when interval cannot be inferred", {
  expect_error(
    eyeris:::check_uniform_sampling_intervals(c(0, Inf), hz = 1000),
    "Unable to infer expected sampling interval"
  )
})

test_that("check_uniform_sampling_intervals tolerates sub-millisecond rounding", {
  # high-rate trackers may report integer-ms timestamps for sub-ms samples,
  # producing alternating 0/1 ms intervals; these must not be flagged as drops
  # nor as a (false) effective-rate mismatch, because the duplicate timestamps
  # (zero-length intervals) reveal the finer-than-reported resolution
  rounded <- cumsum(rep(c(0, 1), 50))

  res <- eyeris:::check_uniform_sampling_intervals(rounded, hz = 2000)
  expect_true(res$uniform)
  expect_equal(res$n_irregular, 0L)
  expect_false(res$rate_mismatch)
})

test_that("check_uniform_sampling_intervals detects systematic dropout via hz", {
  # every other sample of a 1000 Hz recording is missing: the survivors form a
  # perfectly uniform 2 ms grid, so the gap check sees nothing, but the nominal
  # 1000 Hz rate reveals the effective rate is only half of what it should be
  decimated <- seq(0, 200, by = 2)

  res <- eyeris:::check_uniform_sampling_intervals(decimated, hz = 1000)
  expect_false(res$uniform)
  expect_true(res$rate_mismatch)
  expect_equal(res$n_irregular, 0L) # no sporadic gaps, the grid is uniform
  expect_equal(res$expected_interval, 2)

  warning_text <- captured_warning(eyeris:::check_uniform_sampling_intervals(
    decimated,
    hz = 1000
  ))
  expect_match(warning_text, "Effective sampling rate")
  expect_match(warning_text, "systematic sample dropout")

  # without a known nominal rate there is nothing to compare against, so a
  # uniformly (if coarsely) sampled grid must NOT be flagged
  res_no_hz <- eyeris:::check_uniform_sampling_intervals(decimated)
  expect_true(res_no_hz$uniform)
  expect_false(res_no_hz$rate_mismatch)
})

test_that("check_uniform_sampling_intervals does not flag clock jitter as dropout", {
  # an effective rate within tolerance of the nominal rate is not a mismatch
  jittered <- seq(0, 1000, by = 1)
  res <- eyeris:::check_uniform_sampling_intervals(jittered, hz = 1001)
  expect_true(res$uniform)
  expect_false(res$rate_mismatch)
})

test_that("check_uniform_sampling_intervals summary always carries rate_mismatch", {
  # the field must be present on every return path, including degenerate input
  expect_false(
    eyeris:::check_uniform_sampling_intervals(numeric(0))$rate_mismatch
  )
  expect_false(
    eyeris:::check_uniform_sampling_intervals(
      seq(0, 10),
      hz = 1000
    )$rate_mismatch
  )
})

test_that("modal_value returns the most frequent finite value", {
  expect_equal(eyeris:::modal_value(c(1, 1, 1, 2, 3)), 1)
  expect_equal(eyeris:::modal_value(c(2, 2, 4, 4, 4, NA, Inf)), 4)
  expect_true(is.na(eyeris:::modal_value(numeric(0))))
  expect_true(is.na(eyeris:::modal_value(c(NA_real_, NA_real_))))
})

test_that("load_asc does not warn on the uniformly sampled demo dataset", {
  # the bundled demo dataset is uniformly sampled, so it must load cleanly
  demo <- eyelink_asc_demo_dataset()
  msgs <- testthat::capture_messages(load_asc(demo, block = 1, verbose = TRUE))
  expect_false(any(grepl("Non-uniform sampling intervals", msgs)))
})
