# build a single-block timeseries data frame matching the load_asc schema
make_block <- function(times, pupil = NULL, hz = 1000L) {
  if (is.null(pupil)) {
    pupil <- seq_along(times) + 0
  }
  data.frame(
    block = 1L,
    time_orig = times,
    time_secs = (times - times[1]) / 1000,
    time_scaled = (times - times[1]) / 1000,
    pupil_raw = pupil,
    eye_x = pupil + 100,
    eye_y = pupil + 200,
    eye = "L",
    hz = hz,
    type = "area",
    stringsAsFactors = FALSE
  )
}

make_eyeris <- function(timeseries_list) {
  obj <- list(
    timeseries = timeseries_list,
    latest = stats::setNames(
      as.list(rep("pupil_raw", length(timeseries_list))),
      names(timeseries_list)
    ),
    info = list(sample.rate = 1000, screen.x = 1920, screen.y = 1080),
    params = list()
  )
  class(obj) <- "eyeris"
  obj
}

test_that("resample_block reconstructs a uniform grid for dropped samples", {
  # samples at t = 4, 5 ms are missing from a 1000 Hz (1 ms) grid
  block <- make_block(c(0, 1, 2, 3, 6, 7, 8), c(10, 11, 12, 13, 16, 17, 18))

  out <- eyeris:::resample_block(block, verbose = FALSE)

  expect_equal(nrow(out), 9)
  expect_equal(out$time_orig, 0:8)
  expect_equal(unique(diff(out$time_orig)), 1)

  expect_equal(which(out$is_resampled), c(5L, 6L))
  expect_equal(sum(out$is_resampled), 2L)
  expect_true(all(is.na(out$pupil_raw[out$is_resampled])))
  expect_true(all(is.na(out$eye_x[out$is_resampled])))

  expect_equal(out$pupil_raw[!out$is_resampled], c(10, 11, 12, 13, 16, 17, 18))
  expect_equal(out$time_secs, (0:8) / 1000)
  expect_true(all(out$eye == "L"))
  expect_true(all(out$hz == 1000))
  expect_true(all(out$type == "area"))
})

test_that("resample_block is a byte-identical no-op on uniform data", {
  block <- make_block(0:9)
  out <- eyeris:::resample_block(block, verbose = FALSE)

  expect_equal(nrow(out), 10)
  expect_equal(out$pupil_raw, block$pupil_raw)
  # zero-impact: no is_resampled column is added for already-uniform data
  expect_false("is_resampled" %in% names(out))
  expect_identical(out, block)
})

test_that("resample_block leaves high-rate integer-ms data untouched", {
  # a 2000 Hz tracker reporting integer-ms timestamps yields 0,1,1,2,2,3,...
  # these are duplicate-timestamp sub-ms samples, NOT dropped samples -- the
  # robust detector must treat them as uniform so we never collapse them
  # (regression test for the 49-of-100-rows data-loss bug)
  times <- cumsum(rep(c(0, 1), 50))
  block <- make_block(times, hz = 2000L)

  out <- eyeris:::resample_block(block, verbose = FALSE)

  expect_equal(nrow(out), 100) # NO data loss
  expect_false("is_resampled" %in% names(out))
  expect_identical(out, block)
})

test_that("resample_block infers the period across sampling rates", {
  # 500 Hz (2 ms spacing) with one dropped sample (missing t = 6)
  block <- make_block(c(0, 2, 4, 8, 10), hz = 500L)
  out <- eyeris:::resample_block(block, verbose = FALSE)

  expect_equal(out$time_orig, c(0, 2, 4, 6, 8, 10))
  expect_equal(sum(out$is_resampled), 1L)
  expect_equal(which(out$is_resampled), 4L)
})

test_that("resample_block anchors on the first regular interval, back-extends, and repairs a later gap despite early jitter", {
  # early intervals jitter (3, 3 ms) before settling to the 4 ms period, then a
  # dropped sample opens an 8 ms gap. The old slot-insertion repair bailed on the
  # jitter (two samples rounding to one grid slot) and left the gap unrepaired;
  # the resampler anchors the grid phase on the first *reliable* 4 ms interval,
  # back-extends the grid to cover the early samples, interpolates the jitter,
  # and NA-fills only the genuinely dropped sample.
  block <- make_block(
    c(0, 3, 6, 10, 14, 18, 26, 30),
    pupil = c(100, 103, 106, 110, 114, 118, 126, 130),
    hz = 250L
  )

  out <- eyeris:::resample_block(block, verbose = FALSE)

  # grid is phase-locked to the reliable 4 ms samples (..., 2, 6, 10, ...) rather
  # than to the first timestamp, and back-extends to g = 2 to represent the early
  # region (anchoring on t[1] = 0 would instead have produced 0, 4, 8, ...)
  expect_equal(out$time_orig, c(2, 6, 10, 14, 18, 22, 26, 30))
  expect_equal(unique(diff(out$time_orig)), 4)

  # exactly one genuinely dropped sample is inserted as NA (at t = 22 ms)
  expect_equal(which(out$is_resampled), 6L)
  expect_equal(sum(out$is_resampled), 1L)
  expect_true(is.na(out$pupil_raw[6]))

  # early jitter is interpolated onto the grid rather than causing a bail-out:
  # the (0, 100) and (3, 103) samples interpolate to 102 at t = 2, and that row
  # is a real (interpolated) value, not a flagged dropped sample
  expect_equal(out$pupil_raw[1], 102)
  expect_false(out$is_resampled[1])

  # genuine on-grid samples are preserved verbatim
  expect_equal(
    out$pupil_raw[!out$is_resampled],
    c(102, 106, 110, 114, 118, 126, 130)
  )
})

test_that("resample_block preserves off-grid source NAs instead of interpolating over them", {
  # a missing pupil sample sits at an off-grid (jittered) timestamp (t = 3 ms),
  # which feeds the interpolation onto grid point t = 2 ms. resample() must not
  # silently fill that missing observation -- it should stay NA for interpolate()
  # to handle. Regression test: stats::approx() defaults to na.rm = TRUE, which
  # would interpolate across the source NA (filling t = 2 with 102).
  block <- make_block(
    c(0, 3, 6, 10, 14, 18, 26, 30),
    pupil = c(100, NA, 106, 110, 114, 118, 126, 130),
    hz = 250L
  )

  out <- eyeris:::resample_block(block, verbose = FALSE)

  expect_equal(out$time_orig, c(2, 6, 10, 14, 18, 22, 26, 30))

  # the grid point fed by the off-grid missing sample stays NA (not filled to 102)
  expect_true(is.na(out$pupil_raw[1]))
  expect_true(is.na(out$eye_x[1])) # applies to every resampled data channel
  # ... and it is a preserved missing observation, not an inserted dropped sample
  expect_false(out$is_resampled[1])

  # the genuine dropped sample (t = 22 ms gap) is still the only inserted NA row
  expect_equal(which(out$is_resampled), 6L)

  # exact on-grid samples are unchanged by the on_sample restoration
  expect_equal(
    out$pupil_raw[c(2, 3, 4, 5, 7, 8)],
    c(106, 110, 114, 118, 126, 130)
  )
})

test_that("resample_block turns large gaps into NA rows without an inflation guard", {
  # a 400 ms gap on a 1 ms grid inserts ~400 NA rows: under the two-stage
  # resampler a long gap is represented as missing data for interpolate() to
  # decide on, rather than being refused by a max_inflation guard
  block <- make_block(c(0, 1, 2, 403, 404, 405))

  out <- eyeris:::resample_block(block, verbose = FALSE)
  expect_equal(nrow(out), 406)
  expect_equal(out$time_orig, 0:405)
  expect_equal(sum(out$is_resampled), 400L)
  expect_true(all(is.na(out$pupil_raw[out$is_resampled])))

  # the observed samples are preserved verbatim on the grid
  expect_equal(out$pupil_raw[!out$is_resampled], c(1, 2, 3, 4, 5, 6))

  # no "skipping" warning is emitted (the guard was removed); an informational
  # message about the resampling is logged instead
  msgs <- testthat::capture_messages(eyeris:::resample_block(
    block,
    verbose = TRUE
  ))
  expect_false(any(grepl("Skipping", msgs)))
  expect_true(any(grepl("Resampled onto a uniform grid", msgs)))
})

test_that("resample_block no-ops on pure systematic decimation", {
  # every-other sample of a nominal 1000 Hz recording dropped -> the survivors
  # form a perfectly uniform 2 ms grid; there is nothing to insert without
  # fabricating data, so leave it untouched (the load-time guardrail warns)
  block <- make_block(seq(0, 200, by = 2), hz = 1000L)
  out <- eyeris:::resample_block(block, verbose = FALSE)
  expect_equal(nrow(out), nrow(block))
  expect_false("is_resampled" %in% names(out))
})

test_that("resample_block handles degenerate inputs gracefully", {
  expect_equal(
    nrow(eyeris:::resample_block(make_block(0:1), verbose = FALSE)),
    2
  )
  expect_silent(eyeris:::resample_block(
    make_block(c(0, NA, 2, 3)),
    verbose = FALSE
  ))
  out <- eyeris:::resample_block(make_block(c(0, 0, 0)), verbose = FALSE)
  expect_equal(nrow(out), 3)
})

test_that("resample processes each block independently and records params", {
  obj <- make_eyeris(list(
    block_1 = make_block(c(0, 1, 2, 3, 6, 7, 8)), # gap -> resample
    block_2 = make_block(0:9) # uniform -> no-op
  ))

  out <- eyeris:::resample(obj, verbose = FALSE)

  expect_equal(nrow(out$timeseries$block_1), 9)
  expect_equal(sum(out$timeseries$block_1$is_resampled), 2L)
  expect_equal(nrow(out$timeseries$block_2), 10)
  expect_false("is_resampled" %in% names(out$timeseries$block_2))

  # provenance recorded because at least one block was resampled
  expect_false(is.null(out$params$resample))

  # but a fully-uniform object is a true no-op: nothing recorded
  uniform_obj <- make_eyeris(list(block_1 = make_block(0:9)))
  out_uniform <- eyeris:::resample(uniform_obj, verbose = FALSE)
  expect_false("resample" %in% names(out_uniform$params))
  expect_false("is_resampled" %in% names(out_uniform$timeseries$block_1))
})

test_that("repeated resample() calls preserve provenance and stay no-ops", {
  obj <- make_eyeris(list(block_1 = make_block(c(0, 1, 2, 3, 6, 7, 8))))

  once <- eyeris:::resample(obj, verbose = FALSE)
  expect_true("is_resampled" %in% names(once$timeseries$block_1))
  expect_false(is.null(once$params$resample))

  # tag the recorded provenance so a rewrite on the second pass is detectable
  once$params$resample$sentinel <- "original"

  twice <- eyeris:::resample(once, verbose = FALSE)

  # a second pass over already-resampled (now-uniform) data is a true no-op ...
  expect_identical(twice$timeseries$block_1, once$timeseries$block_1)
  # ... and the original provenance is preserved rather than overwritten,
  # because `acted` reflects rows changed by this call -- not the persisting
  # `is_resampled` column from the first call
  expect_identical(twice$params$resample$sentinel, "original")
})

test_that("resample recurses into binocular objects", {
  left <- make_eyeris(list(block_1 = make_block(c(0, 1, 2, 5, 6))))
  right <- make_eyeris(list(block_1 = make_block(c(0, 1, 2, 5, 6))))
  left$binocular_mode <- "both"
  right$binocular_mode <- "both"
  binoc <- list(
    left = left,
    right = right,
    original_file = "x",
    raw_binocular_object = -1
  )
  class(binoc) <- "eyeris"

  expect_true(eyeris:::is_binocular_object(binoc))

  out <- eyeris:::resample(binoc, verbose = FALSE)
  expect_equal(nrow(out$left$timeseries$block_1), 7)
  expect_equal(nrow(out$right$timeseries$block_1), 7)
  expect_equal(sum(out$left$timeseries$block_1$is_resampled), 2L)
})

test_that("resample then interpolate fills the reconstructed gaps", {
  short <- eyeris:::resample_block(
    make_block(c(0, 1, 2, 3, 6, 7, 8), c(10, 11, 12, 13, 16, 17, 18)),
    verbose = FALSE
  )
  expect_true(any(is.na(short$pupil_raw)))

  filled <- eyeris:::interpolate_pupil(short, "pupil_raw", verbose = FALSE)
  expect_false(any(is.na(filled)))
  expect_equal(filled, c(10, 11, 12, 13, 14, 15, 16, 17, 18))
})

test_that("confounds report resampled-sample counts", {
  block <- eyeris:::resample_block(
    make_block(c(0, 1, 2, 3, 6, 7, 8)),
    verbose = FALSE
  )
  cf <- eyeris:::get_confounds_for_step(
    pupil_df = block,
    pupil_vec = block$pupil_raw,
    screen_width = 1920,
    screen_height = 1080,
    hz = 1000
  )
  expect_equal(cf$n_resampled, 2)
  expect_equal(cf$prop_resampled, 2 / 9)

  # absent column -> zero, never errors
  cf0 <- eyeris:::get_confounds_for_step(
    pupil_df = make_block(0:9),
    pupil_vec = make_block(0:9)$pupil_raw,
    screen_width = 1920,
    screen_height = 1080,
    hz = 1000
  )
  expect_equal(cf0$n_resampled, 0)
  expect_equal(cf0$prop_resampled, 0)
})

test_that("glassbox auto-resamples by default but no-ops on uniform data", {
  # the bundled demo is uniformly sampled: auto-on must not change anything
  # (suppressWarnings guards an unrelated base-graphics preview warning)
  g <- suppressWarnings(eyeris::glassbox(
    eyelink_asc_demo_dataset(),
    verbose = FALSE
  ))
  b1 <- g$timeseries$block_1
  expect_false("is_resampled" %in% names(b1))
  expect_equal(nrow(b1), 20767)
  # true no-op: no provenance recorded when nothing was resampled
  expect_false("resample" %in% names(g$params))

  # explicit opt-out is accepted and likewise leaves the data unchanged
  g_off <- suppressWarnings(eyeris::glassbox(
    eyelink_asc_demo_dataset(),
    resample = FALSE,
    verbose = FALSE
  ))
  expect_false("is_resampled" %in% names(g_off$timeseries$block_1))
  expect_equal(nrow(g_off$timeseries$block_1), 20767)
})
