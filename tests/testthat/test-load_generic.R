# helpers ---------------------------------------------------------------------

make_samples <- function(n = 1000, hz = 1000, with_gaze = TRUE) {
  df <- data.frame(
    time = seq(0, by = 1000 / hz, length.out = n),
    pupil = 1000 + cumsum(stats::rnorm(n, 0, 2))
  )
  if (with_gaze) {
    df$eye_x <- 960 + stats::rnorm(n, 0, 5)
    df$eye_y <- 540 + stats::rnorm(n, 0, 5)
  }
  df
}

make_events <- function() {
  data.frame(
    time = c(100, 500, 900),
    text = c("START", "PROBE", "END"),
    stringsAsFactors = FALSE
  )
}

# structure -------------------------------------------------------------------

test_that("load_generic returns a valid eyeris object with expected slots", {
  set.seed(1)
  result <- load_generic(
    pupil = make_samples(),
    events = make_events(),
    sample_rate = 1000,
    verbose = FALSE
  )

  expect_s3_class(result, "eyeris")

  expected_objects <- c(
    "file",
    "timeseries",
    "events",
    "blinks",
    "info",
    "latest"
  )
  expect_true(all(expected_objects %in% names(result)))
})

test_that("timeseries has the canonical eyeris columns", {
  set.seed(1)
  result <- load_generic(
    pupil = make_samples(),
    sample_rate = 1000,
    verbose = FALSE
  )

  ts <- result$timeseries$block_1
  expect_true(is.data.frame(ts))
  expect_true(all(
    c(
      "block",
      "time_orig",
      "time_secs",
      "time_scaled",
      "eye_x",
      "eye_y",
      "eye",
      "hz",
      "type",
      "pupil_raw"
    ) %in%
      names(ts)
  ))
  # time normalized to start at 0 seconds
  expect_equal(ts$time_secs[1], 0)
  # time_scaled mirrors time_secs at load
  expect_equal(ts$time_scaled, ts$time_secs)
})

test_that("info is populated with the minimal required metadata", {
  result <- load_generic(
    pupil = make_samples(),
    sample_rate = 250,
    eye = "R",
    pupil_type = "diameter",
    screen_width = 1024,
    screen_height = 768,
    tracker = "my-tracker",
    model = "model-x",
    verbose = FALSE
  )

  expect_equal(result$info$sample.rate, 250)
  expect_false(result$info$mono == FALSE) # mono TRUE for single eye
  expect_true(result$info$right)
  expect_false(result$info$left)
  expect_equal(result$info$pupil.dtype, "diameter")
  expect_equal(result$info$screen.x, 1024)
  expect_equal(result$info$screen.y, 768)
  expect_equal(result$info$version, "my-tracker")
  expect_equal(result$info$model, "model-x")
})

test_that("latest pointer initializes to pupil_raw", {
  result <- load_generic(
    pupil = make_samples(),
    sample_rate = 1000,
    verbose = FALSE
  )
  expect_equal(result$latest$block_1, "pupil_raw")
  expect_false(result$binocular)
})

# glassbox compatibility ------------------------------------------------------

test_that("load_generic object runs through the full glassbox pipeline", {
  set.seed(2)
  samples <- make_samples(n = 2000)
  samples$pupil[500:560] <- NA # blink-like gap
  samples$pupil[1200:1230] <- 0 # zero-coded missing

  eye <- load_generic(
    pupil = samples,
    events = make_events(),
    sample_rate = 1000,
    screen_width = 1920,
    screen_height = 1080,
    verbose = FALSE
  )

  res <- glassbox(eye, lpfilt = list(plot_freqz = FALSE), verbose = FALSE)

  expect_s3_class(res, "eyeris")
  final_col <- res$latest$block_1
  expect_true(grepl("^pupil_raw", final_col))
  expect_true(final_col %in% names(res$timeseries$block_1))
  # deblink should have NA-padded the missing region
  expect_true(any(is.na(res$timeseries$block_1$pupil_raw_deblink)))
})

test_that("preloaded eyeris object never loads a file, even with load_asc = TRUE", {
  set.seed(21)
  eye <- load_generic(
    pupil = make_samples(n = 1000),
    events = make_events(),
    sample_rate = 1000,
    verbose = FALSE
  )

  called <- FALSE
  testthat::local_mocked_bindings(
    load_asc = function(...) {
      called <<- TRUE
      stop("load_asc() must not run for a preloaded eyeris object")
    },
    .package = "eyeris"
  )

  # a caller-supplied `load_asc = TRUE` must not restore file loading
  res <- glassbox(
    eye,
    load_asc = TRUE,
    lpfilt = list(plot_freqz = FALSE),
    verbose = FALSE
  )

  expect_false(called)
  expect_s3_class(res, "eyeris")
  expect_true("block_1" %in% names(res$timeseries))
})

# gaze handling ---------------------------------------------------------------

test_that("pupil-only data (no gaze) still produces eye_x / eye_y columns", {
  set.seed(3)
  eye <- load_generic(
    pupil = make_samples(with_gaze = FALSE),
    sample_rate = 1000,
    verbose = FALSE
  )

  ts <- eye$timeseries$block_1
  expect_true(all(c("eye_x", "eye_y") %in% names(ts)))
  expect_true(all(is.na(ts$eye_x)))
  expect_true(all(is.na(ts$eye_y)))

  # and it still survives glassbox (which runs gaze-based confounds)
  res <- glassbox(eye, lpfilt = list(plot_freqz = FALSE), verbose = FALSE)
  expect_s3_class(res, "eyeris")
})

test_that("gaze supplied as a separate data frame is merged onto pupil", {
  set.seed(4)
  n <- 500
  pupil <- data.frame(
    time = seq(0, by = 1, length.out = n),
    pupil = 1000 + stats::rnorm(n)
  )
  gaze <- data.frame(
    time = seq(0, by = 1, length.out = n),
    eye_x = 960 + stats::rnorm(n),
    eye_y = 540 + stats::rnorm(n)
  )

  eye <- load_generic(
    pupil = pupil,
    gaze = gaze,
    sample_rate = 1000,
    verbose = FALSE
  )

  ts <- eye$timeseries$block_1
  expect_false(all(is.na(ts$eye_x)))
  expect_false(all(is.na(ts$eye_y)))
  expect_equal(nrow(ts), n)
})

# events handling -------------------------------------------------------------

test_that("events default to empty tables when not supplied", {
  eye <- load_generic(
    pupil = make_samples(),
    sample_rate = 1000,
    verbose = FALSE
  )
  expect_true(is.data.frame(eye$events$block_1))
  expect_equal(nrow(eye$events$block_1), 0)
})

test_that("blinks are stored as the third standardized data frame", {
  set.seed(9)
  blinks <- data.frame(stime = c(120, 640), etime = c(180, 700))
  eye <- load_generic(
    pupil = make_samples(n = 1000),
    events = make_events(),
    blinks = blinks,
    sample_rate = 1000,
    verbose = FALSE
  )
  expect_true(is.data.frame(eye$blinks$block_1))
  expect_true(all(c("stime", "etime") %in% names(eye$blinks$block_1)))
  expect_equal(nrow(eye$blinks$block_1), 2)
  expect_equal(eye$blinks$block_1$stime, c(120, 640))
})

test_that("blinks default to empty tables when not supplied", {
  eye <- load_generic(
    pupil = make_samples(),
    sample_rate = 1000,
    verbose = FALSE
  )
  expect_true(is.data.frame(eye$blinks$block_1))
  expect_equal(nrow(eye$blinks$block_1), 0)
})

test_that("text_unique is added to events", {
  events <- data.frame(
    time = c(100, 200, 300),
    text = c("TRIAL", "TRIAL", "TRIAL"),
    stringsAsFactors = FALSE
  )
  eye <- load_generic(
    pupil = make_samples(),
    events = events,
    sample_rate = 1000,
    verbose = FALSE
  )
  expect_true("text_unique" %in% names(eye$events$block_1))
  expect_equal(length(unique(eye$events$block_1$text_unique)), 3)
})

# block handling --------------------------------------------------------------

test_that("block = NULL omits the block column", {
  eye <- load_generic(
    pupil = make_samples(),
    sample_rate = 1000,
    block = NULL,
    verbose = FALSE
  )
  expect_false("block" %in% names(eye$timeseries$block_1))
})

test_that("block = numeric assigns the requested block number", {
  eye <- load_generic(
    pupil = make_samples(),
    sample_rate = 1000,
    block = 3,
    verbose = FALSE
  )
  expect_true("block_3" %in% names(eye$timeseries))
  expect_equal(unique(eye$timeseries$block_3$block), 3)
})

test_that("block = 'auto' splits multi-block data and assigns events by time", {
  set.seed(5)
  s1 <- make_samples(n = 500)
  s2 <- make_samples(n = 500)
  s2$time <- s2$time + 1e6 # second block far in the future
  s1$block <- 1
  s2$block <- 2
  samples <- rbind(s1, s2)

  events <- data.frame(
    time = c(100, 1e6 + 100), # one event per block, no block column
    text = c("A", "B"),
    stringsAsFactors = FALSE
  )

  eye <- load_generic(
    pupil = samples,
    events = events,
    sample_rate = 1000,
    verbose = FALSE
  )

  expect_equal(sort(names(eye$timeseries)), c("block_1", "block_2"))
  # events should have been routed to their respective blocks by timestamp
  expect_equal(eye$events$block_1$text, "A")
  expect_equal(eye$events$block_2$text, "B")

  res <- glassbox(eye, lpfilt = list(plot_freqz = FALSE), verbose = FALSE)
  expect_equal(sort(names(res$timeseries)), c("block_1", "block_2"))
})

test_that("explicit block column on events overrides timestamp routing", {
  set.seed(15)
  s1 <- make_samples(n = 300)
  s2 <- make_samples(n = 300)
  s2$time <- s2$time + 1e6
  s1$block <- 1
  s2$block <- 2
  samples <- rbind(s1, s2)

  # both timestamps sit inside block 1's range, but the explicit block column
  # says otherwise -- block identity must win over timestamp routing
  events <- data.frame(
    time = c(50, 60),
    text = c("A", "B"),
    block = c(2, 1),
    stringsAsFactors = FALSE
  )

  eye <- load_generic(
    pupil = samples,
    events = events,
    sample_rate = 1000,
    verbose = FALSE
  )

  expect_equal(eye$events$block_1$text, "B")
  expect_equal(eye$events$block_2$text, "A")
})

test_that("separate gaze without a block column is rejected on multi-block data", {
  set.seed(16)
  s1 <- make_samples(n = 300, with_gaze = FALSE)
  s2 <- make_samples(n = 300, with_gaze = FALSE)
  s2$time <- s2$time + 1e6
  s1$block <- 1
  s2$block <- 2
  samples <- rbind(s1, s2)

  gaze <- data.frame(time = c(0, 1e6), eye_x = c(1, 2), eye_y = c(3, 4))

  expect_error(
    load_generic(
      pupil = samples,
      gaze = gaze,
      sample_rate = 1000,
      verbose = FALSE
    ),
    "block"
  )
})

test_that("separate gaze with a block column joins on block + timestamp", {
  set.seed(17)
  s1 <- make_samples(n = 300, with_gaze = FALSE)
  s2 <- make_samples(n = 300, with_gaze = FALSE)
  s2$time <- s2$time + 1e6
  s1$block <- 1
  s2$block <- 2
  samples <- rbind(s1, s2)

  gaze <- rbind(
    data.frame(time = s1$time, eye_x = 100, eye_y = 200, block = 1),
    data.frame(time = s2$time, eye_x = 300, eye_y = 400, block = 2)
  )

  eye <- load_generic(
    pupil = samples,
    gaze = gaze,
    sample_rate = 1000,
    verbose = FALSE
  )

  expect_equal(unique(eye$timeseries$block_1$eye_x), 100)
  expect_equal(unique(eye$timeseries$block_2$eye_x), 300)
})

test_that("event timestamp matching no block range raises an error", {
  set.seed(18)
  s1 <- make_samples(n = 300)
  s2 <- make_samples(n = 300)
  s2$time <- s2$time + 1e6
  s1$block <- 1
  s2$block <- 2
  samples <- rbind(s1, s2)

  # 5e5 sits in the gap between block 1 and block 2 -- matches no range
  events <- data.frame(
    time = c(50, 5e5),
    text = c("A", "B"),
    stringsAsFactors = FALSE
  )

  expect_error(
    load_generic(
      pupil = samples,
      events = events,
      sample_rate = 1000,
      verbose = FALSE
    ),
    "outside every block"
  )
})

# time units & sample rate ----------------------------------------------------

test_that("time_unit = 's' is converted to milliseconds internally", {
  n <- 100
  samples <- data.frame(
    time = seq(0, by = 0.001, length.out = n), # seconds, 1000 Hz
    pupil = rep(1000, n)
  )
  eye <- load_generic(
    pupil = samples,
    sample_rate = 1000,
    time_unit = "s",
    verbose = FALSE
  )
  ts <- eye$timeseries$block_1
  # time_orig stored in ms: last sample ~ 99 ms
  expect_equal(ts$time_orig[n], (n - 1), tolerance = 1e-6)
})

test_that("sample_rate is inferred from timestamps when not supplied", {
  set.seed(6)
  samples <- make_samples(n = 500, hz = 500)
  eye <- load_generic(pupil = samples, verbose = FALSE)
  expect_equal(eye$info$sample.rate, 500)
})

# column mapping --------------------------------------------------------------

test_that("mapping remaps non-standard column names", {
  set.seed(7)
  n <- 300
  samples <- data.frame(
    t_ms = seq(0, by = 1, length.out = n),
    pup = 1000 + stats::rnorm(n)
  )
  eye <- load_generic(
    pupil = samples,
    sample_rate = 1000,
    mapping = list(time = "t_ms", pupil = "pup"),
    verbose = FALSE
  )
  expect_equal(nrow(eye$timeseries$block_1), n)
  expect_true(all(!is.na(eye$timeseries$block_1$pupil_raw)))
})

# error handling --------------------------------------------------------------

test_that("missing required columns raise informative errors", {
  expect_error(load_generic(
    pupil = data.frame(foo = 1:10),
    sample_rate = 1000,
    verbose = FALSE
  ))
  expect_error(load_generic(
    pupil = make_samples(),
    events = data.frame(t = 1, m = "x"), # wrong column names
    sample_rate = 1000,
    verbose = FALSE
  ))
})

# epoching --------------------------------------------------------------------

test_that("a load_generic object can be epoched on event messages", {
  set.seed(8)
  samples <- make_samples(n = 2000)
  events <- data.frame(
    time = c(300, 800, 1300),
    text = c("PROBE_1", "PROBE_2", "PROBE_3"),
    stringsAsFactors = FALSE
  )
  eye <- load_generic(
    pupil = samples,
    events = events,
    sample_rate = 1000,
    verbose = FALSE
  )
  res <- glassbox(eye, lpfilt = list(plot_freqz = FALSE), verbose = FALSE)
  epoched <- epoch(res, events = "PROBE_*", limits = c(-0.1, 0.5))
  expect_s3_class(epoched, "eyeris")
  expect_true(any(grepl("^epoch_", names(epoched))))
})
