make_mock_eyeris <- function(n_blocks = 2, params = NULL) {
  if (is.null(params)) {
    params <- list(
      load_asc = list(
        call = quote(glassbox(file)),
        parameters = list(block = "auto", binocular_mode = "average")
      ),
      deblink = list(
        call_stack = quote(deblink(eyeris, extend = 50)),
        parameters = list(extend = 50)
      ),
      detransient = list(parameters = list(n = 16, mad_thresh = NULL)),
      interpolate = list(parameters = list(verbose = TRUE)),
      lpfilt = list(
        parameters = list(wp = 4, ws = 8, rp = 1, rs = 35, plot_freqz = FALSE)
      ),
      z = list(parameters = list())
    )
  }

  timeseries <- stats::setNames(
    lapply(seq_len(n_blocks), function(i) data.frame(time_secs = 1)),
    paste0("block_", seq_len(n_blocks))
  )

  obj <- list(
    file = "sub-001.asc",
    info = list(sample.rate = 1000),
    timeseries = timeseries,
    params = params
  )
  class(obj) <- "eyeris"
  obj
}

test_that("boilerplate() returns a classed, printable markdown string", {
  obj <- make_mock_eyeris()
  bp <- boilerplate(obj)

  expect_s3_class(bp, "eyeris_boilerplate")
  expect_type(unclass(bp), "character")
  expect_length(unclass(bp), 1)

  # print method should cat the text, not the escaped string
  expect_output(print(bp), "Pupillometry data were preprocessed")
})

test_that("boilerplate() errors on non-eyeris input", {
  expect_error(boilerplate(list(a = 1)), "class `eyeris`")
})

test_that("boilerplate() describes each recorded step with actual params", {
  obj <- make_mock_eyeris()
  bp <- as.character(boilerplate(obj))

  expect_match(bp, "EyeLink `.asc`", fixed = TRUE)
  expect_match(bp, "1000 Hz", fixed = TRUE)
  expect_match(bp, "50 ms in both directions", fixed = TRUE)
  expect_match(bp, "n = 16", fixed = TRUE)
  expect_match(bp, "linear interpolation", fixed = TRUE)
  expect_match(bp, "Butterworth low-pass filter", fixed = TRUE)
  expect_match(bp, "passband edge = 4 Hz", fixed = TRUE)
  expect_match(bp, "stopband edge = 8 Hz", fixed = TRUE)
  expect_match(bp, "z-scored", fixed = TRUE)
})

test_that("boilerplate() steps follow canonical pipeline order", {
  obj <- make_mock_eyeris()
  bp <- as.character(boilerplate(obj))

  pos_import <- regexpr("imported from the EyeLink", bp, fixed = TRUE)
  pos_deblink <- regexpr("Blink and missing-data", bp, fixed = TRUE)
  pos_lpfilt <- regexpr("Butterworth low-pass", bp, fixed = TRUE)
  pos_zscore <- regexpr("z-scored", bp, fixed = TRUE)

  expect_true(pos_import < pos_deblink)
  expect_true(pos_deblink < pos_lpfilt)
  expect_true(pos_lpfilt < pos_zscore)
})

test_that("boilerplate() references the per-run JSON metadata sidecar", {
  obj <- make_mock_eyeris()
  bp <- as.character(boilerplate(obj))

  expect_match(bp, "source/logs/run-XX_metadata.json", fixed = TRUE)
  expect_match(bp, "Reproducibility", fixed = TRUE)
})

test_that("boilerplate() includes the CC BY license + attribution note", {
  obj <- make_mock_eyeris()
  bp <- as.character(boilerplate(obj))

  expect_match(bp, "CC BY 4.0", fixed = TRUE)
  expect_match(bp, "creativecommons.org/licenses/by/4.0/", fixed = TRUE)
  expect_match(bp, "safe to", fixed = TRUE)
  expect_match(bp, "copy and paste", fixed = TRUE)
  # attribution is now required via citation
  expect_match(bp, "citing `eyeris`", fixed = TRUE)
  expect_match(bp, "attribution requirement", fixed = TRUE)
})

test_that("include_license / include_citation toggles work", {
  obj <- make_mock_eyeris()

  no_license <- as.character(boilerplate(obj, include_license = FALSE))
  expect_false(grepl("CC BY 4.0", no_license, fixed = TRUE))

  no_cite <- as.character(boilerplate(obj, include_citation = FALSE))
  expect_false(grepl("**Citation.**", no_cite, fixed = TRUE))
})

test_that("multi-block pipelines surface the run count", {
  single <- as.character(boilerplate(make_mock_eyeris(n_blocks = 1)))
  multi <- as.character(boilerplate(make_mock_eyeris(n_blocks = 3)))

  expect_false(grepl("recording blocks (runs)", single, fixed = TRUE))
  expect_match(multi, "each of the 3 recording blocks (runs)", fixed = TRUE)
})

test_that("version argument overrides the reported package version", {
  obj <- make_mock_eyeris()
  bp <- as.character(boilerplate(obj, version = "9.9.9"))
  expect_match(bp, "version 9.9.9", fixed = TRUE)
})

test_that("asymmetric deblink extend is described as backward/forward", {
  params <- list(deblink = list(parameters = list(extend = c(40, 60))))
  obj <- make_mock_eyeris(params = params)
  bp <- as.character(boilerplate(obj))
  expect_match(bp, "40 ms backward and 60 ms forward", fixed = TRUE)
})

test_that("epoching and baseline correction are described when present", {
  params <- list(
    epoch = list(
      parameters = list(
        events = "stim",
        limits = c(-0.5, 1.5),
        label = "prePostProbe",
        baseline = TRUE,
        baseline_type = "sub"
      )
    )
  )
  obj <- make_mock_eyeris(params = params)
  bp <- as.character(boilerplate(obj))

  expect_match(bp, "event-locked epochs", fixed = TRUE)
  expect_match(bp, "-0.5 to 1.5 s", fixed = TRUE)
  expect_match(bp, "prePostProbe", fixed = TRUE)
  expect_match(bp, "subtractive baseline correction", fixed = TRUE)
})

test_that("downsample and bin steps are described with their params", {
  ds <- make_mock_eyeris(
    params = list(downsample = list(parameters = list(target_fs = 100)))
  )
  expect_match(
    as.character(boilerplate(ds)),
    "downsampled to 100 Hz",
    fixed = TRUE
  )

  bn <- make_mock_eyeris(
    params = list(
      bin = list(parameters = list(bins_per_second = 50, method = "median"))
    )
  )
  bp_bin <- as.character(boilerplate(bn))
  expect_match(bp_bin, "50 bins per second", fixed = TRUE)
  expect_match(bp_bin, "median of the samples", fixed = TRUE)
})

test_that("custom / unknown pipeline extensions are described generically", {
  params <- list(
    deblink = list(parameters = list(extend = 50)),
    winsorize = list(parameters = list(lower = 0.01, upper = 0.99))
  )
  obj <- make_mock_eyeris(params = params)
  bp <- as.character(boilerplate(obj))
  expect_match(bp, "custom `winsorize` preprocessing step", fixed = TRUE)
})

test_that("binocular objects are detected and noted", {
  eye <- make_mock_eyeris()
  eye$binocular_mode <- "both"
  eye$info$binocular_mode <- "both"
  # mimic the per-eye structure expected by is_binocular_object()
  left <- eye
  left$binocular_mode <- "both"
  right <- eye
  right$binocular_mode <- "both"

  binoc <- list(left = left, right = right, original_file = "sub-001.asc")
  class(binoc) <- "eyeris"

  bp <- as.character(boilerplate(binoc))
  expect_match(bp, "binocular", fixed = TRUE)
  expect_match(
    bp,
    "left and right eyes were preprocessed independently",
    fixed = TRUE
  )
})

test_that("objects with no recorded params still produce a helpful message", {
  obj <- make_mock_eyeris(params = list())
  bp <- as.character(boilerplate(obj))
  expect_match(bp, "No preprocessing steps were recorded", fixed = TRUE)
})
