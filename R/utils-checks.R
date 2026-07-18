#' Check for DuckDB availability
#'
#' This internal helper checks whether the \pkg{duckdb} package is installed.
#' If it is not available, a status message is displayed with platform-specific
#' installation instructions (macOS, Linux, Windows). Functions that depend on
#' DuckDB call this check before proceeding.
#'
#' @return `TRUE` if \pkg{duckdb} is installed, otherwise `FALSE` (with an
#'   informative status message).
#'
#' @keywords internal
check_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    packageStartupMessage(
      "** DuckDB not found. Database features are disabled.\n\n",
      "=> To install DuckDB:\n",
      "  - macOS: install.packages('duckdb', type = 'binary')\n",
      "  - Linux: use system packages (e.g., `sudo apt-get install r-cran-duckdb`)\n",
      "           or install.packages('duckdb') if binaries are available\n",
      "  - Windows: install.packages('duckdb')\n\n",
      "Once installed, restart R and reload eyeris to enable database storage\n",
      "(bidsify(..., db_enabled = TRUE) and eyeris_db_* functions).\n"
    )
    return(FALSE)
  }
  TRUE
}

#' Check for Arrow availability
#'
#' This internal helper checks whether the \pkg{arrow} package is installed.
#' If it is not available, a status message is displayed with platform-specific
#' installation instructions. The arrow package is used for efficient parquet
#' file I/O. When not available, eyeris falls back to DuckDB for parquet
#' operations, which is slower but functional.
#'
#' @return `TRUE` if \pkg{arrow} is installed, otherwise `FALSE` (with an
#'   informative status message).
#'
#' @keywords internal
check_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    packageStartupMessage(
      "** Arrow not found. Parquet operations will use DuckDB fallback (slower).\n\n",
      "=> To install Arrow:\n\n",
      "  - macOS:\n",
      "    1. First install system dependencies with Homebrew:\n",
      "       brew update\n",
      "       brew install pkg-config cmake apache-arrow\n",
      "    2. Then install the R package:\n",
      "       install.packages('arrow', type = 'binary')\n\n",
      "  - Linux (Ubuntu/Debian):\n",
      "    1. Install system dependencies:\n",
      "       sudo apt-get update\n",
      "       sudo apt-get install -y libcurl4-openssl-dev libssl-dev\n",
      "    2. Then install the R package:\n",
      "       install.packages('arrow')\n\n",
      "  - Linux (Fedora/RHEL):\n",
      "    1. Install system dependencies:\n",
      "       sudo dnf install libcurl-devel openssl-devel\n",
      "    2. Then install the R package:\n",
      "       install.packages('arrow')\n\n",
      "  - Windows:\n",
      "    install.packages('arrow')\n\n",
      "For more details, see: https://arrow.apache.org/docs/r/\n\n",
      "Once installed, restart R and reload eyeris to enable faster parquet export/import\n",
      "(eyeris_db_to_parquet(), read_eyeris_parquet(), and related functions).\n"
    )
    return(FALSE)
  }
  TRUE
}

#' Check and create directory if it doesn't exist
#'
#' Checks if a directory exists and creates it if it doesn't. Provides
#' informative messages about the process.
#'
#' @param basedir The base directory path
#' @param dir The subdirectory to create (optional)
#' @param verbose Whether to display status messages
#'
#' @return No return value; creates directory if needed
#'
#' @keywords internal
check_and_create_dir <- function(basedir, dir = NULL, verbose = TRUE) {
  if (!is.null(dir)) {
    dir <- file.path(basedir, dir)
  } else {
    dir <- basedir
  }

  if (dir.exists(dir)) {
    log_warn("'{dir}' already exists. Skipping creation...", verbose = verbose)
  } else {
    log_info("'{dir}' does not exist. Creating...", verbose = verbose)

    dir.create(dir, recursive = TRUE)

    log_success(
      "BIDS directory successfully created at: '{dir}'",
      verbose = verbose
    )
  }
}

#' Check if input argument is provided
#'
#' Validates that a required argument is not NULL and throws an error
#' if missing.
#'
#' @param arg The argument to check
#'
#' @return No return value; throws error if argument is NULL
#'
#' @keywords internal
check_input <- function(arg) {
  arg_s <- deparse(substitute(arg))
  err_m <- sprintf("A value for ('%s') must be provided.\t", arg_s)
  err_c <- "input_arg_missing_error"

  if (is.null(arg)) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check if baseline mean is zero
#'
#' Validates that baseline mean is not zero for divisive baseline correction.
#'
#' @param x The baseline mean value to check
#'
#' @return No return value; throws error if baseline mean is zero
#'
#' @keywords internal
check_baseline_mean <- function(x) {
  err_m <- "Baseline mean is zero, unable to divide by a baseline of 0.\t"
  err_c <- "divisive_baseline_mean_zero_error"

  if (x == 0) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check baseline and epoch counts match
#'
#' Validates that the number of baseline epochs matches the number of epochs.
#'
#' @param epochs A list of epoch data
#' @param baselines A list of baseline data
#'
#' @return No return value; throws error if counts don't match
#'
#' @keywords internal
check_baseline_epoch_counts <- function(epochs, baselines) {
  n_epochs <- length(epochs)
  n_baselines <- length(baselines)

  if (n_epochs != n_baselines) {
    err_m <- paste(
      "Number of trials matched based on baseline_events/",
      "baseline_period {",
      n_baselines,
      "} does not match the",
      "number of epochs matched based on events/limits {",
      n_epochs,
      "}! please check whether the event message(s)",
      "provided for baselining align with the epoched data.\n",
      "This usually happens when:\n",
      "1. There are different numbers of baseline events vs epoch events\n",
      "2. Some baseline events don't have valid baseline windows\n",
      "3. The baseline events and epoch events are not properly paired\n",
      "Consider using the same event for both epoching and baselining,\n",
      "or ensure they are properly aligned.\t"
    )
    err_c <- "baseline_epochs_mismatch_error"

    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check baseline input arguments
#'
#' Validates that baseline inputs are properly specified.
#'
#' @param events Event messages for baseline extraction
#' @param limits Time limits for baseline extraction
#'
#' @return No return value; throws error if inputs are invalid
#'
#' @keywords internal
check_baseline_inputs <- function(events, limits) {
  err_c <- "baseline_input_args_error"

  if (is.null(events) && is.null(limits)) {
    err_m <- paste(
      "Compute_baseline is TRUE, but baseline_events and",
      "baseline_period are NULL.\t"
    )
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  } else if (is.na(events[2]) && is.null(limits)) {
    err_m <- paste(
      "If no stop messages are provided, then you must specify",
      "the baseline_period in the form `c(time_min, time_max)`.\t"
    )
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check if column exists in data frame
#'
#' Validates that a specified column exists in a data frame.
#'
#' @param df The data frame to check
#' @param col_name The column name to look for
#'
#' @return No return value; throws error if column doesn't exist
#'
#' @keywords internal
check_column <- function(df, col_name) {
  if (!col_name %in% colnames(df)) {
    err_c <- "column_doesnt_exist_in_df_error"
    err_m <- paste0("No grouping variable '", col_name, "' in the epoched df.")
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Find the modal (most frequent) value in a numeric vector
#'
#' Returns the most common value in a vector. Used to infer the expected
#' inter-sample interval directly from the data in a way that is robust to a
#' minority of irregular intervals (e.g., dropped samples).
#'
#' @param x A numeric vector.
#'
#' @return The most frequently occurring (finite) value in `x`, or `NA_real_`
#'   if `x` contains no finite values.
#'
#' @keywords internal
modal_value <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Check for uniform (consecutive, equal) sampling intervals
#'
#' Validates that consecutive samples in a timeseries are uniformly spaced.
#' Different eye trackers have hardware-specific quirks that violate the
#' uniform-sampling assumption baked into the `eyeris` pipeline. The most
#' relevant here is that some hardware *drops* samples when pupil data is
#' missing, rather than zero-filling the gap (as EyeLink does). Dropped samples
#' leave holes in the otherwise evenly spaced time vector, and because
#' downstream steps (e.g., [eyeris::detransient()], [eyeris::lpfilt()],
#' [eyeris::downsample()]) assume a fixed sampling rate, those holes silently
#' distort the results. This guardrail surfaces the quirk early with an
#' informative warning so it can be addressed before preprocessing.
#'
#' The expected inter-sample interval is inferred from the data as the modal
#' (most frequent) positive interval, which is robust both to a minority of
#' irregular intervals and to sub-millisecond timestamp rounding at high
#' sampling rates. Any nonzero interval not exactly equal to this modal
#' interval is marked irregular; zero-length intervals (the tell-tale of
#' integer-millisecond rounding of sub-millisecond samples) are exempt.
#' Intervals longer than the mode are additionally used to estimate the number
#' of dropped samples.
#' When the timeseries spans multiple recording segments (`blocks`), each
#' segment is checked independently so that the expected gap *between* segments
#' is not mistaken for a dropped sample.
#'
#' Sporadic dropout leaves visible gaps, but *systematic* dropout (e.g., every
#' Nth sample missing) leaves a uniform but coarser grid that the gap check
#' alone cannot see. When the nominal sampling rate (`hz`) is known, it is
#' cross-checked against the data-derived interval to surface this case too,
#' while avoiding false positives from high-rate trackers that report
#' integer-millisecond timestamps for sub-millisecond samples.
#'
#' @param time_vector Numeric vector of sample timestamps (in milliseconds).
#' @param hz Optional known sampling rate in Hz (e.g., from the file header).
#'   Used to annotate the warning with the nominal rate and to cross-check for
#'   systematic dropout.
#' @param blocks Optional vector (same length as `time_vector`) identifying the
#'   recording segment each sample belongs to. When supplied, intervals are
#'   only compared *within* each segment.
#' @param tolerance Relative tolerance for the systematic-dropout cross-check
#'   (default `0.5`). A rate mismatch is flagged when the data-derived interval
#'   exceeds the nominal interval (`1000 / hz`) by more than this fraction.
#' @param block_label Optional character label used in the warning to identify
#'   the segment being checked (e.g., `"block_1"`).
#' @param verbose Logical. Whether to emit the warning message (default `TRUE`).
#'
#' @return Invisibly, a list (or, when `blocks` is supplied, a list of such
#'   lists, one per segment) summarizing the check with elements: `uniform`
#'   (logical), `rate_mismatch` (logical; TRUE when the effective rate is lower
#'   than the nominal `hz`), `expected_interval` (ms), `n_intervals`,
#'   `n_irregular`, `n_missing_samples` (estimated number of dropped samples),
#'   and `prop_irregular`. A warning is emitted via [log_warn()] when irregular
#'   intervals or a rate mismatch are detected.
#'
#' @keywords internal
check_uniform_sampling_intervals <- function(
  time_vector,
  hz = NULL,
  blocks = NULL,
  tolerance = 0.5,
  block_label = NULL,
  verbose = TRUE
) {
  # when recording segments are supplied, validate each independently so that
  # the (expected) gap between segments is not flagged as a dropped sample
  if (!is.null(blocks)) {
    if (length(blocks) != length(time_vector)) {
      log_error(
        "`blocks` must be the same length as `time_vector`",
        "({length(blocks)} vs {length(time_vector)})."
      )
    }
    results <- lapply(unique(blocks), function(b) {
      check_uniform_sampling_intervals(
        time_vector = time_vector[blocks == b],
        hz = hz,
        blocks = NULL,
        tolerance = tolerance,
        block_label = paste0("block_", b),
        verbose = verbose
      )
    })
    return(invisible(results))
  }

  result <- list(
    uniform = TRUE,
    rate_mismatch = FALSE,
    expected_interval = NA_real_,
    n_intervals = 0L,
    n_irregular = 0L,
    n_missing_samples = 0L,
    prop_irregular = 0
  )

  # need at least two non-NA timestamps to form a single interval
  time_clean <- time_vector[!is.na(time_vector)]
  if (length(time_clean) < 2) {
    return(invisible(result))
  }

  intervals <- diff(time_clean)
  positive_intervals <- intervals[intervals > 0]
  if (length(positive_intervals) == 0) {
    # non-increasing time is a monotonicity problem, handled elsewhere
    return(invisible(result))
  }

  # infer the expected interval from the data (robust to dropped samples);
  # fall back to the nominal rate only if the data cannot supply one
  expected <- modal_value(positive_intervals)
  if (!is.finite(expected) || expected <= 0) {
    log_error(
      "Unable to infer expected sampling interval from timestamp differences. ",
      "Check that sample timestamps are finite and increasing."
    )
  }

  result$expected_interval <- expected
  n_total <- length(intervals)
  result$n_intervals <- n_total

  # (A) uniform-grid validation: any interval that differs from the expected
  # spacing indicates an irregular sample grid. Zero-length intervals are the
  # accepted tell-tale of integer-millisecond rounding of sub-millisecond
  # samples (see (B) below), so they are exempt here; genuinely inconsistent
  # nonzero intervals (including time-reversals) remain irregular.
  irregular <- intervals != expected & intervals != 0
  n_irregular <- sum(irregular, na.rm = TRUE)

  # (B) systematic-dropout cross-check against the device's nominal rate.
  # When a tracker drops every Nth sample, the survivors still form a uniform
  # (but coarser) grid, so the gap check in (A) sees nothing wrong; only the
  # known sampling rate reveals that the effective rate is too low. High-rate
  # trackers that report integer-ms timestamps for sub-ms samples also inflate
  # the observed spacing above the nominal interval, but they leave zero-length
  # intervals (duplicate timestamps) as a tell-tale, so only flag a mismatch
  # when no such finer-than-reported resolution is present.
  nominal_interval <- if (!is.null(hz) && is.finite(hz) && hz > 0) {
    1000 / hz
  } else {
    NA_real_
  }
  has_subinterval_resolution <- any(intervals <= 0)
  rate_mismatch <- is.finite(nominal_interval) &&
    !has_subinterval_resolution &&
    expected > nominal_interval * (1 + tolerance)

  if (n_irregular == 0 && !rate_mismatch) {
    return(invisible(result))
  }

  result$uniform <- FALSE
  result$rate_mismatch <- rate_mismatch
  result$n_irregular <- as.integer(n_irregular)
  result$prop_irregular <- n_irregular / n_total

  segment <- if (!is.null(block_label)) paste0(" in ", block_label) else ""
  expected_ms <- round(expected, 1)

  if (n_irregular > 0) {
    long_intervals <- intervals[irregular & intervals > expected]
    short_intervals <- intervals[irregular & intervals < expected]
    n_long <- length(long_intervals)
    n_short <- length(short_intervals)

    # Estimate dropped samples only from intervals longer than expected.
    n_missing <- if (n_long > 0) {
      sum(pmax(round(long_intervals / expected) - 1, 0))
    } else {
      0
    }
    result$n_missing_samples <- as.integer(n_missing)

    if (verbose) {
      nominal <- if (is.finite(nominal_interval)) {
        paste0(hz, " Hz")
      } else {
        "inferred from data"
      }
      pct <- round(100 * result$prop_irregular, 2)
      first_irregular_sample <- which(irregular)[1] + 1
      long_gap_summary <- if (n_long > 0) {
        largest_gap <- round(max(long_intervals), 4)
        largest_gap_ratio <- round(max(long_intervals) / expected, 1)
        paste0(
          " Estimated ",
          n_missing,
          " dropped sample(s); largest long ",
          "interval is ",
          largest_gap,
          " ms (~",
          largest_gap_ratio,
          "x expected)."
        )
      } else {
        ""
      }

      msg <- paste0(
        "Non-uniform sampling intervals detected{segment}: {n_irregular} of ",
        "{n_total} intervals ({pct}%) differ from the expected ",
        "{expected_ms} ms spacing (nominal rate: {nominal}); ",
        "{n_long} longer and {n_short} shorter.{long_gap_summary} ",
        "First irregular interval occurs before sample ",
        "{first_irregular_sample}. This violates the uniform-sampling ",
        "assumption of the eyeris pipeline. Consider resampling onto a ",
        "regular time grid before preprocessing."
      )
      log_warn(msg, verbose = verbose)
    }
  }

  if (rate_mismatch && verbose) {
    effective_hz <- round(1000 / expected, 2)
    nominal_ms <- round(nominal_interval, 4)

    msg <- paste0(
      "Effective sampling rate (~{effective_hz} Hz, {expected_ms} ms spacing) ",
      "is lower than the device's reported {hz} Hz ({nominal_ms} ms){segment}. ",
      "This can indicate systematic sample dropout (e.g., every Nth sample ",
      "missing), which leaves a uniform but coarser time grid that the eyeris ",
      "pipeline would otherwise treat as the true sampling rate. Verify the ",
      "recording against the tracker's configured sampling rate."
    )
    log_warn(msg, verbose = verbose)
  }

  invisible(result)
}

#' Check if object is of class eyeris
#'
#' Validates that an object is of class `eyeris`.
#'
#' @param eyeris The `eyeris` object to check
#' @param fun The function name for error message
#'
#' @return No return value; throws error if object is not `eyeris` class
#'
#' @keywords internal
check_data <- function(eyeris, fun) {
  err_m <- sprintf(
    paste(
      "The provided object to `eyeris::%s()` is of type",
      "'%s' but should be an 'eyeris' object.\t"
    ),
    fun,
    class(eyeris)
  )
  err_c <- "input_data_type_error"

  if (!inherits(eyeris, "eyeris")) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check if pupil_raw column exists
#'
#' Validates that the pupil_raw column exists in the `eyeris` object.
#'
#' @param eyeris The `eyeris` object to check
#' @param fun The function name for error message
#'
#' @return No return value; throws error if pupil_raw column is missing
#'
#' @keywords internal
check_pupil_cols <- function(eyeris, fun) {
  err_m <- sprintf(
    paste(
      "The provided object to `eyeris::%s()` doesn't include the",
      "expected `pupil_raw` column.\t"
    ),
    fun
  )
  err_c <- "missing_pupil_raw_error"

  # check if time series is a list of blocks
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    # now check each block for compliance
    for (block_num in seq_along(eyeris$timeseries)) {
      if (!"pupil_raw" %in% colnames(eyeris$timeseries[[block_num]])) {
        err_m <- sprintf(
          paste(
            "Block %d in the provided object to `eyeris::%s()` doesn't",
            "include the expected `pupil_raw` column.\t"
          ),
          block_num,
          fun
        )
        stop(structure(
          list(message = err_m, call = match.call()),
          class = err_c
        ))
      }
    }
  } else {
    # original check for single df fallback method
    if (!"pupil_raw" %in% colnames(eyeris$timeseries)) {
      stop(structure(list(message = err_m, call = match.call()), class = err_c))
    }
  }
}

#' Check epoch input for plotting
#'
#' Validates that exactly one epoch is specified for plotting.
#'
#' @param epochs A list of epoch data
#'
#' @return No return value; throws error if more than one epoch is specified
#'
#' @keywords internal
check_epoch_input <- function(epochs) {
  err_m <- paste(
    "eyeris::plot() requires that exactly 1 set of epoched data is",
    "provided -- please ensure the string you pass in `epoch` only",
    "matches to 1 epoch.\t"
  )
  err_c <- "too_many_epochs_error"

  if (length(epochs) != 1) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check epoch manual input data structure
#'
#' Validates that the events argument is a list of two data frames.
#'
#' @param ts_list A list containing both start _and_ end timestamp data frames
#'
#' @return No return value; throws error if structure is invalid
#'
#' @keywords internal
check_epoch_manual_input_data <- function(ts_list) {
  err_m <- "The `events` argument must be a list of two data frames.\t"
  err_c <- "timestamps_list_config_error"

  list_check_a <- (!is.list(ts_list) || length(ts_list) != 2)
  list_check_b <- (!is.data.frame(ts_list[[1]]))
  list_check_c <- (!is.data.frame(ts_list[[2]]))

  if (list_check_a || list_check_b || list_check_c) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check epoch manual input data frame format
#'
#' Validates that start and end timestamp data frames have required columns.
#'
#' @param ts_list A list containing start and end timestamp data frames
#'
#' @return No return value; throws error if format is invalid
#'
#' @keywords internal
check_epoch_manual_input_dfs <- function(ts_list) {
  start_times <- ts_list[[1]]
  end_times <- ts_list[[2]]

  if (!("time" %in% names(start_times)) || !("msg" %in% names(start_times))) {
    err_m <- "The start times df must contain 'time' and 'msg' columns.\t"
    err_c <- "start_timestamps_df_config_error"
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }

  if (!("time" %in% names(end_times)) || !("msg" %in% names(end_times))) {
    err_m <- "The end times df must contain 'time' and 'msg' columns.\t"
    err_c <- "end_timestamps_df_config_error"
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }

  # lastly, assert that start and end timestamp data frames are balanced
  check_start_end_timestamps(start_times, end_times)
}

#' Check epoch message values against available events
#'
#' Validates that specified event messages exist in the `eyeris` object.
#'
#' @param eyeris The `eyeris` object containing events
#' @param events A data frame containing event messages to validate
#'
#' @return No return value; throws error if invalid messages are found
#'
#' @keywords internal
check_epoch_msg_values <- function(eyeris, events) {
  invalid <- setdiff(eyeris$events$text, events$msg)
  err_m <- paste(
    "Invalid event messages specified in manual input.",
    "The following event messages do not exist within the raw data:",
    paste(invalid, collapse = ", "),
    "\n"
  )
  err_c <- "invalid_event_messages_error"

  if (length(invalid) > 0) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check limits in wildcard mode
#'
#' Validates that limits are provided when using wildcard mode.
#'
#' @param limits Time limits for epoch extraction
#'
#' @return No return value; throws error if limits are missing in wildcard mode
#'
#' @keywords internal
check_limits <- function(limits) {
  err_m <- paste(
    "Limits cannot be NULL when using wildcard (*) mode",
    "since no stop message is declared.\t"
  )
  err_c <- "invalid_limits_in_wildcard_mode_error"

  if (is.null(limits)) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check start and end timestamps are balanced
#'
#' Validates that start and end timestamp data frames have the same number
#' of rows.
#'
#' @param start The start timestamp data frame
#' @param end The end timestamp data frame
#'
#' @return No return value; throws error if timestamps are unbalanced
#'
#' @keywords internal
check_start_end_timestamps <- function(start, end) {
  err_c <- "unbalanced_start_stop_epoch_timestamps_error"

  s_len <- length(start$time)
  e_len <- length(end$time)

  if (s_len != e_len) {
    if (s_len > e_len) {
      err_m <- paste(
        "There are more epoch start times than end times.",
        "Each start time must have a corresponding end time.\t"
      )
    } else {
      err_m <- paste(
        "There are more epoch end times than start times",
        "Each start time must have a corresponding end time.\t"
      )
    }

    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Count epochs and validate data is epoched
#'
#' Counts the number of epochs and validates that data has been epoched.
#'
#' @param epochs A list of epoch data
#'
#' @return No return value; throws error if no epochs found
#'
#' @keywords internal
count_epochs <- function(epochs) {
  err_m <- "Data must be epoched.\t"
  err_c <- "epoch_count_error"

  if (length(epochs) == 0) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check time series monotonicity
#'
#' Validates that a time vector is monotonically increasing.
#'
#' @param time_vector The time vector to check
#' @param time_col_name The name of the time column for error messages
#'
#' @return No return value; throws error if time series is not monotonic
#'
#' @keywords internal
check_time_monotonic <- function(time_vector, time_col_name = "time_secs") {
  if (is.null(time_vector) || length(time_vector) == 0) {
    log_error(
      "Time vector is NULL or empty. Cannot validate monotonicity. Time column: {time_col_name}"
    )
  }

  # remove NA values for the check
  time_clean <- time_vector[!is.na(time_vector)]

  if (length(time_clean) < 2) {
    log_error(
      "Insufficient non-NA time points to validate monotonicity. Need at least 2 points, got {length(time_clean)}. Time column: {time_col_name}"
    )
  }

  # check if time series is monotonically increasing
  if (!all(diff(time_clean) >= 0)) {
    # find first violation
    diffs <- diff(time_clean)
    first_violation_idx <- which(diffs < 0)[1]

    log_error(
      "Time series is not monotonically increasing. First violation at index {first_violation_idx + 1} where time decreases from {time_clean[first_violation_idx]} to {time_clean[first_violation_idx + 1]}. Time column: {time_col_name}. This may indicate EDF file errors or data corruption."
    )
  }
}

#' Check if object is a binocular eyeris object
#'
#' Detects whether an object is a binocular `eyeris` object created with
#' `binocular_mode = "both"`.
#'
#' @param x The `eyeris` object to check
#'
#' @return Logical indicating whether the object is a binocular `eyeris` object
#'
#' @keywords internal
is_binocular_object <- function(x) {
  is.list(x) &&
    "left" %in% names(x) &&
    "right" %in% names(x) &&
    "binocular_mode" %in% names(x$left) &&
    "binocular_mode" %in% names(x$right) &&
    x$left$binocular_mode == "both" &&
    x$right$binocular_mode == "both"
}

#' Check if binocular correlations should be plotted
#'
#' Validates that binocular correlations should be plotted.
#'
#' @param x The `eyeris` object to check
#'
#' @return Logical indicating whether binocular correlations should be plotted
#'
#' @keywords internal
should_plot_binoc_cors <- function(x) {
  is.list(x) &&
    ("left" %in% names(x) && "right" %in% names(x)) ||
    (isTRUE(x$binocular))
}
