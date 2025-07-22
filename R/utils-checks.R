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
    if (verbose) {
      cli::cli_alert_warning(
        sprintf("[WARN] '%s' already exists. Skipping creation...", dir)
      )
    }
  } else {
    if (verbose) {
      cli::cli_alert_info(
        sprintf("[INFO] '%s' does not exist. Creating...", dir)
      )
    }

    dir.create(dir, recursive = TRUE)

    if (verbose) {
      cli::cli_alert_success(
        sprintf("[OKAY] BIDS directory successfully created at: '%s'", dir)
      )
    }
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

#' Check if column exists in dataframe
#'
#' Validates that a specified column exists in a dataframe.
#'
#' @param df The dataframe to check
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

#' Check if object is of class eyeris
#'
#' Validates that an object is of class eyeris.
#'
#' @param eyeris The `eyeris` object to check
#' @param fun The function name for error message
#'
#' @return No return value; throws error if object is not eyeris class
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
#' Validates that the pupil_raw column exists in the eyeris object.
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

  # check if timeseries is a list of blocks
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
        stop(structure(list(message = err_m, call = match.call()), class = err_c))
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
#' Validates that the events argument is a list of two dataframes.
#'
#' @param ts_list A list containing both start _and_ end timestamp dataframes
#'
#' @return No return value; throws error if structure is invalid
#'
#' @keywords internal
check_epoch_manual_input_data <- function(ts_list) {
  err_m <- "The `events` argument must be a list of two dataframes.\t"
  err_c <- "timestamps_list_config_error"

  list_check_a <- (!is.list(ts_list) || length(ts_list) != 2)
  list_check_b <- (!is.data.frame(ts_list[[1]]))
  list_check_c <- (!is.data.frame(ts_list[[2]]))

  if (list_check_a || list_check_b || list_check_c) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

#' Check epoch manual input dataframe format
#'
#' Validates that start and end timestamp dataframes have required columns.
#'
#' @param ts_list A list containing start and end timestamp dataframes
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

  # lastly, assert that start and end timestamp dataframes are balanced
  check_start_end_timestamps(start_times, end_times)
}

#' Check epoch message values against available events
#'
#' Validates that specified event messages exist in the eyeris object.
#'
#' @param eyeris The `eyeris` object containing events
#' @param events A dataframe containing event messages to validate
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
#' Validates that start and end timestamp dataframes have the same number
#' of rows.
#'
#' @param start The start timestamp dataframe
#' @param end The end timestamp dataframe
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
    cli::cli_abort(paste(
      "[EXIT] Time vector is NULL or empty. Cannot validate monotonicity.",
      "Time column:",
      time_col_name
    ))
  }

  # remove NA values for the check
  time_clean <- time_vector[!is.na(time_vector)]

  if (length(time_clean) < 2) {
    cli::cli_abort(paste(
      "[EXIT] Insufficient non-NA time points to validate monotonicity.",
      "Need at least 2 points, got",
      length(time_clean),
      "Time column:",
      time_col_name
    ))
  }

  # check if time series is monotonically increasing
  if (!all(diff(time_clean) >= 0)) {
    # find first violation
    diffs <- diff(time_clean)
    first_violation_idx <- which(diffs < 0)[1]

    cli::cli_abort(paste(
      "[EXIT] Time series is not monotonically increasing.",
      "First violation at index",
      first_violation_idx + 1,
      "where time decreases from",
      time_clean[first_violation_idx],
      "to",
      time_clean[first_violation_idx + 1],
      "Time column:",
      time_col_name,
      "This may indicate EDF file errors or data corruption."
    ))
  }
}

#' Check if object is a binocular eyeris object
#'
#' Detects whether an object is a binocular eyeris object created with
#' `binocular_mode = "both"`.
#'
#' @param x The `eyeris` object to check
#'
#' @return Logical indicating whether the object is a binocular eyeris object
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
