check_and_create_dir <- function(basedir, dir = NULL, verbose = TRUE) {
  if (!is.null(dir)) {
    dir <- file.path(basedir, dir)
  } else {
    dir <- basedir
  }

  if (dir.exists(dir)) {
    if (verbose) {
      cli::cli_alert_warning(
        sprintf("'%s' already exists. Skipping creation...", dir)
      )
    }
  } else {
    if (verbose) {
      cli::cli_alert_info(
        sprintf("'%s' does not exist. Creating...", dir)
      )
    }

    dir.create(dir, recursive = TRUE)

    if (verbose) {
      cli::cli_alert_success(
        sprintf("BIDS directory successfully created at: '%s'", dir)
      )
    }
  }
}

check_input <- function(arg) {
  arg_s <- deparse(substitute(arg))
  err_m <- sprintf("A value for ('%s') must be provided.\t", arg_s)
  err_c <- "input_arg_missing_error"

  if (is.null(arg)) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

check_baseline_mean <- function(x) {
  err_m <- "Baseline mean is zero, unable to divide by a baseline of 0.\t"
  err_c <- "divisive_baseline_mean_zero_error"

  if (x == 0) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

check_baseline_epoch_counts <- function(epochs, baselines) {
  err_m <- paste(
    "Number of trials matched based on baseline_events/",
    "baseline_period {", length(baselines), "} does not match the",
    "number of epochs matched based on events/limits {",
    length(epochs), "}! please check whether the event message(s)",
    "provided for baselining align with the epoched data.\t"
  )
  err_c <- "baseline_epochs_mismatch_error"

  if (length(epochs) != length(baselines)) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

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

check_column <- function(df, col_name) {
  if (!col_name %in% colnames(df)) {
    err_c <- "column_doesnt_exist_in_df_error"
    err_m <- paste0("No grouping variable '", col_name, "' in the epoched df.")
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

check_data <- function(eyeris, fun) {
  err_m <- sprintf(paste(
    "The provided object to `eyeris::%s()` is of type",
    "'%s' but should be an 'eyeris' object.\t"
  ), fun, class(eyeris))
  err_c <- "input_data_type_error"

  if (!inherits(eyeris, "eyeris")) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

check_pupil_cols <- function(eyeris, fun) {
  err_m <- sprintf(paste(
    "The provided object to `eyeris::%s()` doesn't include the",
    "expected `pupil_raw` column.\t"
  ), fun)
  err_c <- "missing_pupil_raw_error"

  # check if timeseries is a list of blocks
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    # now check each block for compliance
    for (block_num in seq_along(eyeris$timeseries)) {
      if (!"pupil_raw" %in% colnames(eyeris$timeseries[[block_num]])) {
        err_m <- sprintf(paste(
          "Block %d in the provided object to `eyeris::%s()` doesn't",
          "include the expected `pupil_raw` column.\t"
        ), block_num, fun)
        stop(structure(list(message = err_m, call = match.call()),
          class = err_c
        ))
      }
    }
  } else {
    # original check for single df fallback method
    if (!"pupil_raw" %in% colnames(eyeris$timeseries)) {
      stop(structure(list(message = err_m, call = match.call()),
        class = err_c
      ))
    }
  }
}

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

# first, assert that the timestamps list contains two dataframes (or tibbles)
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

# then, assert proper formatting of each dataframe within the timestamps list
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

check_epoch_msg_values <- function(eyeris, events) {
  invalid <- setdiff(eyeris$events$text, events$msg)
  err_m <- paste(
    "Invalid event messages specified in manual input.",
    "The following event messages do not exist within the raw data:",
    paste(invalid, collapse = ", "), "\n"
  )
  err_c <- "invalid_event_messages_error"

  if (length(invalid) > 0) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}

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

count_epochs <- function(epochs) {
  err_m <- "Data must be epoched.\t"
  err_c <- "epoch_count_error"

  if (length(epochs) == 0) {
    stop(structure(list(message = err_m, call = match.call()), class = err_c))
  }
}
