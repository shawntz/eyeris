#' Create baseline label for epoch data
#'
#' Generates a standardized label for baseline-corrected epoch data.
#'
#' @param baselined_data A list containing baseline correction information
#' @param epoch_id The identifier for the epoch
#'
#' @return A character string with the baseline label
#'
#' @keywords internal
make_baseline_label <- function(baselined_data, epoch_id) {
  paste0(
    "baseline_", baselined_data$baseline_cor_col_name,
    "_", epoch_id
  )
}

#' Extract baseline epochs from timeseries data
#'
#' Extracts baseline periods from timeseries data based on event messages
#' and time ranges or start/end messages.
#'
#' @param x An `eyeris` object containing the latest pupil column pointer
#' @param df The timeseries dataframe
#' @param evs Event messages for baseline extraction
#' @param time_range Time range for baseline extraction
#' @param matched_epochs Matched epoch start/end times
#' @param hz Sampling rate in Hz
#'
#' @return A list of baseline epoch dataframes
#'
#' @keywords internal
extract_baseline_epochs <- function(x, df, evs, time_range,
                                    matched_epochs, hz) {
  check_baseline_inputs(evs, time_range)

  time_col <- "time_orig"
  pupil_col <- x$latest
  start <- matched_epochs$start
  baselines <- vector(mode = "list", length = nrow(start)) # pre-alloc list

  # user provides start message + time range for baseline period
  if (!is.null(evs) && !is.null(time_range)) {
    duration <- sum(abs(time_range[1]), abs(time_range[2]))
    n_samples <- duration / (1 / hz)

    for (i in seq_len(nrow(start))) {
      current_epoch <- slice_epochs_with_limits(
        df, start$time[i], time_range,
        hz
      )
      baselines[[i]] <- current_epoch
    }
  } else { # user provides start message + end message for baseline period
    end <- matched_epochs$end
    check_start_end_timestamps(start, end)

    for (i in seq_len(nrow(start))) {
      i_start <- start$time[i]
      i_end <- end$time[i]

      duration <- (i_end - i_start) / hz
      n_samples <- duration * hz

      baselines[[i]] <- df |>
        dplyr::filter(time_orig >= i_start & time_orig < i_end)
    }
  }

  baselines
}

#' Compute baseline correction for epoch data
#'
#' Applies baseline correction to epoch data using either subtractive or
#' divisive methods.
#'
#' @param x An `eyeris` object containing the latest pupil column pointer
#' @param epochs A list of epoch dataframes
#' @param baseline_epochs A list of baseline epoch dataframes
#' @param mode The baseline correction mode ("sub" for subtractive,
#' "div" for divisive)
#' @param epoch_events Event messages for epochs (optional)
#' @param baseline_events Event messages for baselines (optional)
#'
#' @return A list containing baseline correction results and metadata
#'
#' @keywords internal
compute_baseline <- function(x, epochs,
                             baseline_epochs, mode,
                             epoch_events = NULL, baseline_events = NULL) {
  # compute baseline on pre z-scored data
  pupil_col <- gsub("_z", "", x$latest)

  new_col <- paste0(pupil_col, "_", mode, "_bline")

  if (!is.null(baseline_events)) {
    baseline_event_desc <- if (length(baseline_events) == 1) {
      clean_name <- gsub("[*{}]", "", baseline_events)
      gsub("[^a-zA-Z0-9_]", "_", clean_name)
    } else {
      "multi_baseline"
    }
    new_col <- paste0(pupil_col, "_", mode, "_bline_", baseline_event_desc)
  }

  # pre-alloc output data structs
  baseline_data <- vector(mode = "list", length = length(baseline_epochs))
  baseline_means <- rep(NA, length(baseline_epochs))
  method <- "none... skipped"

  n_epochs <- length(epochs)
  n_baseline_epochs <- length(baseline_epochs)

  if (n_baseline_epochs > n_epochs) {
    warning(sprintf(
      paste0(
        "More baseline epochs (%d) than actual epochs (%d).\n",
        "Truncating baseline epochs to match."
      ),
      n_baseline_epochs, n_epochs
    ))
    baseline_epochs <- baseline_epochs[1:n_epochs]
    baseline_data <- vector(mode = "list", length = n_epochs)
    baseline_means <- rep(NA, n_epochs)
  }

  for (i in seq_len(length(baseline_epochs))) {
    if (i > length(epochs)) {
      warning(sprintf("Epoch %d does not exist, skipping baseline computation",
                      i)
      )
      baseline_data[[i]] <- rep(NA_real_, 1)
      baseline_means[i] <- NA_real_
      next
    }

    pupil_dat <- epochs[[i]][[pupil_col]]
    baseline_window_pupil <- baseline_epochs[[i]][[pupil_col]]

    if (is.null(baseline_window_pupil) ||
          length(baseline_window_pupil) == 0 ||
          all(is.na(baseline_window_pupil))) {
      # if no baseline data, fill with NA of correct length
      baseline_removed <- rep(NA_real_, length(pupil_dat))
      baseline_avg <- NA_real_
    } else {
      baseline_avg <- mean(baseline_window_pupil, na.rm = TRUE)
      if (mode == "sub") {
        method <- "subtractive"
        baseline_removed <- pupil_dat - baseline_avg
      } else if (mode == "div") {
        method <- "divisive"
        check_baseline_mean(baseline_avg)
        baseline_removed <- pupil_dat / baseline_avg
      }
    }

    baseline_data[[i]] <- baseline_removed
    baseline_means[i] <- baseline_avg
  }

  list(
    baseline_cor_epochs = baseline_data,
    baseline_means_by_epoch = baseline_means,
    baseline_correction_method = method,
    baseline_cor_col_name = new_col,
    epoch_events = epoch_events,
    baseline_events = baseline_events,
    n_epochs = n_epochs,
    n_baseline_epochs = n_baseline_epochs
  )
}
