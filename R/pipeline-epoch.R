#' Epoch (and baseline) pupil data based on custom event message structure
#'
#' Intended to be used as the final preprocessing step. This function creates
#' data epochs of either fixed or dynamic durations with respect to provided
#' `events` and time `limits`, and also includes an intuitive metadata parsing
#' feature where additional trial data embedded within event messages can easily
#' be identified and joined into the resulting epoched data frames.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param events Either (1) a single string representing the event message to
#' perform trial extraction around, using specified `limits` to center the epoch
#' around or no `limits` (which then just grabs the data epochs between each
#' subsequent event string of the same type); (2) a vector containing both
#' `start` and `end` event message strings -- here, `limits` will be ignored and
#' the duration of each trial epoch will be the number of samples between each
#' matched `start` and `end` event message pair; or (3) a list of 2 dataframes
#' that manually specify start/end event timestamp-message pairs to pull out of
#' the raw timeseries data -- here, it is required that each raw timestamp and
#' event message be provided in the following format:
#'
#' list(
#'   data.frame(time = c(...), msg = c(...)), # start events
#'   data.frame(time = c(...), msg = c(...)), # end events
#'   1                                        # block number
#' )
#'
#' where the first data.frame indicates the `start` event timestamp and message
#' string pairs, and the second data.frame indicates the `end` event timestamp
#' and message string pairs. Additionally, manual epoching only words with
#' 1 block at a time for event-modes `2` and `3`; thus, please be sure to
#' explicitly indicate the block number in your input list (for examples,
#' see above as well as example #9 below for more details)
#'
#' For event-modes `1` and `2`, the way in which you pass in the event message
#' string must conform to a standardized protocol so that `eyeris` knows how to
#' find your events and (optionally) parse any included metadata into the tidy
#' epoch data outputs. You have two primary choices: either (a) specify a string
#' followed by a `*` wildcard expression (e.g., `"PROBE_START*`), which will
#' match any messages that have "PROBE_START ..." (... referring to potential
#' metadata, such as trial number, stim file, etc.); or (b) specify a string
#' using the `eyeris` syntax: (e.g., `"PROBE_{type}_{trial}"`), which will match
#' the messages that follow a structure like this "PROBE_START_1" and
#' "PROBE_STOP_1", and generate two additional metadata columns: `type` and
#' `trial`, which would contain the following values based on these two example
#' strings: `type`: `('START', 'STOP')`, and `trial`: `(1, 1)`
#' @param limits A vector of 2 values (start, end) in seconds, indicating where
#' trial extraction should occur centered around any given `start` message
#' string in the `events` parameter
#' @param label An (optional) string you can provide to customize the name of
#' the resulting `eyeris` class object containing the epoched data frame. If
#' left as `NULL` (default), then list item will be called `epoch_xyz`, where
#' `xyz` will be a sanitized version of the original `start` event string you
#' provided for matching. If you choose to specify a `label` here, then the
#' resulting list object name will take the form: `epoch_label`. **Warning:
#' if no `label` is specified and there are no event message strings to
#' sanitize, then you may obtain a strange-looking epoch list element in
#' your output object (e.g., `$epoch_`, or `$epoch_nana`, etc.). The data
#' should still be accessible within this nested lists, however, to avoid
#' ambiguous list objects, we recommend you provide an `epoch` label here
#' to be safe**
#' @param baseline **(New)** A single parameter that controls baseline
#' correction. Set to `TRUE` to both calculate and apply baseline correction, or
#' `FALSE` to skip it. This replaces the deprecated `calc_baseline` and
#' `apply_baseline` parameters
#' @param baseline_type Whether to perform *subtractive* (`sub`) or *divisive*
#' (`div`) baseline correction. Defaults to `sub`
#' @param baseline_events Similar to `events`, `baseline_events`, you can supply
#' either (1) a single string representing the event message to center the
#' baseline calculation around, as indicated by `baseline_period`; or (2) a
#' single vector containing both a `start` and an `end` event message string --
#' here, `baseline_period` will be ignored and the duration of each baseline
#' period that the mean will be calculated on will be the number of samples
#' between each matched `start` and `end` event message pair, as opposed to a
#' specified fixed duration (as described in 1). Please note, providing a list
#' of trial-level start/end message pairs (like in the `events` parameter) to
#' manually indicate unique start/end chunks for baselining is currently
#' unsupported. Though, we intend to add this feature in a later version of
#' `eyeris`, given it likely won't be a heavily utilized / in demand feature.
#' @param baseline_period A vector of 2 values (start, end) in seconds,
#' indicating the window of data that will be used to perform the baseline
#' correction, which will be centered around the single string "start" message
#' string provided in `baseline_events`. Again, `baseline_period` will be
#' ignored if both a "start" **and** "end" message string are provided to the
#' `baseline_events` argument
#' @param hz Data sampling rate. If not specified, will use the value contained
#' within the tracker's metadata
#' @param verbose A flag to indicate whether to print detailed logging messages
#' Defaults to `TRUE`. Set to `False` to suppress messages about the current
#' processing step and run silently
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#' @param calc_baseline **(Deprecated)** Use `baseline` instead
#' @param apply_baseline **(Deprecated)** Use `baseline` instead
#'
#' @return An `eyeris` object with a new nested list of data frames: `$epoch_*`.
#'   The epochs are organized hierarchically by block and preprocessing step.
#'   Each epoch contains the pupil timeseries data for the specified time window
#'   around each event message, along with metadata about the event.
#'
#'   When using `bidsify()` to export the data, filenames will include both
#'   epoch and baseline event information for clarity.
#'
#' @seealso [lifecycle::deprecate_warn()]
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#' eye_preproc <- eyeris::glassbox(demo_data)
#'
#' # example 1: select 1 second before/after matched event message "PROBE*"
#' eye_preproc |>
#'   eyeris::epoch(events = "PROBE*", limits = c(-1, 1))
#'
#' # example 2: select all samples between each trial
#' eye_preproc |>
#'   eyeris::epoch(events = "TRIALID {trial}")
#'
#' # example 3: grab the 1 second following probe onset
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = "PROBE_START_{trial}",
#'     limits = c(0, 1)
#'   )
#'
#' # example 4: 1 second prior to and 1 second after probe onset
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = "PROBE_START_{trial}",
#'     limits = c(-1, 1),
#'     label = "prePostProbe" # custom epoch label name
#'   )
#'
#' # example 5: manual start/end event pairs
#' # note: here, the `msg` column of each data frame is optional
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = list(
#'       data.frame(time = c(11334491), msg = c("TRIALID 22")), # start events
#'       data.frame(time = c(11337158), msg = c("RESPONSE_22")), # end events
#'       1 # block number
#'     ),
#'     label = "example5"
#'   )
#'
#' # example 6: manual start/end event pairs
#' # note: set `msg` to NA if you only want to pass in start/end timestamps
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = list(
#'       data.frame(time = c(11334491), msg = NA), # start events
#'       data.frame(time = c(11337158), msg = NA), # end events
#'       1 # block number
#'     ),
#'     label = "example6"
#'   )
#'
#' ## examples with baseline arguments enabled
#'
#' # example 7: use mean of 1-s preceding "PROBE_START" (i.e. "DELAY_STOP")
#' # to perform subtractive baselining of the 1-s PROBE epochs.
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = "PROBE_START_{trial}",
#'     limits = c(0, 1), # grab 0 seconds prior to and 1 second post PROBE event
#'     label = "prePostProbe", # custom epoch label name
#'     baseline = TRUE, # Calculate and apply baseline correction
#'     baseline_type = "sub", # "sub"tractive baseline calculation is default
#'     baseline_events = "DELAY_STOP_*",
#'     baseline_period = c(-1, 0)
#'   )
#'
#' # example 8: use mean of time period between set start/end event messages
#' # (i.e. between "DELAY_START" and "DELAY_STOP"). In this case, the
#' # `baseline_period` argument will be ignored since both a "start" and "end"
#' # message string are provided to the `baseline_events` argument.
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = "PROBE_START_{trial}",
#'     limits = c(0, 1), # grab 0 seconds prior to and 1 second post PROBE event
#'     label = "prePostProbe", # custom epoch label name
#'     baseline = TRUE, # Calculate and apply baseline correction
#'     baseline_type = "sub", # "sub"tractive baseline calculation is default
#'     baseline_events = c(
#'       "DELAY_START_*",
#'       "DELAY_STOP_*"
#'     )
#'   )
#'
#' # example 9: additional (potentially helpful) example
#' start_events <- data.frame(
#'   time = c(11334491, 11338691),
#'   msg = c("TRIALID 22", "TRIALID 23")
#' )
#' end_events <- data.frame(
#'   time = c(11337158, 11341292),
#'   msg = c("RESPONSE_22", "RESPONSE_23")
#' )
#' block_number <- 1
#'
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = list(start_events, end_events, block_number),
#'     label  = "example9"
#'   )
#'
#' @export
epoch <- function(eyeris, events, limits = NULL, label = NULL,
                  baseline = FALSE, baseline_type = c("sub", "div"),
                  baseline_events = NULL, baseline_period = NULL,
                  hz = NULL, verbose = TRUE, call_info = NULL,
                  calc_baseline = deprecated(),
                  apply_baseline = deprecated()) {
  # handle deprecated parameters
  if (is_present(calc_baseline)) {
    lifecycle::deprecate_warn(
      "1.3.0",
      "epoch(calc_baseline)", "epoch(baseline)"
    )
    if (isTRUE(calc_baseline)) {
      baseline <- TRUE
    }
  }

  if (is_present(apply_baseline)) {
    lifecycle::deprecate_warn(
      "1.3.0",
      "epoch(apply_baseline)", "epoch(baseline)"
    )
    if (isTRUE(apply_baseline)) {
      baseline <- TRUE
    }
  }

  calc_baseline <- baseline
  apply_baseline <- baseline

  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(
        events = events, limits = limits, label = label,
        baseline = baseline, baseline_type = baseline_type,
        baseline_events = baseline_events, baseline_period = baseline_period,
        hz = hz, verbose = verbose
      )
    )
  } else {
    call_info
  }
  eyeris |>
    pipeline_handler(
      epoch_pupil, "epoch", events, limits, label, calc_baseline,
      apply_baseline, baseline_type, baseline_events, baseline_period, hz,
      verbose,
      call_info = call_info
    )
}

#' Main epoching and baselining logic
#'
#' This function handles the core epoching and baselining operations for pupil
#' data. It processes time series data to extract epochs based on specified
#' events and optionally computes and applies baseline corrections.
#'
#' This function is called by the exposed wrapper [eyeris::epoch()].
#'
#' @param x An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param prev_op The name of the previous operation's output column
#' @param evs Events specification for epoching (character vector or list)
#' @param lims Time limits for epochs (numeric vector)
#' @param label Label for the epoch output
#' @param c_bline Logical indicating whether to calculate baseline
#' @param a_bline Logical indicating whether to apply baseline correction
#' @param bline_type Type of baseline correction ("sub" or "div")
#' @param bline_evs Events specification for baseline calculation
#' @param bline_per Baseline period specification
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch and baseline results
#'
#' @keywords internal
epoch_pupil <- function(x, prev_op, evs, lims, label, c_bline, a_bline,
                        bline_type = c("sub", "div"), bline_evs, bline_per,
                        hz, verbose) {
  bline_type <- tolower(bline_type)
  bline_type <- match.arg(bline_type)

  start_time <- Sys.time()

  if (is.null(hz)) hz <- x$info$sample.rate

  msg_s <- evs[1]
  msg_e <- evs[2]

  processed_data <- list()

  alert_str <- "\nEpoching pupil data..."
  if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
    if (c_bline) {
      alert_str <- "\nEpoching and baselining pupil data..."
    } else {
      alert_str <- "\nEpoching pupil data..."
    }
  }

  if (verbose) {
    alert("info", alert_str)
  }

  if (is.list(evs)) { # manual method (with only 1 block at a time)
    warning(
      paste0(
        "NOTE: Manual epoching only works with 1 block at a time.",
        "\nManual epoch input must be a list of 2 dataframes and 1 numeric:",
        "\n  - `start_events` (df), `end_events` (df), and `block` (numeric)",
        "\nPlease be sure to explicitly indicate the block number in your",
        "input list! (see example #9 in the documentation for more details)."
      )
    )

    if (!is.list(evs) || length(evs) != 3) {
      stop(
        paste0(
          "Manual epoch input must be a list of 2 dataframes and 1 numeric:",
          "\n`start_events` (df), `end_events` (df), and `block` (numeric)"
        )
      )
    }

    block_names <- paste0("block_", evs[[3]])
  } else if (is.character(evs)) {
    block_names <- names(x$events)
  } else {
    stop(
      paste0(
        "Error: Invalid data structure provided.",
        "Expected an `eyeris` dataframe containing",
        "a valid timeseries column.",
      )
    )
  }

  for (bn in block_names) {
    block_int <- get_block_numbers(bn)

    if (!is.list(evs)) {
      n_events <- merge_events_with_timeseries(
        x$events[[bn]], msg_s,
        merge = FALSE
      ) |>
        nrow()

      if (verbose) {
        alert(
          "info",
          sprintf(
            "Block %s: found %d matching events for %s",
            block_int,
            n_events,
            clean_string(msg_s)
          )
        )
      }
    } else {
      n_events <- length(evs[[1]]$time)
    }

    block_metadata <- list(
      id = block_int,
      name = bn,
      n_events = n_events
    )

    processed_data[[bn]] <- epoch_and_baseline_block(
      x, block_metadata, label, evs, lims, msg_s, msg_e,
      c_bline, a_bline, bline_type, bline_evs, bline_per, hz, verbose
    )

    epoch_id <- processed_data[[bn]]$epoch$id
    epoched_data <- processed_data[[bn]]$epoch$res

    if (is.null(x[[epoch_id]])) {
      x[[epoch_id]] <- list()
    }
    x[[epoch_id]][[bn]] <- dplyr::as_tibble(epoched_data)

    # store epoch metadata when no baseline correction is used
    if (!a_bline) {
      epoch_info <- list(
        calc_baseline = FALSE,
        apply_baseline = FALSE,
        epoch_events = evs,
        epoch_limits = lims,
        n_epochs = length(unique(epoched_data$matched_event))
      )

      if (is.null(x[[epoch_id]]$info)) {
        x[[epoch_id]]$info <- list()
      }
      x[[epoch_id]]$info[[bn]] <- epoch_info
    }

    if (verbose) {
      alert(
        "success",
        sprintf(
          "Block %d: pupil data from %d unique event messages extracted",
          block_int,
          length(unique(epoched_data$matched_event))
        )
      )
    }

    msg_str <- "\nPupil epoching completed in %.2f seconds"

    if (a_bline && n_events > 0) {
      baseline_id <- processed_data[[bn]]$baseline$id
      x[[baseline_id]][[bn]] <- processed_data[[bn]]$baseline$res

      if (verbose) {
        alert(
          "success",
          sprintf(
            "Block %d: %d epochs baselined",
            block_int,
            length(x[[baseline_id]][[bn]])
          )
        )
      }
      msg_str <- "\nPupil epoching and baselining completed in %.2f secs"
    } else {
      msg_str <- "\nPupil epoching completed in %.2f seconds"
    }

    elapsed <- difftime(Sys.time(), start_time, units = "secs")

    if (verbose) {
      alert(
        "success",
        sprintf(
          msg_str,
          as.numeric(elapsed)
        )
      )
    }
  }

  # recalculate epoched confounds if they exist, since new epochs were created
  if (!is.null(x$confounds$unepoched_timeseries)) {
    if (verbose) {
      alert("info", "Recalculating epoched confounds for new epochs...")
    }

    # check for epoch data and compute confounds if present
    epoch_names <- grep("^epoch_", names(x), value = TRUE)

    if (length(epoch_names) > 0) {
      x <- calculate_epoched_confounds(x, epoch_names, hz, verbose)
    }
  }

  return(x)
}

#' Block-by-block epoch and baseline handler
#'
#' This function processes a single block of pupil data to extract epochs and
#' optionally compute and apply baseline corrections. It handles the core
#' epoching and baselining logic for a single block of data.
#'
#' This function is called by the internal [epoch_pupil()] function.
#'
#' @param x An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param blk A list containing block metadata
#' @param lab Label for the epoch output
#' @param evs Events specification for epoching (character vector or list)
#' @param lims Time limits for epochs (numeric vector)
#' @param msg_s Start message string
#' @param msg_e End message string
#' @param c_bline Logical indicating whether to calculate baseline
#' @param a_bline Logical indicating whether to apply baseline correction
#' @param bline_type Type of baseline correction ("sub" or "div")
#' @param bline_evs Events specification for baseline calculation
#' @param bline_per Baseline period specification
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch and baseline results
#'
#' @keywords internal
epoch_and_baseline_block <- function(x, blk, lab, evs, lims, msg_s, msg_e,
                                     c_bline, a_bline, bline_type,
                                     bline_evs, bline_per, hz, verbose) {
  # input validation ---------------------------------------------------
  if (!is.list(x$timeseries)) {
    stop("Input timeseries must be a list of blocks")
  }

  x$timeseries <- lapply(x$timeseries, function(block) {
    if (data.table::is.data.table(block)) as.data.frame(block) else block
  })

  baseline_id <- NULL
  computed_baselines <- NULL
  n_events <- blk$n_events
  block_id <- blk$id
  block_name <- blk$name
  block_data <- x$timeseries[[block_name]]
  block_events <- x$events[[block_name]]

  dt <- data.table::as.data.table(block_data)

  if (!"time_orig" %in% names(dt)) {
    stop(
      sprintf(
        "Block '%s' doesn't contain the expected `time_orig` column."
      )
    )
  }

  data.table::setkey(dt, "time_orig")

  # epoch logic --------------------------------------------------------
  if (!is.list(evs) || length(evs) != 3) { # i.e., manual method
    timestamps <- get_timestamps(evs, block_events, msg_s, msg_e, lims)
  } else {
    timestamps <- NULL
  }

  result <- process_epoch_and_baselines(
    list(timeseries = dt, events = block_events),
    timestamps,
    evs,
    lims,
    hz,
    verbose
  )

  epoch_id <- make_epoch_label(evs, lab, result)

  # baseline logic -----------------------------------------------------
  baseline_id <- NULL
  computed_baselines <- NULL

  if (c_bline && length(result) > 0) {
    bline_msg_s <- bline_evs[1]
    bline_msg_e <- bline_evs[2]

    bline_matches <- get_timestamps(bline_evs, block_events,
      bline_msg_s, bline_msg_e, bline_per,
      baseline_mode = TRUE
    )

    check_baseline_epoch_counts(timestamps, bline_matches)

    baseline_epochs <- extract_baseline_epochs(
      x, block_data, bline_evs, bline_per, bline_matches, hz
    )

    computed_baselines <- compute_baseline(
      x, result, baseline_epochs, bline_type,
      epoch_events = evs, baseline_events = bline_evs
    )

    baseline_id <- make_baseline_label(computed_baselines, epoch_id)

    if (a_bline) {
      for (i in seq_len(length(result))) {
        result[[i]][[computed_baselines$baseline_cor_col_name]] <-
          computed_baselines$baseline_cor_epochs[[i]]
      }
    }

    computed_baselines[["info"]] <- list(
      calc_baseline = c_bline,
      apply_baseline = a_bline,
      baseline_type = bline_type,
      baseline_events = bline_evs,
      baseline_period = bline_per,
      epoch_events = evs,
      epoch_limits = lims,
      n_epochs = length(result),
      n_baseline_epochs = length(baseline_epochs)
    )
  }

  epoched_result <- convert_nested_dt(result$epochs)
  epoch_df <- do.call(rbind.data.frame, result)

  if (a_bline && grepl("_z", x$latest) && length(result) > 0) {
    bline_col_name <- dplyr::sym(computed_baselines$baseline_cor_col_name)
    bline_z_col_name <- paste0(bline_col_name, "_z")

    epoch_df <- epoch_df |>
      dplyr::mutate(!!bline_z_col_name := get_zscores(!!bline_col_name))
  }

  list(
    epoch = list(id = epoch_id, res = epoch_df),
    baseline = list(id = baseline_id, res = computed_baselines)
  )
}

#' Epoch and baseline processor
#'
#' This function processes a single block of pupil data to extract epochs and
#' optionally compute and apply baseline corrections. It handles the core
#' epoching and baselining logic for a single block of data.
#'
#' This function is called by the internal [epoch_and_baseline_block()]
#' function.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param timestamps A list containing start and end timestamps
#' @param evs Events specification for epoching (character vector or list)
#' @param lims Time limits for epochs (numeric vector)
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch and baseline results
#'
#' @keywords internal
process_epoch_and_baselines <- function(eyeris, timestamps, evs,
                                        lims, hz, verbose) {
  n_timestamps <- nrow(timestamps$start)

  if (n_timestamps == 0 && !is.null(n_timestamps)) {
    if (verbose) {
      alert("info", " * No timestamps to process in this block... skipping.")
    }

    return(list())
  }

  epochs <- NULL

  if (is.character(evs) && length(evs) == 1) {
    if (is.null(lims)) {
      epochs <- eyeris |>
        epoch_only_start_msg(timestamps$start, hz, verbose)
    } else {
      epochs <- eyeris |>
        epoch_start_msg_and_limits(timestamps$start, lims, hz, verbose)
    }
  } else if (is.character(evs) && length(evs) == 2) {
    epochs <- eyeris |>
      epoch_start_end_msg(timestamps$start, timestamps$end, hz, verbose)
  } else if (is.list(evs)) {
    epochs <- eyeris |>
      epoch_manually(evs, hz, verbose)
  }

  if (!is.null(n_timestamps) &&
        length(epochs) > 0 &&
        length(epochs) != n_timestamps) {
    stop(sprintf(
      paste0(
        "Expected %d samples but got %d samples.",
        "Check data for a possible matching error.",
        n_timestamps, length(epochs)
      )
    ))
  }

  if (verbose) {
    alert("success", "Done!")
  }

  epochs
}

#' Manually epoch using provided start/end dataframes of timestamps
#'
#' This function manually epochs data using provided start/end dataframes
#' of timestamps.
#'
#' This function is called by the internal [process_epoch_and_baselines()]
#' function.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param ts_list A list containing start/end dataframes of timestamps
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch results
#'
#' @keywords internal
epoch_manually <- function(eyeris, ts_list, hz, verbose) {
  s_df <- ts_list[[1]]
  e_df <- ts_list[[2]]
  block_num <- ts_list[[3]]

  if (!is.data.frame(s_df)) {
    stop("List item 1 must be a data frame (`start_events`)!")
  }

  if (!is.data.frame(e_df)) {
    stop("List item 2 must be a data frame (`end_events`)!")
  }

  if (!is.numeric(block_num)) {
    stop("List item 3 must be a numeric (`block`)!")
  }

  epochs <- vector("list", nrow(s_df))

  if (verbose) {
    pb <- counter_bar(nrow(s_df), msg = "Epoching events", width = 70)
  }

  for (i in seq_len(nrow(s_df))) {
    i_start <- s_df$time[i]
    i_end <- e_df$time[i]

    current_epoch <- eyeris |>
      purrr::pluck("timeseries") |>
      slice_epoch(i_start, i_end)

    duration <- nrow(current_epoch) / hz
    n_samples <- duration * hz

    start_metadata_vals <- dplyr::rename_with(s_df, ~ paste0("start_", .x))
    end_metadata_vals <- dplyr::rename_with(e_df, ~ paste0("end_", .x))

    metadata_vals <- dplyr::bind_cols(
      start_metadata_vals[i, ],
      end_metadata_vals[i, ]
    )

    epochs[[i]] <- current_epoch |>
      dplyr::mutate(
        timebin = seq(0, duration, length.out = n_samples)
      ) |>
      dplyr::bind_cols(metadata_vals)

    if (verbose) {
      tick(pb, by = 1)
    }
  }

  epochs
}

#' Epoch based on a single event message (without explicit limits)
#'
#' This function epochs data based on a single event message
#' (i.e., without explicit limits).
#'
#' This function is called by the internal [epoch_only_start_msg()] function.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param start A dataframe containing the start timestamps
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch results
#'
#' @keywords internal
epoch_only_start_msg <- function(eyeris, start, hz, verbose) {
  all_epochs <- slice_epochs_no_limits(eyeris$timeseries, start)

  epochs <- vector("list", nrow(start))

  if (verbose) {
    pb <- counter_bar(nrow(start), msg = "Epoching events", width = 70)
  }

  for (i in seq_len(nrow(start))) {
    metadata_vals <- index_metadata(start, i)

    current_epoch <- all_epochs[[i]]

    duration <- nrow(current_epoch) / hz
    n_samples <- duration * hz

    epochs[[i]] <- current_epoch |>
      dplyr::mutate(
        timebin = seq(0, duration, length.out = n_samples)
      ) |>
      dplyr::bind_cols(metadata_vals) |>
      dplyr::select(-time)

    if (verbose) {
      tick(pb, by = 1)
    }
  }

  epochs
}

#' Epoch using a start message with fixed limits around it
#'
#' This function epochs data using a start message with fixed limits around it.
#'
#' This function is called by the internal [epoch_start_msg_and_limits()]
#' function.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param start A dataframe containing the start timestamps
#' @param lims Time limits for epochs (numeric vector)
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch results
#'
#' @keywords internal
epoch_start_msg_and_limits <- function(eyeris, start, lims, hz, verbose) {
  duration <- sum(abs(lims[1]), abs(lims[2]))
  n_samples <- duration / (1 / hz)
  n_events <- nrow(start)

  epochs <- vector(mode = "list", length = nrow(start)) # pre-alloc list

  if (verbose) {
    pb <- counter_bar(n_events, msg = "Epoching events", width = 70)
  }

  for (i in seq_len(n_events)) {
    metadata_vals <- index_metadata(start, i)

    epochs[[i]] <- eyeris |>
      purrr::pluck("timeseries") |>
      slice_epochs_with_limits(
        start$time[i], lims, hz
      ) |>
      dplyr::mutate(
        timebin = seq(from = 0, to = duration, length.out = n_samples),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals) |>
      dplyr::select(-time)

    if (verbose) {
      tick(pb, by = 1)
    }
  }

  epochs
}

#' Epoch using a start and an end message (explicit timestamps)
#'
#' This function epochs data using a start and an end message
#' (i.e., explicit timestamps).
#'
#' This function is called by the internal [epoch_start_end_msg()] function.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param start A dataframe containing the start timestamps
#' @param end A dataframe containing the end timestamps
#' @param hz Sampling rate in Hz
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A list containing epoch results
#'
#' @keywords internal
epoch_start_end_msg <- function(eyeris, start, end, hz, verbose) {
  if (nrow(start) != nrow(end)) {
    stop("Start and end timestamps must have the same number of rows")
  }

  epochs <- vector("list", nrow(start))

  if (verbose) {
    pb <- counter_bar(nrow(start), msg = "Epoching events", width = 70)
  }

  for (i in seq_len(nrow(start))) {
    i_start <- start$time[i]
    i_end <- end$time[i]

    start_metadata_vals <- dplyr::rename_with(
      index_metadata(start, i), ~ paste0("start_", .x)
    )

    end_metadata_vals <- dplyr::rename_with(
      index_metadata(end, i), ~ paste0("end_", .x)
    )

    metadata_vals <- start_metadata_vals |>
      dplyr::bind_cols(end_metadata_vals)

    duration <- (i_end - i_start) / hz
    n_samples <- duration * hz

    epochs[[i]] <- eyeris |>
      purrr::pluck("timeseries") |>
      dplyr::filter(time_orig >= s, time_orig < e) |>
      dplyr::mutate(
        timebin = seq(0, duration, length.out = n_samples)
      ) |>
      dplyr::bind_cols(metadata_vals)

    if (verbose) {
      tick(pb, by = 1)
    }
  }

  epochs
}
