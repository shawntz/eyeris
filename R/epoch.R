#' Epoch (and baseline) pupil data based on custom event message structure
#'
#' Intended to be used as the final preprocessing step. This function creates
#' data epochs of either fixed or dynamic durations with respect to provided
#' `events` and time `limits`, and also includes an intuitive metadata parsing
#' feature where additional trial data embedded within event messages can easily
#' be identified and joined into the resulting epoched data frames.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load()].
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
#' `list(data.frame(time = c(...), msg = c(...)),`
#' `data.frame(time = c(...), msg = c(...)))`
#'
#' where the first data.frame indicates the `start` event timestamp and message
#' string pairs, and the second data.frame indicates the `end` event timestamp
#' and message string pairs.
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
#' strings: `type`: `('START', 'STOP')`, and `trial`: `(1, 1)`.
#' @param limits A vector of 2 values (start, end) in seconds, indicating where
#' trial extraction should occur centered around any given `start` message
#' string in the `events` parameter.
#' @param label An (optional) string you can provide to customize the name of
#' the resulting `eyeris` class object containing the epoched data frame. If
#' left as `NULL` (default), then list item will be called `epoch_xyz`, where
#' `xyz` will be a sanitized version of the original `start` event string you
#' provided for matching. If you choose to specify a `label` here, then the
#' resulting list object name will take the form: `epoch_label`.
#' @param calc_baseline A flag indicated whether to perform baseline correction.
#' Note, setting `calc_baseline` to TRUE alone will only compute the baseline
#' period, but will not apply it to the preprocessed timeseries unless
#' `apply_baseline` is also set to TRUE.
#' @param apply_baseline A flag indicating whether to apply the calculated
#' baseline to the pupil timeseries. The baseline correction will be applied to
#' the pupil from the latest preprocessing step.
#' @param baseline_type Whether to perform *subtractive* (`sub`) or *divisive*
#' (`div`) baseline correction. Defaults to `sub`.
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
#' `baseline_events` argument.
#' @param hz Data sampling rate. If not specified, will use the value contained
#' within the tracker's metadata.
#'
#' @return Updated `eyeris` object with dataframes containing the epoched data
#' (`epoch_`).
#'
#' @examples
#' \dontrun{
#' eye_preproc <- system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore()
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
#'       data.frame(time = c(11337158), msg = c("RESPONSE_22")) # end events
#'     )
#'   )
#'
#' # example 6: manual start/end event pairs
#' # note: set `msg` to NA if you only want to pass in start/end timestamps
#' eye_preproc |>
#'   eyeris::epoch(
#'     events = list(
#'       data.frame(time = c(11334491), msg = NA), # start events
#'       data.frame(time = c(11337158), msg = NA) # end events
#'     )
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
#'     calc_baseline = TRUE,
#'     apply_baseline = TRUE,
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
#'     calc_baseline = TRUE,
#'     apply_baseline = TRUE,
#'     baseline_type = "sub", # "sub"tractive baseline calculation is default
#'     baseline_events = c("DELAY_START_*",
#'                         "DELAY_STOP_*")
#'   )
#'
#' # example 9: additional (potentially helpful) example
#' start_events <- data.frame(time = c(11334491, 11338691),
#'                            msg = c("TRIALID 22", "TRIALID 23"))
#' end_events <- data.frame(time = c(11337158, 11341292),
#'                          msg = c("RESPONSE_22", "RESPONSE_23"))
#' eye_preproc |>
#'   eyeris::epoch(events = list(start_events, end_events))
#' }
#'
#' @export
epoch <- function(eyeris, events, limits = NULL, label = NULL,
                  calc_baseline = FALSE, apply_baseline = FALSE,
                  baseline_type = c("sub", "div"), baseline_events = NULL,
                  baseline_period = NULL, hz = NULL) {
  return(pipeline_handler(
    eyeris, epoch_pupil, "epoch", events, limits, label, calc_baseline,
    apply_baseline, baseline_type, baseline_events, baseline_period, hz
  ))
}

epoch_pupil <- function(x, prev_op, evs, lims, label, c_bline, a_bline,
                        bline_type = c("sub", "div"), bline_evs, bline_per,
                        hz) {
  if (is.null(hz)) {
    hz <- x$info$sample.rate
  }

  msg_s <- evs[1]
  msg_e <- evs[2]

  timestamped_events <- x |>
    purrr::pluck("events")

  if (!is.list(evs)) {
    timestamps <- get_timestamps(evs, timestamped_events, msg_s, msg_e, lims)
    timestamps_s <- timestamps$start
    timestamps_e <- timestamps$end
  }

  # run 1 of 4 possible epoch modes
  if (is.character(evs) && length(evs) == 1) {
    if (is.null(lims)) {
      # events == "(start) regex pattern string" & limits is NULL =>
      # defaults to grabbing event string of the next, matching event string
      epoched_data <- epoch_only_start_msg(x, timestamps_s, hz)
    } else if (is.numeric(lims) && length(lims) == 2) {
      # events == "(start) regex pattern string" & limits == (start, end) =>
      # returns data slice from limits, centered on the event string
      epoched_data <- epoch_start_msg_and_limits(x, timestamps_s, lims, hz)
    }
  } else if (is.character(evs) && length(evs) == 2) {
    # events == ("start_regex_string", "end_regex_string") & limits is NULL =>
    # grabs everything in between the two messages
    # (warn if limits isn't NULL that it will be ignored)
    if (!is.null(lims)) cli::cli_alert_warning("`limits` are being ignored!")
    epoched_data <- epoch_start_end_msg(x, timestamps_s, timestamps_e, hz)
  } else if (is.list(evs)) {
    # events == list of 2 (n x 2) dataframes of timestamps to manually select =>
    # (warn if limits isn't NULL that it will be ignored)
    # (throw error if any messages + timestamp combos don't exist in the
    # eyeris$events object)
    if (!is.null(lims)) cli::cli_alert_warning("`limits` are being ignored!")
    epoched_data <- epoch_manually(x, evs, hz)
  }

  epoch_id <- make_epoch_label(evs, label, epoched_data)

  if (c_bline) {
    bline_type <- tolower(bline_type)
    bline_type <- match.arg(bline_type)

    bline_msg_s <- bline_evs[1]
    bline_msg_e <- bline_evs[2]

    bline_matches <- get_timestamps(bline_evs, timestamped_events, bline_msg_s,
      bline_msg_e, bline_per,
      baseline_mode = TRUE
    )

    check_baseline_epoch_counts(timestamps, bline_matches)
    baseline_epochs <- extract_baseline_epochs(
      x, bline_evs, bline_per,
      bline_matches, hz
    )
    computed_baselines <- compute_baseline(
      x, epoched_data, baseline_epochs,
      bline_type
    )

    if (a_bline) {
      for (i in seq_len(length(epoched_data))) {
        epoched_data[[i]][[computed_baselines$baseline_corrected_col_name]] <-
          computed_baselines$baseline_corrected_epochs[[i]]
      }
    }

    baseline_id <- make_baseline_label(computed_baselines, epoch_id)

    baseline_args <- list(
      calc_baseline = c_bline,
      apply_baseline = a_bline,
      baseline_type = bline_type,
      baseline_events = bline_evs,
      baseline_period = bline_per
    )

    computed_baselines[["info"]] <- baseline_args

    x[[baseline_id]] <- computed_baselines
  }

  epochs_df <- do.call(rbind.data.frame, epoched_data)
  x[[epoch_id]] <- epochs_df

  return(x)
}

get_timestamps <- function(evs, timestamped_events, msg_s, msg_e, limits,
                           baseline_mode = FALSE) {
  start_ts <- NULL
  end_ts <- NULL

  if (baseline_mode) { ## baseline calculation enabled
    start_ts <- parse_timestamps(evs, timestamped_events, msg_s)

    if (!is.na(msg_e)) {
      end_ts <- parse_timestamps(evs, timestamped_events, msg_e)
    }
  } else { ## baseline calculation disabled
    if (!is.list(evs)) {
      start_ts <- parse_timestamps(evs, timestamped_events, msg_s)
    }

    if (is.list(evs)) {
      msg_s <- msg_s[[1]]$msg
      msg_e <- msg_e[[1]]$msg
    }

    if (!any(is.na(msg_e))) {
      if (!any(endsWith(msg_s, "*"))) {
        if (!any(is.na(evs[2]))) {
          end_ts <- parse_timestamps(evs, timestamped_events, msg_e)
        }
      } else {
        if (is.null(limits)) {
          check_limits(limits)
        }
      }
    }
  }

  out_list <- list(
    start = start_ts,
    end = end_ts
  )

  return(out_list)
}

parse_timestamps <- function(evs, timestamped_events, msg) {
  timestamps <- timestamped_events |>
    parse_events_and_metadata(msg)

  return(timestamps)
}

make_epoch_label <- function(evs, label, epoched_data) {
  if (is.null(label) && !is.list(evs)) {
    epoch_id <- sanitize_event_tag(evs[1])
  } else if (is.null(label) && is.list(evs)) {
    epoch_id <- sanitize_event_tag(paste0(
      epoched_data[[1]]$start_msg[1], epoched_data[[1]]$end_msg[1]
    ))
  } else {
    epoch_id <- paste0("epoch_", label)
  }

  return(epoch_id)
}

index_metadata <- function(x, i) {
  return(x[i, ])
}

parse_events_and_metadata <- function(events, metadata_template) {
  special_chars <- c(
    "\\", ".", "+", "*", "?", "^", "$", "(", ")", "[", "]",
    "{", "}", "|"
  )

  event_messages <- events |>
    dplyr::pull(text)

  if (any(endsWith(metadata_template, "*"))) { # wildcard mode
    prefix <- substr(metadata_template, 1, nchar(metadata_template) - 1)

    for (char in special_chars) { # escape special chars
      prefix <- stringr::str_replace_all(
        prefix, stringr::fixed(char),
        paste0("\\", char)
      )
    }

    regex_pattern <- paste0("^", prefix, ".*$")
    matches <- stringr::str_detect(event_messages, regex_pattern)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern,
      event_message = event_messages,
      matched_event = ifelse(matches, event_messages, NA_character_)
    ) |>
      tidyr::drop_na("matched_event")
  } else { # template (non-wildcard) mode
    template <- metadata_template
    placeholders <- unlist(stringr::str_extract_all(template, "\\{[^{}]+\\}"))
    placeholder_names <- gsub("[{}]", "", placeholders)

    for (i in placeholder_names) {
      placeholder <- paste0("\\{", i, "\\}")
      template <- stringr::str_replace(template, placeholder, "(.*?)")
    }

    regex_pattern <- paste0("^", template, "$")
    matches <- stringr::str_match(event_messages, regex_pattern) |>
      as.data.frame()
    colnames(matches) <- c("matched_event", placeholder_names)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern
    ) |>
      dplyr::bind_cols(matches) |>
      tidyr::drop_na("matched_event")
  }

  epoched_timeseries <- events |>
    dplyr::mutate(matched_event = text) |>
    dplyr::right_join(result, by = "matched_event") |>
    dplyr::select(-block, -text) |>
    dplyr::relocate(matched_event, .after = matching_pattern)

  return(epoched_timeseries)
}

sanitize_event_tag <- function(string) {
  string <- string |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^[:alnum:] ]", " ") |>
    stringr::str_split("\\s+")

  words <- string[[1]]
  words[-1] <- stringr::str_to_title(words[-1])

  camel_case_str <- paste0(words, collapse = "")
  sanitized_str <- paste0("epoch_", camel_case_str)
  sanitized_str <- gsub("\\d", "", sanitized_str)

  return(sanitized_str)
}

slice_epoch <- function(x_raw, s, e) {
  epoch_df <- x_raw |>
    dplyr::filter(time_orig >= s & time_orig < e)

  return(epoch_df)
}

slice_epochs_no_limits <- function(x_raw, all_ts) {
  epochs <- vector(mode = "list", length = length(all_ts)) # pre-alloc list

  for (i in seq_along(all_ts$time)) {
    current_time <- all_ts$time[i]

    if (i < length(all_ts$time)) {
      next_time <- all_ts$time[i + 1]
    } else {
      # for final trial with no explicit end timestamp,
      # infer from duration of the penultimate trial
      penult_dur <- all_ts$time[i] - all_ts$time[i - 1]
      next_time <- current_time + penult_dur
    }

    epochs[[i]] <- slice_epoch(x_raw, current_time, next_time)
  }

  return(epochs)
}

slice_epochs_with_limits <- function(x_raw, cur_ts, lims, hz) {
  s_time <- cur_ts + (lims[1] * 1000)
  e_time <- cur_ts + (lims[2] * 1000)
  epoch_df <- slice_epoch(x_raw, s_time, e_time)

  duration <- sum(abs(lims[1]), abs(lims[2]))
  n_samples <- duration / (1 / hz)

  epoch_df <- epoch_df[1:n_samples, ]
  return(epoch_df)
}

epoch_manually <- function(eyeris, ts_list, hz) {
  check_epoch_manual_input_data(ts_list)
  check_epoch_manual_input_dfs(ts_list)

  s_df <- ts_list[[1]]
  e_df <- ts_list[[2]]

  epochs <- vector(mode = "list", length = nrow(s_df)) # pre-alloc list

  for (i in seq_along(s_df$time)) {
    i_start <- s_df$time[i]
    i_end <- e_df$time[i]
    current_epoch <- slice_epoch(eyeris$timeseries, i_start, i_end)
    duration <- nrow(current_epoch) / hz
    n_samples <- duration * hz

    start_metadata_vals <- s_df |>
      dplyr::rename_with(~ paste0("start_", .x))

    end_metadata_vals <- e_df |>
      dplyr::rename_with(~ paste0("end_", .x))

    metadata_vals <- dplyr::bind_cols(start_metadata_vals, end_metadata_vals)

    current_epoch <- current_epoch |>
      dplyr::mutate(
        timebin = seq(
          from = 0,
          to = duration,
          length.out = n_samples
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals[i, ])

    epochs[[i]] <- current_epoch
  }

  return(epochs)
}

epoch_only_start_msg <- function(eyeris, start, hz) {
  epochs <- vector(mode = "list", length = nrow(start)) # pre-alloc list

  all_epochs <- slice_epochs_no_limits(eyeris$timeseries, start)

  for (i in seq_len(nrow(start))) {
    metadata_vals <- index_metadata(start, i)
    current_epoch <- all_epochs[[i]]
    duration <- nrow(current_epoch) / hz
    n_samples <- duration * hz

    current_epoch <- current_epoch |>
      dplyr::mutate(
        timebin = seq(
          from = 0,
          to = duration,
          length.out = n_samples
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals) |>
      dplyr::select(-time)

    epochs[[i]] <- current_epoch
  }

  return(epochs)
}

epoch_start_msg_and_limits <- function(eyeris, start, lims, hz) {
  epochs <- vector(mode = "list", length = nrow(start)) # pre-alloc list

  for (i in seq_len(nrow(start))) {
    metadata_vals <- index_metadata(start, i)

    current_epoch <- slice_epochs_with_limits(
      eyeris$timeseries,
      start$time[i], lims, hz
    )

    duration <- sum(abs(lims[1]), abs(lims[2]))
    n_samples <- duration / (1 / hz)

    current_epoch <- current_epoch |>
      dplyr::mutate(
        timebin = seq(
          from = 0,
          to = duration,
          length.out = n_samples
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals) |>
      dplyr::select(-time)

    epochs[[i]] <- current_epoch
  }

  return(epochs)
}

epoch_start_end_msg <- function(eyeris, start, end, hz) {
  check_start_end_timestamps(start, end)

  epochs <- vector(mode = "list", length = nrow(start)) # pre-alloc list

  for (i in seq_len(nrow(start))) {
    i_start <- start$time[i]
    i_end <- end$time[i]

    start_metadata_vals <- index_metadata(start, i) |>
      dplyr::rename_with(~ paste0("start_", .x))

    end_metadata_vals <- index_metadata(end, i) |>
      dplyr::rename_with(~ paste0("end_", .x))

    metadata_vals <- dplyr::bind_cols(start_metadata_vals, end_metadata_vals)

    duration <- (i_end - i_start) / hz
    n_samples <- duration * hz

    epochs[[i]] <- eyeris |>
      purrr::pluck("timeseries") |>
      dplyr::filter(time_orig >= i_start & time_orig < i_end) |>
      dplyr::mutate(
        timebin = seq(
          from = 0,
          to = duration,
          length.out = n_samples
        ),
        .after = time_orig
      ) |>
      dplyr::mutate(!!!metadata_vals)
  }

  return(epochs)
}
