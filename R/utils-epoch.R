#' Index metadata from data frame
#'
#' Extracts a single row of metadata from a data frame.
#'
#' @param x The data frame to index
#' @param i The row index
#'
#' @return A single row from the data frame
#'
#' @keywords internal
index_metadata <- function(x, i) {
  x[i, ]
}

#' Generate epoch label from events and data
#'
#' Creates a standardized label for epoch data based on events or
#' user-provided label.
#'
#' @param evs Event messages or list of events
#' @param label User-provided label (optional)
#' @param epoched_data List of epoched data for label generation
#'
#' @return A character string with the epoch label
#'
#' @keywords internal
make_epoch_label <- function(evs, label, epoched_data) {
  if (is.null(label) && !is.list(evs)) {
    sanitize_event_tag(evs[1])
  } else if (is.null(label) && is.list(evs)) {
    if (length(epoched_data) > 0) {
      sanitize_event_tag(paste0(
        epoched_data[[1]]$start_msg[1],
        epoched_data[[1]]$end_msg[1]
      ))
    } else {
      log_error("No epoched data available for label generation")
    }
  } else {
    paste0("epoch_", label)
  }
}

#' Sanitize event tag string into canonical epoch label
#'
#' Converts event tag strings into standardized epoch labels by removing
#' special characters and converting to camel case.
#'
#' @param string The event tag string to sanitize
#' @param prefix The prefix to add to the sanitized string (default: "epoch_")
#'
#' @return A sanitized epoch label string
#'
#' @keywords internal
sanitize_event_tag <- function(string, prefix = "epoch_") {
  sanitized <- string |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^[:alnum:] ]", " ") |>
    stringr::str_squish() |>
    stringr::str_split(" ") |>
    unlist()

  if (length(sanitized) > 1) {
    sanitized[-1] <- stringr::str_to_title(sanitized[-1])
  }

  camel_case_str <- paste0(sanitized, collapse = "")
  paste0(prefix, gsub("\\d", "", camel_case_str))
}

#' Slice epoch from raw time series data
#'
#' Extracts a time segment from raw time series data based on start and
#' end times.
#'
#' @param x_raw The raw time series data frame
#' @param s Start time in milliseconds
#' @param e End time in milliseconds
#'
#' @return A data frame containing the epoch data
#'
#' @keywords internal
slice_epoch <- function(x_raw, s, e) {
  epoch_df <- dplyr::filter(x_raw, time_orig >= s, time_orig < e)

  if ("block" %in% names(x_raw)) {
    epoch_df$block <- unique(x_raw$block)[1]
  }

  epoch_df
}

#' Slice epochs with no explicit limits
#'
#' Creates epochs using adjacent time stamps without explicit time limits.
#'
#' @param x_raw The raw time series data frame
#' @param all_ts A data frame containing timestamp information
#'
#' @return A list of epoch data frames
#'
#' @keywords internal
slice_epochs_no_limits <- function(x_raw, all_ts) {
  epochs <- vector("list", length(all_ts$time))

  for (i in seq_along(all_ts$time)) {
    current_time <- all_ts$time[i]

    if (i < length(all_ts$time)) {
      next_time <- all_ts$time[i + 1]
    } else {
      if (i > 1) {
        next_time <- current_time + (current_time - all_ts$time[i - 1])
      } else {
        next_time <- current_time
      }
    }

    epochs[[i]] <- slice_epoch(x_raw, current_time, next_time)
  }

  epochs
}

#' Slice epochs with explicit limits
#'
#' Creates epochs using explicit time limits around a central timestamp.
#'
#' @param x_raw The raw time series data frame
#' @param cur_ts The central timestamp
#' @param lims Time limits in seconds (negative for before, positive for after)
#' @param hz Sampling rate in Hz
#'
#' @return A data frame containing the epoch data
#'
#' @keywords internal
slice_epochs_with_limits <- function(x_raw, cur_ts, lims, hz) {
  s_time <- cur_ts + (lims[1] * 1000)
  e_time <- cur_ts + (lims[2] * 1000)
  epoch_df <- slice_epoch(x_raw, s_time, e_time)

  duration <- sum(abs(lims[1]), abs(lims[2]))
  n_samples <- duration / (1 / hz)

  epoch_df <- epoch_df[1:n_samples, ]
}

#' Obtain timestamps from events data
#'
#' Extracts start and end timestamps from events data based on message patterns.
#'
#' @param evs Event messages or list of events
#' @param timestamped_events Events data frame with timestamps
#' @param msg_s Start message pattern
#' @param msg_e End message pattern
#' @param limits Time limits for wildcard mode
#' @param baseline_mode Whether in baseline calculation mode
#'
#' @return A list containing start and end timestamps
#'
#' @keywords internal
get_timestamps <- function(
  evs,
  timestamped_events,
  msg_s,
  msg_e,
  limits,
  baseline_mode = FALSE
) {
  start_ts <- NULL
  end_ts <- NULL

  if (baseline_mode) {
    start_ts <- merge_events_with_timeseries(timestamped_events, msg_s)
    if (!is.na(msg_e)) {
      end_ts <- merge_events_with_timeseries(timestamped_events, msg_e)
    }
  } else {
    # baseline calculation disabled
    if (!is.list(evs)) {
      start_ts <- merge_events_with_timeseries(timestamped_events, msg_s)
    }

    if (is.list(evs)) {
      msg_s <- msg_s[[1]]$msg
      msg_e <- msg_e[[1]]$msg
    }

    if (!any(is.na(msg_e))) {
      if (!any(endsWith(msg_s, "*"))) {
        if (!any(is.na(evs[2]))) {
          end_ts <- merge_events_with_timeseries(timestamped_events, msg_e)
        }
      } else {
        if (is.null(limits)) check_limits(limits)
      }
    }
  }
  return(list(start = start_ts, end = end_ts))
}

#' Extract event identifiers from event messages
#'
#' Extracts identifiers (like image names or trial numbers) from event messages
#' to enable matching between start and end events.
#'
#' @param events Data frame containing event messages
#'
#' @return A vector of extracted identifiers
#'
#' @keywords internal
extract_event_ids <- function(events) {
  # get the appropriate message column
  if ("matched_event" %in% names(events)) {
    messages <- events$matched_event
  } else if ("event_message" %in% names(events)) {
    messages <- events$event_message
  } else if ("text" %in% names(events)) {
    messages <- events$text
  } else {
    # fallback: look for any text-like column
    text_cols <- names(events)[sapply(events, is.character)]
    if (length(text_cols) > 0) {
      messages <- events[[text_cols[1]]]
    } else {
      stop("No text column found in events data frame")
    }
  }

  # extract identifiers using common patterns
  # pattern 1: filename.ext (e.g., "PROBE_S 392.jpg" -> "392.jpg")
  ids <- stringr::str_extract(messages, "[a-zA-Z0-9_-]+\\.[a-zA-Z0-9]+$")

  # pattern 2: trial numbers (e.g., "TRIAL_01" -> "01")
  if (all(is.na(ids))) {
    ids <- stringr::str_extract(messages, "[0-9]+$")
  }

  # pattern 3: general identifier after last space
  if (all(is.na(ids))) {
    ids <- stringr::str_extract(messages, "[^ ]+$")
  }

  # fallback: use the full message if no pattern matches
  if (all(is.na(ids))) {
    ids <- messages
  }

  return(ids)
}

#' Process event messages and merge with time series
#'
#' Matches event messages against templates and extracts metadata,
#' supporting both exact matches and pattern matching with wildcards.
#'
#' @param events Events data frame with timestamps and messages
#' @param metadata_template Template pattern to match against
#' @param merge Whether to merge results (default: `TRUE`)
#'
#' @return A data frame with matched events and extracted metadata
#'
#' @keywords internal
merge_events_with_timeseries <- function(
  events,
  metadata_template,
  merge = TRUE
) {
  special_chars <- c(
    "\\",
    ".",
    "+",
    "*",
    "?",
    "^",
    "$",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "|"
  )

  # use text_unique if available, otherwise fall back to text
  if ("text_unique" %in% colnames(events)) {
    event_messages <- dplyr::pull(events, text_unique)
    event_text_original <- dplyr::pull(events, text)
  } else {
    event_messages <- dplyr::pull(events, text)
    event_text_original <- event_messages
  }

  event_times <- dplyr::pull(events, time)

  # first check for exact matches (like "11")
  if (!grepl("[*{}]", metadata_template)) {
    matches <- event_text_original == metadata_template
    matched_indices <- which(matches)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = metadata_template,
      event_message = event_text_original[matched_indices],
      matched_event = event_messages[matched_indices],
      time = event_times[matched_indices]
    ) |>
      tidyr::drop_na(matched_event)

    return(result)
  }

  if (any(endsWith(metadata_template, "*"))) {
    # wildcard mode
    prefix <- substr(metadata_template, 1, nchar(metadata_template) - 1)

    for (char in special_chars) {
      prefix <- stringr::str_replace_all(
        prefix,
        stringr::fixed(char),
        paste0("\\", char)
      )
    }

    regex_pattern <- paste0("^", prefix, ".*$")
    matches <- stringr::str_detect(event_text_original, regex_pattern)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern,
      event_message = event_text_original,
      matched_event = ifelse(matches, event_messages, NA_character_),
      time = ifelse(matches, event_times, NA_real_)
    ) |>
      tidyr::drop_na(matched_event)
  } else {
    # template mode
    template <- metadata_template
    placeholders <- unlist(stringr::str_extract_all(template, "\\{[^{}]+\\}"))
    placeholder_names <- gsub("[{}]", "", placeholders)

    for (i in seq_along(placeholder_names)) {
      placeholder <- paste0("\\{", placeholder_names[i], "\\}")
      template <- stringr::str_replace(template, placeholder, "(.*?)")
    }

    regex_pattern <- paste0("^", template, "$")
    matches <- stringr::str_match(event_text_original, regex_pattern)
    matches_df <- as.data.frame(matches)
    colnames(matches_df) <- c("matched_event", placeholder_names)

    result_times <- rep(NA_real_, length(event_text_original))
    matched_indices <- which(!is.na(matches[, 1]))
    result_times[matched_indices] <- event_times[matched_indices]

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern
    ) |>
      dplyr::bind_cols(matches_df) |>
      dplyr::mutate(time = result_times) |>
      tidyr::drop_na(matched_event)
  }

  if (merge) {
    result <- dplyr::distinct(result)

    events_mapping <- events |>
      dplyr::select(text_unique, text) |>
      dplyr::rename(event_text_original = text)

    if (!"text_unique" %in% colnames(events)) {
      events_mapping <- events |>
        dplyr::select(text) |>
        dplyr::mutate(text_unique = text) |>
        dplyr::rename(event_text_original = text)
    }

    epoched_timeseries <- events_mapping |>
      dplyr::right_join(result, by = c("text_unique" = "matched_event")) |>
      dplyr::rename(matched_event = event_text_original) |>
      dplyr::relocate(matched_event, .after = matching_pattern)

    return(epoched_timeseries)
  } else {
    # when merge = FALSE, ensure we have the time column
    if (!"time" %in% colnames(result)) {
      # this shouldn't happen, but added for safety
      result$time <- event_times[match(result$matched_event, event_messages)]
    }
    return(result)
  }
}
