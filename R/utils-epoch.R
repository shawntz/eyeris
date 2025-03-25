# Index metadata
index_metadata <- function(x, i) {
  x[i, ]
}

# Generate list label for any given epoch
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
      stop("No epoched data available for label generation")
    }
  } else {
    paste0("epoch_", label)
  }
}

# Sanitize event tag string into a canonical epoch label
sanitize_event_tag <- function(string) {
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
  paste0("epoch_", gsub("\\d", "", camel_case_str))
}

# Slice epoch from the raw timeseries data based on start and end times
slice_epoch <- function(x_raw, s, e) {
  epoch_df <- dplyr::filter(x_raw, time_orig >= s, time_orig < e)

  if ("block" %in% names(x_raw)) {
    epoch_df$block <- unique(x_raw$block)[1]
  }

  epoch_df
}

# Slice epochs with no explicit limits (using adjacent timestamps)
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

# Slice epochs with explicit limits
slice_epochs_with_limits <- function(x_raw, cur_ts, lims, hz) {
  s_time <- cur_ts + (lims[1] * 1000)
  e_time <- cur_ts + (lims[2] * 1000)
  epoch_df <- slice_epoch(x_raw, s_time, e_time)

  duration <- sum(abs(lims[1]), abs(lims[2]))
  n_samples <- duration / (1 / hz)

  epoch_df <- epoch_df[1:n_samples, ]
}

# Obtain timestamps from the events data
get_timestamps <- function(evs, timestamped_events, msg_s, msg_e, limits,
                           baseline_mode = FALSE) {
  start_ts <- NULL
  end_ts <- NULL

  if (baseline_mode) {
    start_ts <- merge_events_with_timeseries(timestamped_events, msg_s)
    if (!is.na(msg_e)) {
      end_ts <- merge_events_with_timeseries(timestamped_events, msg_e)
    }
  } else { # baseline calculation disabled
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
  return(list(
    start = start_ts,
    end = end_ts
  ))
}

# Process event messages, extract metadata, and merge with timeseries
merge_events_with_timeseries <- function(events, metadata_template,
                                         merge = TRUE) {
  special_chars <- c(
    "\\", ".", "+", "*", "?", "^",
    "$", "(", ")", "[", "]", "{", "}", "|"
  )
  event_messages <- dplyr::pull(events, text)
  event_times <- dplyr::pull(events, time)

  # first check for exact matches (like "11")
  if (!grepl("[*{}]", metadata_template)) {
    matches <- event_messages == metadata_template
    matched_indices <- which(matches)

    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = metadata_template,
      event_message = event_messages[matched_indices],
      matched_event = event_messages[matched_indices],
      time = event_times[matched_indices]
    ) |>
      tidyr::drop_na(matched_event)

    return(result)
  }

  if (any(endsWith(metadata_template, "*"))) { # wildcard mode
    prefix <- substr(metadata_template, 1, nchar(metadata_template) - 1)

    for (char in special_chars) {
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
      tidyr::drop_na(matched_event)
  } else { # template mode
    template <- metadata_template
    placeholders <- unlist(stringr::str_extract_all(template, "\\{[^{}]+\\}"))
    placeholder_names <- gsub("[{}]", "", placeholders)

    for (i in seq_along(placeholder_names)) {
      placeholder <- paste0("\\{", placeholder_names[i], "\\}")
      template <- stringr::str_replace(template, placeholder, "(.*?)")
    }

    regex_pattern <- paste0("^", template, "$")
    matches <- stringr::str_match(event_messages, regex_pattern)
    matches_df <- as.data.frame(matches)
    colnames(matches_df) <- c("matched_event", placeholder_names)
    result <- dplyr::tibble(
      template = metadata_template,
      matching_pattern = regex_pattern
    ) |>
      dplyr::bind_cols(matches_df) |>
      tidyr::drop_na(matched_event)
  }

  if (merge) {
    result <- dplyr::distinct(result)

    epoched_timeseries <- events |>
      dplyr::mutate(matched_event = text) |>
      dplyr::right_join(result, by = "matched_event") |>
      dplyr::select(-block, -text) |>
      dplyr::relocate(matched_event, .after = matching_pattern)

    return(epoched_timeseries)
  } else {
    return(result)
  }
}
