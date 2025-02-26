make_baseline_label <- function(baselined_data, epoch_id) {
  paste0(
    "baseline_", baselined_data$baseline_cor_col_name,
    "_", epoch_id
  )
}

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

compute_baseline <- function(x, epochs, baseline_epochs, mode) {
  # compute baseline on pre z-scored data
  pupil_col <- gsub("_z", "", x$latest)
  new_col <- paste0(pupil_col, "_", mode, "_bline")

  # pre-alloc output data structs
  baseline_data <- vector(mode = "list", length = length(baseline_epochs))
  baseline_means <- rep(NA, length(baseline_epochs))
  method <- "none... skipped"

  for (i in seq_len(length(baseline_epochs))) {
    baseline_window_pupil <- baseline_epochs[[i]][[pupil_col]]
    baseline_avg <- mean(baseline_window_pupil, na.rm = TRUE)
    pupil_dat <- epochs[[i]][[pupil_col]]

    if (mode == "sub") {
      method <- "subtractive"
      baseline_removed <- pupil_dat - baseline_avg
    } else if (mode == "div") {
      method <- "divisive"
      check_baseline_mean(baseline_avg)
      baseline_removed <- pupil_dat / baseline_avg
    }

    baseline_data[[i]] <- baseline_removed
    baseline_means[i] <- baseline_avg
  }

  list(
    baseline_cor_epochs = baseline_data,
    baseline_means_by_epoch = baseline_means,
    baseline_correction_method = method,
    baseline_cor_col_name = new_col
  )
}
