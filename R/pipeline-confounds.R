#' Extract confounding variables calculated separately for each pupil data file
#'
#' Calculates various confounding variables for pupil data, including blink
#' statistics, gaze position metrics, and pupil size characteristics. These
#' confounds are calculated separately for each preprocessing step, recording
#' block, and epoched time series in the `eyeris` object.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#'
#' @return An `eyeris` object with a new nested list of data frames:
#' `$confounds`
#'   The confounds are organized hierarchically by block and preprocessing step.
#'   Each step contains metrics such as:
#'   - Blink rate and duration statistics
#'   - Gaze position (x,y) mean and standard deviation
#'   - Pupil size mean, standard deviation, and range
#'   - Missing data percentage
#'
#' @examples
#' # load demo dataset
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # calculate confounds for all blocks and preprocessing steps
#' confounds <- demo_data |>
#'   eyeris::glassbox() |>
#'   eyeris::epoch(
#'     events = "PROBE_{type}_{trial}",
#'     limits = c(-1, 1), # grab 1 second prior to and 1 second post event
#'     label = "prePostProbe" # custom epoch label name
#'   ) |>
#'   eyeris::summarize_confounds()
#'
#' # access confounds for entire time series for a specific block and step
#' confounds$confounds$unepoched_timeseries
#'
#' # access confounds for a specific epoched time series
#' # for a specific block and step
#' confounds$confounds$epoched_timeseries
#' confounds$confounds$epoched_epoch_wide
#'
#' @export
summarize_confounds <- function(eyeris) {
  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- summarize_confounds(eyeris$left)
    right_result <- summarize_confounds(eyeris$right)

    # return combined structure
    list_out <- list(
      left = left_result,
      right = right_result,
      original_file = eyeris$original_file,
      raw_binocular_object = eyeris$raw_binocular_object
    )

    class(list_out) <- "eyeris"

    return(list_out)
  }

  # regular eyeris object processing
  tryCatch(
    {
      check_data(eyeris, "summarize_confounds")
    },
    error = function(e) {
      error_handler(e, "input_data_type_error")
    }
  )

  available_blocks <- get_block_numbers(eyeris)

  if (is.null(available_blocks)) {
    log_error("eyeris no blocks found error.")
  }

  hz <- eyeris$info$sample.rate
  if (is.null(hz)) {
    log_error(
      "Sampling rate (Hz) must be present in tracker metadata (i.e., eyeris$info)"
    )
  }

  confounds_list <- list()

  for (block_id in available_blocks) {
    block_name <- paste0("block_", block_id)
    block_pupil_data <- eyeris$timeseries[[block_name]]
    pupil_steps <- grep("^pupil_", names(block_pupil_data), value = TRUE)
    confounds_list[[block_name]] <- list()

    for (step_name in pupil_steps) {
      stepwise_pupil <- block_pupil_data[[step_name]]
      if (is.null(stepwise_pupil)) {
        next
      }

      confounds_list[[block_name]][[step_name]] <- get_confounds_for_step(
        pupil_df = block_pupil_data,
        pupil_vec = stepwise_pupil,
        screen_width = eyeris$info$screen.x,
        screen_height = eyeris$info$screen.y,
        hz = hz
      )
    }
  }

  eyeris$confounds$unepoched_timeseries <- confounds_list

  epoch_names <- grep("^epoch_", names(eyeris), value = TRUE)

  if (length(epoch_names) > 0) {
    eyeris <- calculate_epoched_confounds(
      eyeris,
      epoch_names,
      hz,
      verbose = TRUE
    )
  }

  eyeris
}

#' Calculate confounds for a single pupil data step
#'
#' Computes various metrics from pupil data including:
#' - Blink detection
#' - Gaze on/off screen detection
#' - Gap analysis
#' - Gaze distance from screen center
#' - Gaze variance
#' - Blink rate
#' - Blink duration
#' - Blink time
#'
#' @param pupil_df A data frame containing pupil data
#' @param pupil_vec A vector of pupil data for the current step
#' @param screen_width The screen width in pixels
#' @param screen_height The screen height in pixels
#' @param hz The sampling rate in Hz
#'
#' @return A data frame containing confounds metrics for the current step
#'
#' @keywords internal
get_confounds_for_step <- function(
  pupil_df,
  pupil_vec,
  screen_width,
  screen_height,
  hz
) {
  if (!("is_blink" %in% names(pupil_df))) {
    pupil_df <- tag_blinks(pupil_df, pupil_vec)
  }

  if (!("is_offscreen" %in% names(pupil_df))) {
    pupil_df <- tag_gaze_coords(
      pupil_df = pupil_df,
      screen_width = screen_width,
      screen_height = screen_height
    )
  }

  total_time_ms <- (nrow(pupil_df) - 1) / hz * 1000
  is_invalid <- is.na(pupil_vec) | pupil_df$is_blink | pupil_df$is_offscreen
  gap_rle <- if (!any(is.na(is_invalid))) {
    rle(is_invalid)
  } else {
    list(lengths = 0, values = FALSE)
  }
  gap_lengths <- gap_rle$lengths[gap_rle$values]
  blink_rle <- if (!any(is.na(pupil_df$is_blink))) {
    rle(pupil_df$is_blink)
  } else {
    list(lengths = 0, values = FALSE)
  }
  blink_durs <- blink_rle$lengths[blink_rle$values] / hz * 1000
  total_blink_time <- sum(pupil_df$is_blink) / hz * 1000

  # center coordinate of screen
  cx <- screen_width / 2
  cy <- screen_height / 2

  data.frame(
    sampling_rate_hz = hz,
    total_time_ms = total_time_ms,
    n_samples = nrow(pupil_df),
    n_invalid = sum(is_invalid),
    prop_invalid = mean(is_invalid),
    n_gaps = length(gap_lengths),
    max_gap_n_samples = if (length(gap_lengths)) max(gap_lengths) else 0,
    max_gap_duration_ms = if (length(gap_lengths)) {
      max(gap_lengths) / hz * 1000
    } else {
      0
    },
    min_gap_n_samples = if (length(gap_lengths)) min(gap_lengths) else 0,
    min_gap_duration_ms = if (length(gap_lengths)) {
      min(gap_lengths) / hz * 1000
    } else {
      0
    },
    mean_gap_n_samples = if (length(gap_lengths)) mean(gap_lengths) else 0,
    mean_gap_duration_ms = if (length(gap_lengths)) {
      mean(gap_lengths) / hz * 1000
    } else {
      0
    },
    screen_width = screen_width,
    screen_height = screen_height,
    gaze_x_var_px = var(pupil_df$eye_x, na.rm = TRUE),
    gaze_y_var_px = var(pupil_df$eye_y, na.rm = TRUE),
    mean_gaze_distance_from_center_px = mean(
      calc_euclidean_dist(pupil_df$eye_x, pupil_df$eye_y, cx, cy),
      na.rm = TRUE
    ),
    mean_gaze_distance_from_center_norm = mean(
      pupil_df$gaze_dist_from_center,
      na.rm = TRUE
    ),
    prop_clipped = mean(pupil_vec %in% range(pupil_vec, na.rm = TRUE)),
    n_blinks = length(blink_durs),
    blink_rate_hz = length(blink_durs) / (total_time_ms / 1000),
    min_blink_duration_ms = if (length(blink_durs)) min(blink_durs) else NA,
    max_blink_duration_ms = if (length(blink_durs)) max(blink_durs) else NA,
    mean_blink_duration_ms = if (length(blink_durs)) mean(blink_durs) else NA,
    total_blink_time_ms = total_blink_time,
    prop_blink_time = total_blink_time / total_time_ms
  )
}

#' Tag blinks in pupil data
#'
#' Identifies when pupil data corresponds to eye blinks based on missing values
#' in the pupil vector.
#'
#' @param pupil_df A data frame containing pupil data
#' @param pupil_vec A numeric vector containing pupil diameter values
#'
#' @return A data frame with added column:
#'   - `is_blink`: Logical indicating if pupil data corresponds to a
#'     blink (NA values)
#'
#' @keywords internal
tag_blinks <- function(pupil_df, pupil_vec) {
  pupil_df$is_blink <- is.na(pupil_vec)
  pupil_df
}

#' Calculate Euclidean distance between points
#'
#' @param x1 First x coordinate or vector of x coordinates
#' @param y1 First y coordinate or vector of y coordinates
#' @param x2 Second x coordinate or vector of x coordinates (defaults to `0`)
#' @param y2 Second y coordinate or vector of y coordinates (defaults to `0`)
#'
#' @return A numeric vector of Euclidean distances
#'
#' @keywords internal
calc_euclidean_dist <- function(x1, y1, x2 = 0, y2 = 0) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

#' Normalize gaze coordinates to screen-relative units
#'
#' Transforms raw gaze coordinates (in pixels) to normalized coordinates where:
#' - (0,0) represents the center of the screen
#' - Coordinates are scaled to \[-1,1\] range
#' - Also calculates the normalized distance from screen center
#'
#' @param pupil_df A data frame containing raw gaze
#' coordinates (`eye_x`, `eye_y`)
#' @param screen_width The screen width in pixels
#' @param screen_height The screen height in pixels
#'
#' @return A data frame with added columns:
#'   - `eye_x_norm`: Normalized x coordinate \[-1,1\]
#'   - `eye_y_norm`: Normalized y coordinate \[-1,1\]
#'   - `gaze_dist_from_center`: Normalized distance from screen center
#'
#' @keywords internal
normalize_gaze_coords <- function(pupil_df, screen_width, screen_height) {
  center_x <- screen_width / 2
  center_y <- screen_height / 2
  pupil_df$eye_x_norm <- (pupil_df$eye_x - center_x) / center_x
  pupil_df$eye_y_norm <- (pupil_df$eye_y - center_y) / center_y
  pupil_df$gaze_dist_from_center <- calc_euclidean_dist(
    pupil_df$eye_x_norm,
    pupil_df$eye_y_norm
  )
  pupil_df
}

#' Tag gaze coordinates as on/off screen
#'
#' Identifies when gaze coordinates fall outside the screen boundaries, with an
#' optional buffer zone to account for potential overshoot in eye tracking.
#'
#' @param pupil_df A data frame containing gaze coordinates
#' @param screen_width The screen width in pixels
#' @param screen_height The screen height in pixels
#' @param overshoot_buffer Additional buffer zone beyond screen edges
#'   (default: `0.05`). Expressed as proportion of screen size.
#'   For example, `0.05` means 5% beyond screen edges will still be
#'   considered "on screen"
#'
#' @return A data frame with added column:
#'   - `is_offscreen`: Logical indicating if gaze is outside screen boundaries
#'
#' @keywords internal
tag_gaze_coords <- function(
  pupil_df,
  screen_width,
  screen_height,
  overshoot_buffer = 0.05
) {
  pupil_df <- normalize_gaze_coords(
    pupil_df = pupil_df,
    screen_width = screen_width,
    screen_height = screen_height
  )

  buffer <- 1 + overshoot_buffer
  pupil_df$is_offscreen <- abs(pupil_df$eye_x_norm) > buffer |
    abs(pupil_df$eye_y_norm) > buffer

  pupil_df
}

#' Export confounds data to CSV files and/or database
#'
#' Exports each block's confounds data to a separate CSV file and/or database table.
#' Each file will contain all pupil steps (e.g., pupil_raw, pupil_clean)
#' as rows, with confound metrics as columns.
#'
#' @param confounds_list A nested list structure containing confounds data
#' @param output_dir The directory where CSV files will be saved
#' @param filename_prefix Either a string prefix for filenames or a function
#' that takes a block name and returns a prefix
#' @param verbose A flag to indicate whether to print progress messages
#' @param run_num The run number (if NULL, will be extracted from block names)
#' @param csv_enabled Whether to write CSV files (default TRUE)
#' @param db_con Database connection object (NULL if database disabled)
#' @param sub Subject ID for database metadata
#' @param ses Session ID for database metadata
#' @param task Task name for database metadata
#' @param eye_suffix Eye suffix for binocular data (e.g., "eye-L", "eye-R")
#' @param epoch_label Epoch label for epoched data (added as column)
#'
#' @return Invisibly returns a vector of created file paths
#'
#' @keywords internal
export_confounds_to_csv <- function(
  confounds_list,
  output_dir,
  filename_prefix,
  verbose,
  run_num = NULL,
  csv_enabled = TRUE,
  db_con = NULL,
  sub = NULL,
  ses = NULL,
  task = NULL,
  eye_suffix = NULL,
  epoch_label = NULL
) {
  # handle binocular objects
  if (is_binocular_object(confounds_list)) {
    # process left and right eyes independently with appropriate suffixes
    left_files <- export_confounds_to_csv(
      confounds_list$left,
      file.path(output_dir, "left"),
      function(block_name) paste0(filename_prefix, "_eye-L_", block_name),
      verbose,
      run_num,
      csv_enabled,
      db_con,
      sub,
      ses,
      task,
      "eye-L",
      epoch_label
    )

    right_files <- export_confounds_to_csv(
      confounds_list$right,
      file.path(output_dir, "right"),
      function(block_name) paste0(filename_prefix, "_eye-R_", block_name),
      verbose,
      run_num,
      csv_enabled,
      db_con,
      sub,
      ses,
      task,
      "eye-R",
      epoch_label
    )

    # return combined file paths
    return(c(left_files, right_files))
  }

  # regular confounds list processing
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  created_files <- character()

  for (block_name in names(confounds_list)) {
    block_data <- confounds_list[[block_name]]
    if (length(block_data) == 0) {
      next
    }

    all_steps <- names(block_data)
    base_names <- sub("^pupil_", "", all_steps)

    step_rows <- list()

    for (i in seq_along(all_steps)) {
      full_step_name <- all_steps[i]
      step_data <- block_data[[full_step_name]]
      if (is.null(step_data)) {
        next
      }

      if (!is.data.frame(step_data)) {
        step_data <- as.data.frame(step_data)
      }

      # extract run number from block name if run_num is NULL
      actual_run_num <- run_num
      if (is.null(actual_run_num)) {
        # extract from block name (e.g., "block_1" -> "01")
        block_number <- gsub("block_", "", block_name)
        actual_run_num <- sprintf("%02d", as.numeric(block_number))
      }

      step_row <- data.frame(block = actual_run_num, step = full_step_name)

      # add epoch_label column if provided (for epoch data)
      if (!is.null(epoch_label)) {
        step_row$epoch_label <- epoch_label
      }

      step_row <- cbind(step_row, step_data)

      step_rows[[full_step_name]] <- step_row
    }

    result_df <- do.call(rbind, step_rows)

    prefix <- if (is.function(filename_prefix)) {
      filename_prefix(block_name)
    } else {
      filename_prefix
    }

    filename <- file.path(output_dir, paste0(prefix, ".csv"))

    success <- write_csv_and_db(
      data = result_df,
      csv_path = filename,
      csv_enabled = csv_enabled,
      db_con = db_con,
      data_type = "run_confounds",
      sub = sub,
      ses = ses,
      task = task,
      run = actual_run_num,
      eye_suffix = eye_suffix,
      verbose = verbose
    )

    if (success) {
      created_files <- c(created_files, filename)
    }
  }

  invisible(created_files)
}

#' Calculate confounds for epoched data
#'
#' Helper function to calculate confounds for epoched time series data.
#' This function is used internally by both `summarize_confounds()`
#' and `epoch()`.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param epoch_names A vector of epoch names to process
#' @param hz The sampling rate
#' @param verbose A flag to indicate whether to print progress messages
#'
#' @return An updated `eyeris` object with epoched confounds
#'
#' @keywords internal
calculate_epoched_confounds <- function(
  eyeris,
  epoch_names,
  hz,
  verbose = TRUE
) {
  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- calculate_epoched_confounds(
      eyeris$left,
      epoch_names,
      hz,
      verbose
    )
    right_result <- calculate_epoched_confounds(
      eyeris$right,
      epoch_names,
      hz,
      verbose
    )

    # return combined structure
    list_out <- list(
      left = left_result,
      right = right_result,
      original_file = eyeris$original_file
    )

    class(list_out) <- "eyeris"

    return(list_out)
  }

  # regular eyeris object processing
  eyeris$confounds$epoched_timeseries <- list()
  eyeris$confounds$epoched_epoch_wide <- list()

  for (epoch_name in epoch_names) {
    eyeris$confounds$epoched_timeseries[[epoch_name]] <- list()
    eyeris$confounds$epoched_epoch_wide[[epoch_name]] <- list()

    for (block_name in names(eyeris[[epoch_name]])) {
      if (block_name == "info") {
        next
      }

      epoch_data <- eyeris[[epoch_name]][[block_name]]

      if (
        is.null(epoch_data) ||
          !is.data.frame(epoch_data) ||
          nrow(epoch_data) == 0
      ) {
        next
      }

      if ("matched_event" %in% colnames(epoch_data)) {
        epoch_ids <- unique(epoch_data$matched_event)
      } else if ("start_matched_event" %in% colnames(epoch_data)) {
        epoch_ids <- unique(epoch_data$start_matched_event)
      } else {
        epoch_ids <- character(0)
      }

      pupil_steps <- grep("^pupil_", names(epoch_data), value = TRUE)

      epoch_wide_confounds <- list()
      step_specific_confounds <- list()

      for (id in epoch_ids) {
        if ("matched_event" %in% colnames(epoch_data)) {
          epoch_subset <- epoch_data[epoch_data$matched_event == id, ]
        } else if ("start_matched_event" %in% colnames(epoch_data)) {
          epoch_subset <- epoch_data[epoch_data$start_matched_event == id, ]
        } else {
          epoch_subset <- epoch_data
        }

        if (nrow(epoch_subset) == 0) {
          next
        }

        epoch_duration_ms <- (nrow(epoch_subset) - 1) / hz * 1000
        block_blinks <- eyeris$blinks[[block_name]]
        n_blinks_baseline <- NA
        if ("baseline_period" %in% names(epoch_subset)) {
          baseline_start <- min(epoch_subset$baseline_period)
          baseline_end <- max(epoch_subset$baseline_period)
          n_blinks_baseline <- sum(sapply(
            seq_len(nrow(block_blinks)),
            function(i) {
              max(block_blinks$stime[i], baseline_start) <=
                min(block_blinks$etime[i], baseline_end)
            }
          ))
        }
        first_blink_time <- NA
        epoch_start_time <- min(epoch_subset$time_orig)
        blinks_in_epoch <- block_blinks[
          block_blinks$stime >= epoch_start_time &
            block_blinks$stime <= max(epoch_subset$time_orig),
        ]
        if (nrow(blinks_in_epoch) > 0) {
          first_blink_time <- (min(blinks_in_epoch$stime) - epoch_start_time) /
            hz *
            1000
        }

        epoch_wide_confounds[[as.character(id)]] <- data.frame(
          matched_event = id,
          text_unique = if ("text_unique" %in% colnames(epoch_subset)) {
            unique(epoch_subset$text_unique)[1]
          } else {
            id
          },
          n_samples = nrow(epoch_subset),
          n_blinks_in_baseline = n_blinks_baseline,
          time_to_first_blink_ms = first_blink_time,
          epoch_duration_ms = epoch_duration_ms
        )

        step_confounds_list <- list()
        for (step_name in pupil_steps) {
          pupil_vec <- epoch_subset[[step_name]]

          if (all(is.na(pupil_vec))) {
            next
          }

          base_confounds <- get_confounds_for_step(
            pupil_df = epoch_subset,
            pupil_vec = pupil_vec,
            screen_width = eyeris$info$screen.x,
            screen_height = eyeris$info$screen.y,
            hz = hz
          )

          pre_epoch_window <- c(epoch_start_time - 200, epoch_start_time)
          pre_epoch_data <- eyeris$timeseries[[block_name]] |>
            dplyr::filter(
              time_orig >= pre_epoch_window[1] &
                time_orig <= pre_epoch_window[2]
            )

          step_df <- data.frame(
            matched_event = id,
            text_unique = if ("text_unique" %in% colnames(epoch_subset)) {
              unique(epoch_subset$text_unique)[1]
            } else {
              id
            },
            step = sub("pupil_", "", step_name),
            range = diff(range(pupil_vec, na.rm = TRUE)),
            zscore_max = max(scale(pupil_vec), na.rm = TRUE),
            zscore_min = min(scale(pupil_vec), na.rm = TRUE),
            prop_blink_time = base_confounds$prop_blink_time,
            pre_epoch_pupil_sd = sd(pre_epoch_data[[step_name]], na.rm = TRUE),
            epoch_pupil_sd = sd(pupil_vec, na.rm = TRUE)
          )

          step_confounds_list[[step_name]] <- step_df
        }

        if (length(step_confounds_list) > 0) {
          step_specific_confounds[[as.character(id)]] <- do.call(
            rbind,
            step_confounds_list
          )
        }
      }

      if (length(epoch_wide_confounds) > 0) {
        eyeris$confounds$epoched_epoch_wide[[epoch_name]][[
          block_name
        ]] <- do.call(rbind, epoch_wide_confounds)
      }

      if (length(step_specific_confounds) > 0) {
        eyeris$confounds$epoched_timeseries[[epoch_name]][[
          block_name
        ]] <- do.call(rbind, step_specific_confounds)
      }
    }
  }

  return(eyeris)
}
