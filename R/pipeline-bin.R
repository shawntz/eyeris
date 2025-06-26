#' Bin time series data by averaging within time bins
#'
#' This function bins pupillometry data by dividing time into equal intervals
#' and averaging the data within each bin. Unlike downsampling, binning
#' averages data points within each time bin.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Provide parameters via
#' `bin = list(...)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' Binning divides one second of pupillary data into X bins and averages
#' pupillometry data around each bin center. The resulting time points will be:
#' 1/2X, 3/2X, 5/2X, ..., etc. where X is the number of bins per second.
#'
#' This approach is commonly used in pupillometry research to study temporal
#' dynamics of pupil dilatory response; however, it should be used with caution
#' (as averaging within bins can distort the pupillary dynamics).
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()].
#' @param bins_per_second The number of bins to create per second of data.
#' @param method The binning method: "mean" (default) or "median".
#'
#' @return An `eyeris` object with binned data and updated sampling rate.
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline.
#' [eyeris::downsample()] for downsampling functionality.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # bin data into 10 bins per second using the (default) mean method
#' demo_data |>
#'   eyeris::glassbox(bin = list(bins_per_second = 10)) |>
#'   plot(seed = 0)
#'
#' @export
bin <- function(eyeris, bins_per_second, method = "mean") {
  if (!method %in% c("mean", "median")) {
    cli::cli_abort("Method must be either 'mean' or 'median'")
  }

  if (bins_per_second <= 0 ||
        !is.numeric(bins_per_second) ||
        bins_per_second != round(bins_per_second)) {
    cli::cli_abort("bins_per_second must be a positive integer")
  }

  current_fs <- eyeris$info$sample.rate
  new_fs <- bins_per_second

  eyeris <- eyeris |>
    pipeline_handler(
      bin_pupil,
      "bin",
      bins_per_second,
      method,
      current_fs
    )

  eyeris$info$binning.sample.rate <- new_fs

  return(eyeris)
}

bin_pupil <- function(x, prev_op, bins_per_second, method, current_fs) {
  if (any(is.na(x[[prev_op]]))) {
    cli::cli_abort("NAs detected in pupil data. Need to interpolate first.")
  } else {
    prev_pupil <- x[[prev_op]]
  }

  time_col <- NULL
  for (col in names(x)) {
    if (col != prev_op && is.numeric(x[[col]]) &&
          any(grepl("time", tolower(col)))) {
      time_col <- col
      break
    }
  }

  # if no time column found, create one based on sample indices
  # though, this should never happen, hopefully
  if (is.null(time_col)) {
    time_secs_inferred <- (1:seq_along(prev_pupil) - 1) / current_fs
  } else {
    time_secs_inferred <- x[[time_col]]
  }

  # create bin centers (1/2X, 3/2X, 5/2X, ...)
  bin_duration <- 1 / bins_per_second
  max_time <- max(time_secs_inferred, na.rm = TRUE)
  bin_centers <- seq(bin_duration / 2, max_time, by = bin_duration)
  binned_pupil <- numeric(length(bin_centers))

  for (i in seq_along(bin_centers)) {
    center <- bin_centers[i]
    bin_start <- center - bin_duration / 2
    bin_end <- center + bin_duration / 2
    bin_indices <- which(
      time_secs_inferred >= bin_start & time_secs_inferred < bin_end
    )

    if (length(bin_indices) > 0) {
      if (method == "mean") {
        binned_pupil[i] <- mean(prev_pupil[bin_indices], na.rm = TRUE)
      } else {
        binned_pupil[i] <- median(prev_pupil[bin_indices], na.rm = TRUE)
      }
    } else {
      binned_pupil[i] <- NA
    }
  }

  binned_df <- data.frame(
    time_secs = bin_centers,
    stringsAsFactors = FALSE
  )

  binned_df[[paste0(prev_op, "_bin")]] <- binned_pupil

  for (col in names(x)) {
    if (col != prev_op && col != time_col && is.numeric(x[[col]]) &&
          !grepl("_bin$", col)) {
      binned_values <- numeric(length(bin_centers))

      for (i in seq_along(bin_centers)) {
        center <- bin_centers[i]
        bin_start <- center - bin_duration / 2
        bin_end <- center + bin_duration / 2

        bin_indices <- which(
          time_secs_inferred >= bin_start & time_secs_inferred < bin_end
        )

        if (length(bin_indices) > 0) {
          if (method == "mean") {
            binned_values[i] <- mean(x[[col]][bin_indices], na.rm = TRUE)
          } else {
            binned_values[i] <- median(x[[col]][bin_indices], na.rm = TRUE)
          }
        } else {
          binned_values[i] <- NA
        }
      }

      binned_df[[col]] <- binned_values
    }
  }

  binned_df
}
