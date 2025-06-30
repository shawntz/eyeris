#' Bin pupil time series by averaging within time bins
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
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param bins_per_second The number of bins to create per second of data
#' @param method The binning method: "mean" (default) or "median"
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call. Defaults to `NULL`
#'
#' @return An `eyeris` object with binned data and updated sampling rate
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline
#' [eyeris::downsample()] for downsampling functionality
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # bin data into 10 bins per second using the (default) "mean" method
#' demo_data |>
#'   eyeris::glassbox(bin = list(bins_per_second = 10, method = "mean")) |>
#'   plot(seed = 0)
#'
#' @export
bin <- function(eyeris, bins_per_second, method = "mean", call_info = NULL) {
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

  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(bins_per_second = bins_per_second, method = method)
    )
  } else {
    call_info
  }

  eyeris |>
    pipeline_handler(
      bin_pupil,
      "bin",
      bins_per_second,
      method,
      current_fs,
      call_info = call_info
    )
}

#' Bin pupil data into specified time bins
#'
#' This function bins pupil data into specified time bins using either mean or
#' median aggregation. It creates evenly spaced bins across the time series and
#' aggregates pupil values within each bin.
#'
#' This function is called by the exposed wrapper [eyeris::bin()].
#'
#' @param x A data frame containing the pupil time series data
#' @param prev_op The name of the previous operation's output column
#' @param bins_per_second Number of bins per second (positive integer)
#' @param method Aggregation method: "mean" or "median"
#' @param current_fs Current sampling rate in Hz
#'
#' @return A data frame with binned pupil data containing columns:
#'   - `time_secs`: Bin center timestamps
#'   - `pupil_binned_{method}_{bins_per_second}hz`: Binned pupil values
#'
#' @keywords internal
bin_pupil <- function(x, prev_op, bins_per_second, method, current_fs) {
  # debug: check if prev_op is empty or NULL
  if (is.null(prev_op) || length(prev_op) == 0 || prev_op == "") {
    cli::cli_abort(paste(
      "Previous operation column name is empty or NULL.",
      "Expected a valid column name like 'pupil_raw'. This usually means the",
      "eyeris object's 'latest' pointer is not set correctly.",
      "Current prev_op value:", deparse(prev_op)
    ))
  }

  # debug: check if the column exists
  if (!prev_op %in% colnames(x)) {
    cli::cli_abort(paste(
      "Column '", prev_op, "' not found in eyeris data object.",
      "Available columns:", paste(colnames(x), collapse = ", ")
    ))
  }

  if (any(is.na(x[[prev_op]]))) {
    cli::cli_abort("NAs detected in pupil data. Need to interpolate first.")
  } else {
    prev_pupil <- x[[prev_op]]
  }

  time_col <- "time_secs"
  time_secs_inferred <- x[[time_col]]

  # validate that time series is monotonically increasing
  check_time_monotonic(time_secs_inferred, time_col)

  # create bin centers (1/2X, 3/2X, 5/2X, ...)
  # anchored to the start of the time vector
  bin_duration <- 1 / bins_per_second
  min_time <- min(time_secs_inferred, na.rm = TRUE)
  max_time <- max(time_secs_inferred, na.rm = TRUE)
  bin_centers <- seq(min_time + bin_duration / 2, max_time, by = bin_duration)

  # pre-compute bin assignments for all time points
  bin_assignments <- findInterval(
    time_secs_inferred, bin_centers - bin_duration / 2
  )

  binned_df <- data.frame(
    time_secs = bin_centers,
    stringsAsFactors = FALSE
  )

  # Helper function to bin a vector according to pre-computed bin
  # assignments and bin centers using either mean or median aggregation.
  #
  # Args:
  #   vec: The vector to bin
  #   bin_assignments: Vector indicating which bin each element belongs to
  #   bin_centers: Vector of bin center values
  #   method: The aggregation method: "mean" or "median"
  #
  # Returns: A vector of binned values with length equal to bin_centers
  bin_vector <- function(vec, bin_assignments, bin_centers, method) {
    result <- numeric(length(bin_centers))
    for (i in seq_along(bin_centers)) {
      bin_indices <- which(bin_assignments == i)
      if (length(bin_indices) > 0) {
        if (method == "mean") {
          result[i] <- mean(vec[bin_indices], na.rm = TRUE)
        } else {
          result[i] <- median(vec[bin_indices], na.rm = TRUE)
        }
      } else {
        result[i] <- NA
      }
    }
    result
  }

  binned_bin_col <- bin_vector(prev_pupil, bin_assignments, bin_centers, method)
  binned_df <- cbind(
    binned_df,
    setNames(
      list(binned_bin_col), paste0(prev_op, "_bin")
    )
  )

  # process all remaining cols from the orig df
  cols_to_process <- 0
  for (col in names(x)) {
    if (col != prev_op && col != time_col && !grepl("_bin$", col)) {
      cols_to_process <- cols_to_process + 1
    }
  }

  if (cols_to_process > 0) {
    pb <- counter_bar(cols_to_process, msg = "Binning columns", width = 70)
  } else {
    pb <- NULL
  }

  for (col in names(x)) {
    if (col != prev_op && col != time_col && !grepl("_bin$", col)) {
      if (is.numeric(x[[col]])) {
        binned_df[[col]] <- bin_vector(
          x[[col]],
          bin_assignments,
          bin_centers,
          method
        )
      } else {
        binned_df[[col]] <- sapply(seq_along(bin_centers), function(i) {
          bin_indices <- which(bin_assignments == i)
          if (length(bin_indices) > 0) {
            x[[col]][bin_indices[1]]
          } else {
            NA
          }
        })
      }
      tick(pb, by = 1)
    }
  }

  list_out <- list(
    downsampled_df = binned_df,
    decimated.sample.rate = bins_per_second
  )
}
