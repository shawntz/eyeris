#' Interpolate missing pupil samples
#'
#' Linear interpolation of time series data. The intended use of this method
#' is for filling in missing pupil samples (NAs) in the time series. This method
#' uses "na.approx()" function from the zoo package, which implements linear
#' interpolation using the "approx()" function from the stats package.
#' Currently, NAs at the beginning and the end of the data are replaced with
#' values on either end, respectively, using the "rule = 2" argument in the
#' `approx()` function.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Use `glassbox(interpolate = TRUE)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' This function is automatically called by `glassbox()` by default. Use
#' `glassbox(interpolate = FALSE)` to disable this step as needed.
#'
#' Users should prefer using `glassbox()` rather than invoking this function
#' directly unless they have a specific reason to customize the pipeline
#' manually.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`. Set to `FALSE` to suppress messages about the current
#' processing step and run silently
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_raw_{...}_interpolate`
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   # set to FALSE to skip (not recommended)
#'   eyeris::glassbox(interpolate = TRUE) |>
#'   plot(seed = 0)
#'
#' @export
interpolate <- function(eyeris, verbose = TRUE, call_info = NULL) {
  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(verbose = verbose)
    )
  } else {
    call_info
  }

  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- eyeris$left |>
      pipeline_handler(
        interpolate_pupil,
        "interpolate",
        verbose,
        call_info = call_info
      )

    right_result <- eyeris$right |>
      pipeline_handler(
        interpolate_pupil,
        "interpolate",
        verbose,
        call_info = call_info
      )

    # return combined structure
    list_out <- list(
      left = left_result,
      right = right_result,
      original_file = eyeris$original_file,
      raw_binocular_object = eyeris$raw_binocular_object
    )

    class(list_out) <- "eyeris"

    return(list_out)
  } else {
    # regular eyeris object, process normally
    eyeris |>
      pipeline_handler(
        interpolate_pupil,
        "interpolate",
        verbose,
        call_info = call_info
      )
  }
}

#' Interpolate missing pupil data using linear interpolation
#'
#' This function fills missing values (NAs) in pupil data using linear
#' interpolation. It uses the `zoo::na.approx()` function with settings
#' optimized for pupillometry data.
#'
#' This function is called by the exposed wrapper [eyeris::interpolate()].
#'
#' @param x A data frame containing the pupil time series data
#' @param prev_op The name of the previous operation's output column
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return A vector of interpolated pupil values with the same length as the
#' input
#'
#' @keywords internal
interpolate_pupil <- function(x, prev_op, verbose) {
  if (!any(is.na(x[[prev_op]]))) {
    if (verbose) {
      cli::cli_alert_warning(
        "[WARN] No NAs detected in pupil data... Skipping interpolation!"
      )
    }
    return(x[[prev_op]])
  } else {
    prev_pupil <- x[[prev_op]]
  }

  zoo::na.approx(
    prev_pupil,
    na.rm = FALSE,
    maxgap = Inf,
    rule = 2
  )
}
