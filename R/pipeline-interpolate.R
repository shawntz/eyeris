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
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`. Set to `FALSE` to suppress messages about the current
#' processing step and run silently.
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_raw_{...}_interpolate`.
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
interpolate <- function(eyeris, verbose = TRUE) {
  eyeris |>
    pipeline_handler(interpolate_pupil, "interpolate", verbose)
}

interpolate_pupil <- function(x, prev_op, verbose) {
  if (!any(is.na(x[[prev_op]]))) {
    if (verbose) {
      cli::cli_alert_warning(
        "[ INFO ] - No NAs detected in pupil data... Skipping interpolation!"
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
