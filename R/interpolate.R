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
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_raw_{...}_interpolate`.
#'
#' @examples
#' system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   plot(seed = 0)
#'
#' @export
interpolate <- function(eyeris) {
  eyeris |>
    pipeline_handler(interpolate_pupil, "interpolate")
}

interpolate_pupil <- function(x, prev_op) {
  if (!any(is.na(x[[prev_op]]))) {
    cli::cli_alert_warning(
      "[ INFO ] - No NAs detected in pupil data for interpolation... Skipping!"
    )
    return(x[[prev_op]])
  } else {
    prev_pupil <- x[[prev_op]]
  }

  interp_pupil <- zoo::na.approx(
    prev_pupil,
    na.rm = FALSE,
    maxgap = Inf,
    rule = 2
  )

  return(interp_pupil)
}
