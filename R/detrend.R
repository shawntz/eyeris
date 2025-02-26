#' Detrend the pupil time series
#'
#' Linearly detrend_pupil data by fitting a linear model of `pupil_data ~ time`,
#' and return the fitted betas and the residuals (`pupil_data - fitted_values`).
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#'
#' @return An `eyeris` object with two new columns in `timeseries`:
#' `detrend_fitted_betas`, and `pupil_raw_{...}_detrend`.
#'
#' @examples
#' system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::detrend() |>
#'   plot(seed = 0)
#'
#' @export
detrend <- function(eyeris) {
  eyeris |>
    pipeline_handler(detrend_pupil, "detrend")
}

detrend_pupil <- function(x, prev_op) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_orig"]]

  fit <- lm(pupil ~ timeseries)

  fitted_values <- fit$fitted.values
  coefficients <- fit$coefficients
  residuals <- fit$residuals

  list(
    fitted_values = fitted_values,
    coefficients = coefficients,
    residuals = residuals
  )
}
