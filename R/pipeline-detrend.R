#' Detrend the pupil time series
#'
#' Linearly detrend_pupil data by fitting a linear model of `pupil_data ~ time`,
#' and return the fitted betas and the residuals (`pupil_data - fitted_values`).
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Use `glassbox(detrend = TRUE)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' This function is automatically called by `glassbox()` if `detrend = TRUE`.
#'
#' Users should prefer using `glassbox()` rather than invoking this function
#' directly unless they have a specific reason to customize the pipeline
#' manually.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load_asc()].
#'
#' @return An `eyeris` object with two new columns in `timeseries`:
#' `detrend_fitted_betas`, and `pupil_raw_{...}_detrend`.
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
#'   plot(seed = 0)
#'
#' @export
detrend <- function(eyeris) {
  eyeris_out <- eyeris |>
    pipeline_handler(detrend_pupil, "detrend")

  eyeris_out$metadata$detrended <- TRUE

  eyeris_out
}

detrend_pupil <- function(x, prev_op) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_secs"]]

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
