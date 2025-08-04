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
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call. Defaults to `NULL`
#'
#' @return An `eyeris` object with two new columns in `time series`:
#' `detrend_fitted_betas`, and `pupil_raw_{...}_detrend`
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full `eyeris` glassbox preprocessing pipeline
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
#'   plot(seed = 0)
#'
#' @export
detrend <- function(eyeris, call_info = NULL) {
  call_info <- if (is.null(call_info)) {
    list(call_stack = match.call(), parameters = list())
  } else {
    call_info
  }

  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- eyeris$left |>
      pipeline_handler(detrend_pupil, "detrend", call_info = call_info)

    left_result$metadata$detrended <- TRUE

    right_result <- eyeris$right |>
      pipeline_handler(detrend_pupil, "detrend", call_info = call_info)

    right_result$metadata$detrended <- TRUE

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
    eyeris_out <- eyeris |>
      pipeline_handler(detrend_pupil, "detrend", call_info = call_info)

    eyeris_out$metadata$detrended <- TRUE

    eyeris_out
  }
}

#' Internal function to detrend pupil data
#'
#' @description This function detrends pupil data by fitting a linear model
#' of `pupil_data ~ time`, and returning the fitted betas and the residuals
#' (`pupil_data - fitted_values`).
#'
#' This function is called by the exposed wrapper [eyeris::detrend()].
#'
#' @param x A data frame containing pupil data with columns `time_secs` and
#'   the previous operation's pupil column
#' @param prev_op The name of the previous operation's pupil column
#'
#' @return A list containing the fitted values, coefficients, and residuals
#'
#' @keywords internal
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
