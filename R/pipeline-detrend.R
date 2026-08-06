#' Detrend the pupil time series
#'
#' Detrend pupil data by fitting a model of `pupil_data ~ time` and returning
#' the fitted values (the estimated trend) together with the residuals
#' (`pupil_data - fitted_values`). Two trend models are supported via the
#' `method` argument: `"linear"` (the default) removes a straight-line drift by
#' fitting an ordinary linear model, while `"spline"` removes a smooth,
#' potentially nonlinear drift by fitting a natural cubic spline basis of `time`
#' (`splines::ns(time, df = spline_df)`).
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Use `glassbox(detrend = TRUE)` for
#' linear detrending, or `glassbox(detrend = list(method = "spline"))` for
#' spline detrending.
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
#' @param method A string indicating the detrending model to fit. Either
#' `"linear"` (the default) to remove a straight-line trend by regressing pupil
#' size on time, or `"spline"` to remove a smooth, potentially nonlinear trend
#' by fitting a natural cubic spline basis of time
#' (`splines::ns(time, df = spline_df)`)
#' @param spline_df The degrees of freedom for the natural cubic spline basis
#' used when `method = "spline"`. Higher values allow the fitted trend to follow
#' more rapid, nonlinear drift; lower values enforce a smoother trend. Must be a
#' single whole number `>= 1`. Defaults to `5`. Ignored when `method =
#' "linear"`
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call. Defaults to `NULL`
#'
#' @return An `eyeris` object with two new columns in `time series`:
#' `detrend_fitted_values`, and `pupil_raw_{...}_detrend`
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full `eyeris` glassbox preprocessing pipeline
#'
#' For a complete, end-to-end reference pipeline that demonstrates how all
#' `eyeris` preprocessing functions are chained together in practice, see the
#' "Building Blocks Under the Hood" section of the *Anatomy of an `eyeris`
#' Object* vignette --- \code{vignette("anatomy", package = "eyeris")} --- as
#' well as the *Complete Pupillometry Pipeline Walkthrough* vignette:
#' \code{vignette("complete-pipeline", package = "eyeris")}.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # (a) linear detrending (the default)
#' demo_data |>
#'   eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
#'   # a wider `preview_window` makes the (slow) fitted trend easier to see
#'   plot(seed = 0, preview_window = c(5, 20))
#'
#' # (b) spline detrending (removes a smooth, nonlinear trend)
#' demo_data |>
#'   eyeris::glassbox(detrend = list(method = "spline", spline_df = 5)) |>
#'   plot(seed = 0, preview_window = c(5, 20))
#'
#' @export
detrend <- function(
  eyeris,
  method = c("linear", "spline"),
  spline_df = 5,
  call_info = NULL
) {
  method <- match.arg(method)

  # only the spline model consumes spline_df, so only validate it there
  if (identical(method, "spline")) {
    spline_df <- validate_spline_df(spline_df)
  }

  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(method = method, spline_df = spline_df)
    )
  } else {
    call_info
  }

  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- eyeris$left |>
      pipeline_handler(
        detrend_pupil,
        "detrend",
        method = method,
        spline_df = spline_df,
        call_info = call_info
      )

    left_result$metadata$detrended <- TRUE

    right_result <- eyeris$right |>
      pipeline_handler(
        detrend_pupil,
        "detrend",
        method = method,
        spline_df = spline_df,
        call_info = call_info
      )

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
      pipeline_handler(
        detrend_pupil,
        "detrend",
        method = method,
        spline_df = spline_df,
        call_info = call_info
      )

    eyeris_out$metadata$detrended <- TRUE

    eyeris_out
  }
}

#' Internal function to detrend pupil data
#'
#' @description This function detrends pupil data by fitting a model of
#' `pupil_data ~ time` and returning the fitted values (the estimated trend)
#' together with the residuals (`pupil_data - fitted_values`).
#'
#' When `method = "linear"` (the default) the trend is a straight line fit with
#' `lm(pupil ~ time)`. When `method = "spline"` the trend is a natural cubic
#' spline of time fit with `lm(pupil ~ splines::ns(time, df = spline_df))`,
#' allowing a smooth nonlinear drift to be removed.
#'
#' This function is called by the exposed wrapper [eyeris::detrend()].
#'
#' @param x A data frame containing pupil data with columns `time_secs` and
#'   the previous operation's pupil column
#' @param prev_op The name of the previous operation's pupil column
#' @param method A string indicating the detrending model to fit, either
#'   `"linear"` (the default) or `"spline"`
#' @param spline_df The degrees of freedom for the natural cubic spline basis
#'   used when `method = "spline"`. Defaults to `5`
#'
#' @return A list containing the fitted values, coefficients, and residuals
#'
#' @keywords internal
detrend_pupil <- function(x, prev_op, method = "linear", spline_df = 5) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_secs"]]

  method <- match.arg(method, c("linear", "spline"))

  # use na.exclude so intentional missing-data gaps left by
  # interpolate(max_gap_ms) are excluded from the fit but preserved (as NA) in
  # the returned fitted values and residuals, keeping output length aligned with
  # the input. With no NAs this is identical to the default fit.
  fit <- if (identical(method, "spline")) {
    # a natural cubic spline of time removes a smooth, potentially nonlinear
    # trend (vs. the straight-line trend removed by the linear model)
    lm(
      pupil ~ splines::ns(timeseries, df = spline_df),
      na.action = stats::na.exclude
    )
  } else {
    lm(pupil ~ timeseries, na.action = stats::na.exclude)
  }

  list(
    fitted_values = stats::fitted(fit),
    coefficients = stats::coef(fit),
    residuals = stats::residuals(fit)
  )
}

#' Validate the `spline_df` argument for spline detrending
#'
#' Ensures `spline_df` (the degrees of freedom for the natural cubic spline
#' basis used by [eyeris::detrend()] when `method = "spline"`) is a single whole
#' number `>= 1`, aborting with an informative error otherwise.
#'
#' @param spline_df The candidate degrees-of-freedom value to validate
#'
#' @return The validated value coerced to an integer
#'
#' @keywords internal
validate_spline_df <- function(spline_df) {
  if (
    !is.numeric(spline_df) ||
      length(spline_df) != 1 ||
      is.na(spline_df) ||
      spline_df < 1 ||
      spline_df != round(spline_df)
  ) {
    log_error(paste0(
      "`spline_df` must be a single whole number >= 1 (the degrees of ",
      "freedom for the natural cubic spline used in spline detrending)."
    ))
  }

  as.integer(spline_df)
}
