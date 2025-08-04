#' Remove pupil samples that are physiologically unlikely
#'
#' @description
#' The intended use of this method is for removing pupil samples that emerge
#' more quickly than would be physiologically expected. This is accomplished by
#' rejecting samples that exceed a "speed"-based threshold (i.e., median
#' absolute deviation from sample-to-sample). This threshold is computed based
#' on the constant `n`, which defaults to the value `16`.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Provide parameters via
#' `detransient = list(...)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' This function is automatically called by `glassbox()` by default. If needed,
#' customize the parameters for `detransient` by providing a parameter list. Use
#' `glassbox(detransient = FALSE)` to disable this step as needed.
#'
#' Users should prefer using `glassbox()` rather than invoking this function
#' directly unless they have a specific reason to customize the pipeline
#' manually.
#'
#' **Computed properties:**
#'
#' - **`pupil_speed`:** Compute speed of pupil by approximating the derivative
#'    of `x` (pupil) with respect to `y` (time) using finite differences.
#'    - Let \eqn{x = (x_1, x_2, \dots, x_n)} and
#'      \eqn{y = (y_1, y_2, \dots, y_n)} be two numeric vectors with
#'      \eqn{n \ge 2}; then, the finite differences are computed as:
#'      \deqn{\delta_i = \frac{x_{i+1} - x_i}{y_{i+1} - y_i},
#'      \quad i = 1, 2, \dots, n-1.}
#'    - This produces an output vector \eqn{p = (p_1, p_2, \dots, p_n)}
#'      defined by:
#'      \itemize{
#'        \item For the first element:
#'          \deqn{p_1 = |\delta_1|,}
#'        \item For the last element:
#'          \deqn{p_n = |\delta_{n-1}|,}
#'        \item For the intermediate elements (\eqn{i = 2, 3, \dots, n-1}):
#'          \deqn{p_i = \max\{|\delta_{i-1}|,\,|\delta_i|\}.}
#'      }
#'
#' - **`median_speed`:** The median of the computed `pupil_speed`:
#'   \deqn{median\_speed = median(p)}
#'
#' - **`mad_val`:** The median absolute deviation (MAD) of `pupil_speed`
#'   from the median:
#'   \deqn{mad\_val = median(|p - median\_speed|)}
#'
#' - **`mad_thresh`:** A threshold computed from the median speed and the MAD,
#'   using a constant multiplier \eqn{n} (default value: 16):
#'   \deqn{mad\_thresh = median\_speed + (n \times mad\_val)}
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param n A constant used to compute the median absolute deviation (MAD)
#' threshold. Defaults to `16`
#' @param mad_thresh Default `NULL`. This parameter provides
#' alternative options for handling edge cases where the computed
#' properties here within [eyeris::detransient()]  \eqn{mad\_val}
#' and \eqn{median\_speed} are very small. For example, if
#' \deqn{mad\_val = 0 \quad \text{and} \quad median\_speed = 1,}
#' then, with the default multiplier \eqn{n = 16},
#' \deqn{mad\_thresh = median\_speed +
#' (n \times mad\_val) = 1 + (16 \times 0) = 1.}
#' In this situation, any speed \eqn{p_i \ge 1} would be flagged as a
#' transient, which might be overly sensitive. To reduce this sensitivity,
#' two possible adjustments are available:
#'
#' 1. If \eqn{mad\_thresh = 1}, the transient detection criterion is
#'    modified from
#'    \deqn{p_i \ge mad\_thresh}
#'    to
#'    \deqn{p_i > mad\_thresh .}
#'
#' 2. If \eqn{mad\_thresh} is very small, the user may manually
#'    adjust the sensitivity by supplying an alternative threshold value
#'    here directly via this `mad_thresh` parameter.
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call. Defaults to `NULL`
#'
#' @return An `eyeris` object with a new column in `time series`:
#' `pupil_raw_{...}_detransient`
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full `eyeris` glassbox preprocessing pipeline.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   eyeris::glassbox(
#'     detransient = list(n = 16) # set to FALSE to skip step (not recommended)
#'   ) |>
#'   plot(seed = 0)
#'
#' @export
detransient <- function(eyeris, n = 16, mad_thresh = NULL, call_info = NULL) {
  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(n = n, mad_thresh = mad_thresh)
    )
  } else {
    call_info
  }

  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- eyeris$left |>
      pipeline_handler(
        detransient_pupil,
        "detransient",
        n,
        mad_thresh,
        call_info = call_info
      )

    right_result <- eyeris$right |>
      pipeline_handler(
        detransient_pupil,
        "detransient",
        n,
        mad_thresh,
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
        detransient_pupil,
        "detransient",
        n,
        mad_thresh,
        call_info = call_info
      )
  }
}

#' Internal function to remove transient artifacts from pupil data
#'
#' @description This function implements transient artifact removal by
#' identifying and removing samples that exceed a speed-based threshold.
#' The threshold is computed based on the constant `n`, which defaults to
#' the value `16`.
#'
#' This function is called by the exposed wrapper [eyeris::detransient()].
#'
#' @details The function works by:
#' \itemize{
#'   \item Calculating the speed of pupil changes using finite differences
#'   \item Identifying samples that exceed a speed-based threshold
#'   \item Removing these samples from the pupil data
#' }
#'
#' @param x A data frame containing pupil data with columns `time_secs` and
#'   the previous operation's pupil column
#' @param prev_op The name of the previous operation's pupil column
#' @param n The constant used to compute the median absolute deviation (MAD)
#'   threshold. Defaults to `16`
#' @param mad_thresh The threshold used to identify transient artifacts.
#'   Defaults to `NULL`
#'
#' @return A numeric vector of the same length as the input data with transient
#'   artifacts removed (set to NA)
#'
#' @keywords internal
detransient_pupil <- function(x, prev_op, n, mad_thresh) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_secs"]]

  # note: `pupil_speed` is calculated using the helper function below
  pupil_speed <- speed(pupil, timeseries)

  if (!is.null(mad_thresh)) {
    using_mad_thresh_override <- TRUE
  } else {
    using_mad_thresh_override <- FALSE
  }

  if (!using_mad_thresh_override) {
    median_speed <- median(pupil_speed, na.rm = TRUE)
    mad_val <- median(abs(pupil_speed - median_speed), na.rm = TRUE)
    mad_thresh <- median_speed + (n * mad_val)
  } else if (is.numeric(mad_thresh)) {
    log_warn("Using user supplied `mad_thresh`... skipping calculation")
    mad_val <- 0
  } else {
    log_error("`mad_thresh` must either be `NULL` or numeric.")
  }

  # validate `mad_val` != 0: unrealistic outcome for real data
  # likely means filtering has already been applied to the data
  # (i.e., if online filtering is enabled on the EyeLink Host PC)
  if (!using_mad_thresh_override && !is.na(mad_val) && mad_val == 0) {
    warning(paste(
      "\n ***WARNING: SOMETHING OUTRAGEOUS IS HAPPENING WITH YOUR PUPIL",
      "DATA!***",
      "\n\nThe median absolute deviation (MAD) of your pupil speed is 0.\n",
      "This indicates a potential setup / hardware issue which has likely",
      "propogated into your pupil recording.\n",
      "Most often, this is due to online filtering being applied directly",
      "to your pupil data via the EyeLink Host PC.\n\n",
      "WE DO NOT RECOMMEND USING ANY ONLINE AUTO FILTERING",
      "DURING YOUR DATA RECORDING.\n\n",
      "***ADDITIONAL INFO***\nThe default setting for communicating with the",
      "EyeLink\n",
      "machine is to have filtering enabled on either live-streamed data or",
      "saved data in EDF files.\n\n We do not want these filters because:\n",
      "(1)",
      "it is unclear how the EyeLink machine is specifying their low-pass",
      "filter,",
      "and\n (2) it conflicts with an assumption here in",
      "`eyeris::detransient()`",
      "to use one-step \ndifference to compute a threshold for detecting",
      "large",
      "jumps in pupil data\n (suggesting blinking or other artifacts).\n\n",
      "***GENERAL TIP***\n You should aim to have your data as raw as",
      "possible so you",
      "can 'cook' it fresh!\n\n",
      "***WHAT TO DO NEXT***\n If you would like to continue preprocessing",
      "your current",
      "pupil data file,\n you should consider skipping the",
      "`eyeris::detransient()`",
      "step of the pipeline altogether.\n Advanced users might consider",
      "additional",
      "testing by manually passing in different `mad_thresh` override",
      "values\n",
      "into `eyeris::detransient()` and then carefully assessing the",
      "consequences",
      "of any given override value on the pupil data;\n however, this step",
      "is not recommended for most users.\n\n PLEASE CONTINUE AT YOUR OWN",
      "RISK.\n\n\n"
    ))

    log_error("Computed property `mad_val` == 0!")
  }

  # handle case where mad_val is NA (all pupil data is NA)
  if (!using_mad_thresh_override && is.na(mad_val)) {
    # if all pupil data is NA, return the original pupil data unchanged
    return(pupil)
  }

  if (mad_thresh == 1) {
    comparison <- pupil_speed > mad_thresh
  } else {
    comparison <- pupil_speed >= mad_thresh
  }

  ifelse(comparison, as.numeric(NA), pupil)
}

#' Calculate pupil speed using finite differences
#'
#' Computes the speed of pupil changes using finite differences between
#' consecutive time points. This is a helper function for the detransient step.
#'
#' @param x A numeric vector of pupil data
#' @param y A numeric vector of time data
#'
#' @return A vector of pupil speeds at each time point
#'
#' @keywords internal
speed <- function(x, y) {
  delta <- diff(x) / diff(y)
  pupil <- abs(cbind(c(NA, delta), c(delta, NA))) # matrix of differences
  pupil <- apply(pupil, 1, function(row) {
    if (all(is.na(row))) {
      NA # return NA for all-NA rows
    } else {
      max(row, na.rm = TRUE) # only compute max for valid rows
    }
  })
  pupil
}
