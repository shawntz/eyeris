#' Remove pupil samples that are physiologically unlikely
#'
#' @description
#' The intended use of this method is for removing pupil samples that emerge
#' more quickly than would be physiologically expected. This is accomplished by
#' rejecting samples that exceed a "speed"-based threshold (i.e., median
#' absolute deviation from sample-to-sample). This threshold is computed based
#' on the constant `n`, which defaults to the value `16`.
#'
#' @details
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
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param n A constant used to compute the median absolute deviation (MAD)
#' threshold.
#'
#' @param mad_override Default `NULL`. This parameter provides
#' alternative options for handling edge cases where the computed
#' properties here within [eyeris::detransient()]  \eqn{\text{mad\_val}}
#' and \eqn{\text{median\_speed}} are very small. For example, if
#' \deqn{\text{mad\_val} = 0 \quad \text{and} \quad \text{median\_speed} = 1,}
#' then, with the default multiplier \eqn{n = 16},
#' \deqn{\text{mad\_thresh} = \text{median\_speed} +
#' (n \times \text{mad\_val}) = 1 + (16 \times 0) = 1.}
#' In this situation, any speed \eqn{p_i \ge 1} would be flagged as a
#' transient, which might be overly sensitive. To reduce this sensitivity,
#' two possible adjustments are available:
#'
#' 1. If \eqn{\text{mad\_thresh} = 1}, the transient detection criterion is
#'    modified from
#'    \deqn{p_i \ge \text{mad\_thresh}}
#'    to
#'    \deqn{p_i > \text{mad\_thresh}.}
#'
#' 2. If \eqn{\text{mad\_thresh}} is very small, the user may manually
#'    adjust the sensitivity by providing an alternative threshold value
#'    via the `mad_override` parameter.
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_raw_{...}_detransient`.
#'
#' @examples
#' system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   plot(seed = 0)
#'
#' @export
detransient <- function(eyeris, n = 16, mad_override = NULL) {
  eyeris |>
    pipeline_handler(detransient_pupil, "detransient", n, mad_override)
}

# adapted from:
# https://github.com/dr-JT/pupillometry/blob/main/R/pupil_artifact.R
detransient_pupil <- function(x, prev_op, n, mad_override) {
  pupil <- x[[prev_op]]
  timeseries <- x[["time_orig"]]

  # note: `pupil_speed` calculated with the helper function below
  pupil_speed <- speed(pupil, timeseries)

  median_speed <- median(pupil_speed, na.rm = TRUE)
  mad_val <- median(abs(pupil_speed - median_speed), na.rm = TRUE)
  mad_thresh <- median_speed + (n * mad_val)

  if (is.null(mad_override)) {
    if (mad_thresh == 1) {
      comparison <- pupil_speed > mad_thresh
    } else {
      comparison <- pupil_speed >= mad_thresh
    }
  } else {
    if (is.numeric(mad_override)) {
      comparison <- pupil_speed >= mad_override
    } else {
      stop("`mad_override` must either be `NULL` or numeric.")
    }
  }

  pupil <- ifelse(comparison, as.numeric(NA), pupil)

  return(pupil)
}

speed <- function(x, y) {
  delta <- diff(x) / diff(y)
  pupil <- abs(cbind(c(NA, delta), c(delta, NA))) # matrix of differences
  pupil <- apply(pupil, 1, function(row) {
    if (all(is.na(row))) {
      return(NA) # return NA for all-NA rows
    } else {
      return(max(row, na.rm = TRUE)) # only compute max for valid rows
    }
  })

  return(pupil)
}
