#' NA-pad blink events / missing data
#'
#' Deblinking (a.k.a. NA-padding) of time series data. The intended use of
#' this method is to remove blink-related artifacts surrounding periods of
#' missing data. For instance, when an individual blinks, there are usually
#' rapid decreases followed by increases in pupil size, with a chunk of data
#' missing in-between these 'spike'-looking events. The deblinking procedure
#' here will NA-pad each missing data point by your specified number of ms.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Provide parameters via
#' `deblink = list(...)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' This function is automatically called by `glassbox()` by default. If needed,
#' customize the parameters for `deblink` by providing a parameter list. Use
#' `glassbox(deblink = FALSE)` to disable this step as needed.
#'
#' Users should prefer using `glassbox()` rather than invoking this function
#' directly unless they have a specific reason to customize the pipeline
#' manually.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param extend Either a single number indicating the number of milliseconds to
#' pad forward/backward around each missing sample, or, a vector of length two
#' indicating different numbers of milliseconds pad forward/backward around each
#' missing sample, in the format `c(backward, forward)`
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#'
#' @return An `eyeris` object with a new column: `pupil_raw_{...}_deblink`
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # 50 ms in both directions (the default)
#' demo_data |>
#'   eyeris::glassbox(deblink = list(extend = 50)) |>
#'   plot(seed = 0)
#'
#' # 40 ms backward, 50 ms forward
#' demo_data |>
#'   # set deblink to FALSE (instead of a list of params)
#'   #  to skip step (not recommended)
#'   eyeris::glassbox(deblink = list(extend = c(40, 50))) |>
#'   plot(seed = 0)
#'
#' @export
deblink <- function(eyeris, extend = 50, call_info = NULL) {
  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(extend = extend)
    )
  } else {
    call_info
  }

  eyeris |>
    pipeline_handler(
      deblink_pupil,
      "deblink",
      extend = extend,
      call_info = call_info
    )
}

#' Internal function to remove blink artifacts from pupil data
#'
#' @description This function implements blink artifact removal by extending
#' the duration of detected blinks (missing samples) by a specified number of
#' milliseconds both forward and backward in time. This helps to remove
#' deflections in pupil size that occur due to eyelid movements during and
#' around actual blink periods.
#'
#' This function is called by the exposed wrapper [eyeris::deblink()].
#'
#' @details The function works by:
#' \itemize{
#'   \item Identifying missing samples as blink periods
#'   \item Extending these periods by the specified number of milliseconds
#'   \item Setting all samples within the extended blink periods to NA
#'   \item Preserving all other samples unchanged
#' }
#'
#' This implementation is based on the approach described in the pupillometry
#' package by dr-JT
#' (\url{https://github.com/dr-JT/pupillometry/blob/main/R/pupil_deblink.R}).
#'
#' @param x A data frame containing pupil data with columns `time_orig` and
#'   the previous operation's pupil column
#' @param prev_op The name of the previous operation's pupil column
#' @param extend Either a single number indicating symmetric padding in both
#'   directions, or a vector of length 2 indicating asymmetric padding in the
#'   format `c(backward, forward)` in milliseconds. Defaults to `50`
#'
#' @return A numeric vector of the same length as the input data with blink
#'   artifacts removed (set to NA)
#'
#' @keywords internal
deblink_pupil <- function(x, prev_op, extend) {
  column <- dplyr::sym(prev_op)

  if (length(extend) == 1) { # symmetric blink padding case
    extend_backward <- extend
    extend_forward <- extend
  } else if (length(extend) == 2) { # asymmetric
    extend_backward <- extend[1]
    extend_forward <- extend[2]
  } else {
    cli::cli_abort(
      paste(
        "extend must either be a single integer (symmetric) or a vector of",
        "length 2 (asymmetric) in the format `c(backward, forward)`!"
      )
    )
  }

  x |>
    dplyr::select(
      time = time_orig,
      pupil = !!column
    ) |>
    dplyr::mutate(
      blink = ifelse(is.na(pupil), 1, 0),
      blink.lag = dplyr::lag(blink),
      blink.lead = dplyr::lead(blink),
      blink.start = ifelse(blink == 1 & !is.na(blink.lag) & blink.lag == 0,
        time, as.numeric(NA)
      ),
      blink.start = zoo::na.locf(blink.start, na.rm = FALSE, fromLast = TRUE),
      blink.end = ifelse(blink == 1 & !is.na(blink.lead) & blink.lead == 0,
        time, as.numeric(NA)
      ),
      blink.end = zoo::na.locf(blink.end, na.rm = FALSE),
      blink = ifelse(!is.na(blink.start) &
                       time >= blink.start - extend_backward &
                       time <= blink.start, 1, blink),
      blink = ifelse(!is.na(blink.end) &
                       time <= blink.end + extend_forward &
                       time >= blink.end, 1, blink),
      pupil_deblink = ifelse(pupil == 0 | blink == 1, as.numeric(NA), pupil)
    ) |>
    dplyr::pull(pupil_deblink)
}
