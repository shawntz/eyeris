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
#' @details
#' By default, only gaps shorter than or equal to `max_gap_ms` milliseconds are
#' interpolated. Any gap longer than this threshold is left as `NA` rather than
#' being interpolated over. This follows the recommendation of Kret &
#' Sjak-Shie (2018) to avoid interpolating across long stretches of missing
#' data, where linear interpolation is unlikely to reflect the true underlying
#' pupil signal. The default of `250` ms matches the value used in that paper.
#'
#' Set `max_gap_ms = Inf` (or `NULL`) to disable the limit and interpolate
#' across all gaps, restoring the behavior of `eyeris` versions <= 3.2.0.
#'
#' Downstream `glassbox()` steps that cannot operate on missing data (low-pass
#' filtering, downsampling, and binning) automatically work *around* these
#' retained gaps -- filtering/resampling over a temporarily filled copy and
#' then restoring the gaps as `NA` -- so the gaps are preserved through to the
#' final preprocessed output. Because of this temporary fill, the filtering
#' steps (`lpfilt()` and the anti-aliasing filter in `downsample()`) can
#' slightly bias the valid samples immediately adjacent to a long retained gap
#' toward the interpolated values. These steps therefore emit a warning when
#' they operate over such gaps, so you can choose to disable them (e.g.
#' `lpfilt = FALSE` and/or `downsample = FALSE` in `glassbox()`) if this bias is
#' a concern for your analysis.
#'
#' \strong{Note:} Prior to `eyeris` version 3.3.0, all gaps were interpolated
#' regardless of duration. Enforcing `max_gap_ms` is a change in default
#' behavior and may affect downstream results.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Use `glassbox(interpolate = TRUE)`,
#' or provide parameters via `glassbox(interpolate = list(max_gap_ms = ...))`.
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
#' @param max_gap_ms The maximum duration (in milliseconds) of a gap of missing
#' (`NA`) pupil samples that will be interpolated. Gaps longer than this
#' threshold are left as `NA` (not interpolated). Must be greater than `0`;
#' defaults to `250` ms, the value recommended by Kret & Sjak-Shie (2018). Set
#' to `Inf` (or `NULL`) to interpolate across all gaps regardless of duration.
#' The threshold is converted to a whole number of samples (rounded down) using
#' each recording's sampling rate. To skip interpolation entirely, set
#' `interpolate = FALSE` in [eyeris::glassbox()] instead
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`. Set to `FALSE` to suppress messages about the current
#' processing step and run silently
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_raw_{...}_interpolate`
#'
#' @references
#' Kret, M. E., & Sjak-Shie, E. E. (2018). Preprocessing pupil size data:
#' Guidelines and code. \emph{Behavior Research Methods, 51}(3), 1336-1342.
#' \doi{10.3758/s13428-018-1075-y}
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full `eyeris` glassbox preprocessing pipeline.
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
#' demo_data |>
#'   # set to FALSE to skip (not recommended)
#'   eyeris::glassbox(interpolate = TRUE) |>
#'   # `preview_window` zooms in on a 2-second subset of the time series
#'   plot(seed = 0, preview_window = c(10, 12))
#'
#' # only interpolate gaps up to 100 ms; leave longer gaps as NA
#' demo_data |>
#'   eyeris::glassbox(interpolate = list(max_gap_ms = 100)) |>
#'   plot(seed = 0, preview_window = c(10, 12))
#'
#' @export
interpolate <- function(
  eyeris,
  max_gap_ms = 250,
  verbose = TRUE,
  call_info = NULL
) {
  max_gap_ms <- validate_max_gap_ms(max_gap_ms)

  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(max_gap_ms = max_gap_ms, verbose = verbose)
    )
  } else {
    call_info
  }

  # one-time-per-run heads-up about the change in default behavior
  if (is.finite(max_gap_ms)) {
    notify_max_gap_behavior_change(max_gap_ms, verbose = verbose)
  }

  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- eyeris$left |>
      pipeline_handler(
        interpolate_pupil,
        "interpolate",
        max_gap_ms = max_gap_ms,
        verbose = verbose,
        call_info = call_info
      )

    right_result <- eyeris$right |>
      pipeline_handler(
        interpolate_pupil,
        "interpolate",
        max_gap_ms = max_gap_ms,
        verbose = verbose,
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
        max_gap_ms = max_gap_ms,
        verbose = verbose,
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
#' Gaps longer than `max_gap_ms` milliseconds are left as `NA` rather than
#' interpolated. The threshold is converted from milliseconds to a number of
#' samples using the sampling period of the data (estimated from `time_orig`),
#' so it is robust to different sampling rates.
#'
#' This function is called by the exposed wrapper [eyeris::interpolate()].
#'
#' @param x A data frame containing the pupil time series data
#' @param prev_op The name of the previous operation's output column
#' @param verbose A flag to indicate whether to print detailed logging messages
#' @param max_gap_ms The maximum duration (in milliseconds) of a gap of missing
#' (`NA`) samples to interpolate. Gaps longer than this are left as `NA`. Must
#' be greater than `0`; use `Inf` or `NULL` to interpolate across all gaps.
#' Defaults to `250`
#'
#' @return A vector of interpolated pupil values with the same length as the
#' input
#'
#' @keywords internal
interpolate_pupil <- function(x, prev_op, verbose, max_gap_ms = 250) {
  prev_pupil <- x[[prev_op]]

  if (!any(is.na(prev_pupil))) {
    log_warn(
      "No NAs detected in pupil data... Skipping interpolation!",
      verbose = verbose
    )
    return(prev_pupil)
  }

  # treat NULL/Inf as "no limit" (interpolate across all gaps)
  if (is.null(max_gap_ms) || !is.finite(max_gap_ms)) {
    return(zoo::na.approx(prev_pupil, na.rm = FALSE, maxgap = Inf, rule = 2))
  }

  # convert the max-gap duration (ms) into a number of consecutive samples
  # using the data's own sampling period; robust to different sampling rates.
  # The threshold is floored to a whole number of samples (a tiny epsilon
  # guards against floating-point error at exact boundaries, e.g. 250 ms at
  # 60 Hz), so the effective limit is the largest whole number of samples whose
  # total duration is <= max_gap_ms.
  max_gap_samples <- Inf
  if ("time_orig" %in% colnames(x)) {
    sample_period_ms <- stats::median(diff(x[["time_orig"]]), na.rm = TRUE)
    if (is.finite(sample_period_ms) && sample_period_ms > 0) {
      max_gap_samples <- floor(max_gap_ms / sample_period_ms + 1e-9)
    } else {
      # the requested guideline limit could not be honored -> always warn
      log_warn(
        paste0(
          "Could not estimate a sampling period from `time_orig`; ",
          "interpolating across all gaps (`max_gap_ms` not enforced)."
        ),
        verbose = TRUE
      )
    }
  } else {
    # the requested guideline limit could not be honored -> always warn
    log_warn(
      paste0(
        "`time_orig` column not found; cannot enforce `max_gap_ms`. ",
        "Interpolating across all gaps."
      ),
      verbose = TRUE
    )
  }

  interpolated <- zoo::na.approx(
    prev_pupil,
    na.rm = FALSE,
    maxgap = max_gap_samples,
    rule = 2
  )

  # report how many samples were intentionally left as NA (long gaps)
  n_left_na <- sum(is.na(interpolated) & is.na(prev_pupil))
  if (is.finite(max_gap_samples) && n_left_na > 0) {
    log_warn(
      paste0(
        "Left {n_left_na} sample(s) as NA across gaps longer than ",
        "{max_gap_ms} ms (not interpolated)."
      ),
      verbose = verbose
    )
  }

  interpolated
}

#' Validate the `max_gap_ms` argument for interpolation
#'
#' @param max_gap_ms The user-supplied maximum gap duration in milliseconds
#'
#' @return The validated `max_gap_ms` value (`Inf` if `NULL` was supplied)
#'
#' @keywords internal
validate_max_gap_ms <- function(max_gap_ms) {
  if (is.null(max_gap_ms)) {
    return(Inf)
  }

  if (!is.numeric(max_gap_ms) || length(max_gap_ms) != 1 || is.na(max_gap_ms)) {
    log_error(paste0(
      "`max_gap_ms` must be a single number (in milliseconds), `Inf`, ",
      "or `NULL`."
    ))
  }

  if (max_gap_ms <= 0) {
    log_error(paste0(
      "`max_gap_ms` must be greater than 0 ms. To skip interpolation ",
      "entirely, set `interpolate = FALSE` in `glassbox()` (or do not call ",
      "`interpolate()`)."
    ))
  }

  max_gap_ms
}

#' Notify the user (once per run) about the max-gap behavior change
#'
#' Emits a one-time-per-run warning explaining that interpolation now leaves
#' gaps longer than `max_gap_ms` as `NA`, a change in default behavior from
#' `eyeris` versions <= 3.2.0. The flag is cleared by [reset_gap_notices()],
#' which `glassbox()` calls at the start of each run.
#'
#' @param max_gap_ms The active maximum gap duration in milliseconds
#' @param verbose A flag to indicate whether to print the message
#'
#' @return Invisibly returns `NULL`
#'
#' @keywords internal
notify_max_gap_behavior_change <- function(max_gap_ms, verbose = TRUE) {
  if (!verbose) {
    return(invisible(NULL))
  }

  if (isTRUE(.eyeris_session$max_gap_notified)) {
    return(invisible(NULL))
  }

  log_warn(
    paste0(
      "Interpolation now leaves gaps longer than {max_gap_ms} ms as `NA` ",
      "instead of interpolating across them (following Kret & Sjak-Shie, ",
      "2018). This is a change in default behavior from eyeris <= 3.2.0 and ",
      "may affect your results. To restore the previous behavior, set ",
      "`interpolate = list(max_gap_ms = Inf)` in `glassbox()` (or ",
      "`max_gap_ms = Inf` in `interpolate()`)."
    ),
    verbose = verbose
  )

  .eyeris_session$max_gap_notified <- TRUE

  invisible(NULL)
}

#' Warn (once per run) that a filter/resampling step is operating over gaps
#'
#' Emits a one-time-per-run warning explaining that a step which relies on
#' filtering (e.g. `lpfilt()` or the anti-aliasing filter in `downsample()`) is
#' operating over long gaps that `interpolate(max_gap_ms)` left as `NA`. Because
#' those gaps are temporarily filled to let the filter run and then masked back
#' to `NA`, the filter can slightly bias the valid samples immediately adjacent
#' to each gap toward the interpolated values. Users may prefer to disable
#' filtering and/or downsampling. The flag is cleared by [reset_gap_notices()],
#' which `glassbox()` calls at the start of each run.
#'
#' @param step Character label of the calling step (e.g. `"lpfilt"`,
#' `"downsample"`), used both in the message and to fire the notice only once
#' per run per step
#'
#' @return Invisibly returns `NULL`
#'
#' @keywords internal
warn_filter_over_gaps <- function(step) {
  flag_name <- paste0(step, "_gap_warned")

  if (isTRUE(.eyeris_session[[flag_name]])) {
    return(invisible(NULL))
  }

  log_warn(
    paste0(
      "`",
      step,
      "()` is operating on data that contains gaps longer than the ",
      "interpolation limit (`max_gap_ms`), which were left as `NA`. These gaps ",
      "are temporarily filled so the filter can run and then masked back to ",
      "`NA`; this can slightly bias the valid pupil samples immediately ",
      "adjacent to each gap toward the interpolated values. If this bias is a ",
      "concern for your analysis, consider disabling filtering and/or ",
      "downsampling (e.g. `lpfilt = FALSE` and/or `downsample = FALSE` in ",
      "`glassbox()`)."
    ),
    verbose = TRUE
  )

  .eyeris_session[[flag_name]] <- TRUE

  invisible(NULL)
}

#' Reset the per-run gap-related notice flags
#'
#' Clears the session flags used by [notify_max_gap_behavior_change()] and
#' [warn_filter_over_gaps()] so that those notices fire at most once per
#' `glassbox()` run (rather than once per R session). `glassbox()` calls this at
#' the start of each run.
#'
#' @return Invisibly returns `NULL`
#'
#' @keywords internal
reset_gap_notices <- function() {
  .eyeris_session$max_gap_notified <- NULL
  .eyeris_session$lpfilt_gap_warned <- NULL
  .eyeris_session$downsample_gap_warned <- NULL

  invisible(NULL)
}
