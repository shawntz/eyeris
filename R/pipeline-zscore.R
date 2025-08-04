#' Z-score pupil time series data
#'
#' The intended use of this method is to scale the arbitrary units of the pupil
#' size time series to have a mean of `0` and a standard deviation of `1`. This
#' is accomplished by mean centering the data points and then dividing them by
#' their standard deviation (i.e., z-scoring the data, similar to
#' [base::scale()]). Opting to z-score your pupil data helps with trial-level
#' and between-subjects analyses where arbitrary units of pupil size recorded by
#' the tracker do not scale across participants, and therefore make analyses
#' that depend on data from more than one participant difficult to interpret.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Use `glassbox(zscore = TRUE)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' This function is automatically called by `glassbox()` by default. Use
#' `glassbox(zscore = FALSE)` to disable this step as needed.
#'
#' Users should prefer using `glassbox()` rather than invoking this function
#' directly unless they have a specific reason to customize the pipeline
#' manually.
#'
#' In general, it is common to z-score pupil data within any given
#' participant, and furthermore, z-score that participant's data as a function
#' of block number (for tasks/experiments where participants complete more than
#' one block of trials) to account for potential time-on-task effects across
#' task/experiment blocks.
#'
#' As such, if you use the `eyeris` package as intended, you should NOT need
#' to specify any groups for the participant/block-level situations described
#' above. This is because `eyeris` is designed to preprocess a single block of
#' pupil data for a single participant, one at a time. Therefore, when you later
#' merge all of the preprocessed data from `eyeris`, each individual,
#' preprocessed block of data for each participant will have already been
#' independently scaled from the others.
#'
#' Additionally, if you intend to compare mean z-scored pupil size across task
#' conditions, such as that for memory successes vs. memory failures, then do
#' NOT set your behavioral outcome (i.e., success/failure) variable as a
#' grouping variable within your analysis. If you do, you will consequently
#' obtain a mean pupil size of 0 and standard deviation of 1 within each group
#' (since the scaled pupil size would be calculated on the time series from each
#' outcome variable group, separately). Instead, you should compute the z-score
#' on the entire pupil time series (before epoching the data), and then split and
#' take the mean of the z-scored time series as a function of condition variable.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#'
#' @return An `eyeris` object with a new column in `time series`:
#' `pupil_raw_{...}_z`
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   eyeris::glassbox(zscore = TRUE) |> # set to FALSE to skip (not recommended)
#'   plot(seed = 0)
#'
#' @export
zscore <- function(eyeris, call_info = NULL) {
  call_info <- if (is.null(call_info)) {
    list(call_stack = match.call(), parameters = list())
  } else {
    call_info
  }

  # handle binocular objects
  if (is_binocular_object(eyeris)) {
    # process left and right eyes independently
    left_result <- eyeris$left |>
      pipeline_handler(zscore_pupil, "z", call_info = call_info)

    right_result <- eyeris$right |>
      pipeline_handler(zscore_pupil, "z", call_info = call_info)

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
    eyeris |> pipeline_handler(zscore_pupil, "z", call_info = call_info)
  }
}

#' Internal function to z-score pupil data
#'
#' @description This function z-scores pupil data by subtracting the mean and
#' dividing by the standard deviation.
#'
#' This function is called by the exposed wrapper [eyeris::zscore()]
#'
#' @param x A data frame containing pupil data
#' @param prev_op The name of the previous operation in the pipeline
#'
#' @return A vector of z-scored pupil data
#'
#' @keywords internal
zscore_pupil <- function(x, prev_op) {
  # validate the previous operation column name
  if (is.null(prev_op) || length(prev_op) == 0 || prev_op == "") {
    log_error("Previous operation column name is empty or NULL.")
  }

  if (!prev_op %in% colnames(x)) {
    log_error(
      "Column '{prev_op}' not found in data. Available columns: {paste(colnames(x), collapse = ', ')}"
    )
  }

  # check for duplicate suffixes in column name (might indicate corruption)
  if (grepl("_([^_]+)_\\1", prev_op)) {
    log_error(
      "Corrupted column name detected: {prev_op}. This might indicate an eyeris pipeline processing error."
    )
  }

  pupil_col <- dplyr::sym(prev_op)

  x |> dplyr::mutate(zscore = get_zscores(!!pupil_col)) |> dplyr::pull(zscore)
}

get_zscores <- function(x) {
  means <- mean(x, na.rm = TRUE)
  sds <- sd(x, na.rm = TRUE)

  (x - means) / sds
}
