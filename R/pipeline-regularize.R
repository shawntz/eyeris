#' Regularize an irregularly-sampled pupil timeseries onto a uniform grid
#'
#' Reconstructs a uniform (consistently-spaced) time grid for each recording
#' segment by inserting placeholder rows wherever the tracker dropped samples.
#'
#' @details
#' Most of the `eyeris` pipeline (e.g., [eyeris::detransient()],
#' [eyeris::lpfilt()], [eyeris::downsample()]) assumes a fixed sampling rate.
#' EyeLink trackers honor that assumption by zero-filling missing pupil samples,
#' but some hardware instead *drops* samples entirely when pupil data is
#' missing, leaving holes in the otherwise evenly-spaced time vector. Those
#' holes silently distort any rate-dependent step.
#'
#' `regularize()` repairs the **time axis**: for each block it inserts a row at
#' every missing timestamp. Whether a block needs repair is decided by the
#' robust [check_uniform_sampling_intervals()] detector, which distinguishes
#' genuine dropped samples from data that only *looks* irregular -- notably
#' high-rate trackers that report integer-millisecond timestamps for
#' sub-millisecond samples (these are left untouched, so genuine samples are
#' never collapsed). For blocks it does repair, it rebuilds the ideal uniform
#' grid at the detected sampling period and inserts the missing rows. Inserted
#' rows carry `NA` for the pupil and gaze channels and are flagged in a new
#' logical `is_resampled` column so they can be tracked downstream.
#'
#' It does **not** fill the inserted values itself; that is the job of
#' [eyeris::interpolate()]. Running `regularize()` therefore turns the
#' "dropped-sample" problem into the ordinary "missing-value" (`NA`) problem
#' that the rest of the pipeline already handles.
#'
#' For data that is already uniformly sampled (e.g., EyeLink), `regularize()`
#' is a guaranteed no-op: no rows are inserted, no `is_resampled` column is
#' added, and the data is returned unchanged.
#'
#' If a block's spacing is not *predominantly* regular -- for example, a tracker
#' that systematically drops every Nth sample, or a recording with a very large
#' internal gap that should have been split into separate blocks -- rebuilding a
#' dense grid would fabricate an implausible amount of data. In that case
#' `regularize()` leaves the block untouched rather than synthesizing the grid.
#'
#' @note
#' This step is part of the `glassbox()` preprocessing pipeline and runs
#' **automatically by default** (it is a no-op unless irregular sampling is
#' detected). Opt out with `glassbox(regularize = FALSE)`, or tune the guard
#' with `glassbox(regularize = list(max_inflation = ...))`. Advanced users may
#' call it directly if needed.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param max_inflation The maximum factor by which a block's row count may grow
#' during grid reconstruction. Blocks whose reconstruction would exceed this
#' factor are left unchanged (with a warning), guarding against fabricating data
#' for systematically-decimated recordings or unsplit recording pauses. Defaults
#' to `2` (i.e., reconstruction may at most double the number of rows). Set to
#' `Inf` to always reconstruct
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#'
#' @return An `eyeris` object whose `timeseries` blocks have been placed on a
#' uniform time grid, with a new logical `is_resampled` column marking inserted
#' rows.
#'
#' @seealso [eyeris::interpolate()] for filling the gaps left by dropped
#' samples, and [eyeris::glassbox()] for the recommended way to run this step
#' as part of the full `eyeris` glassbox preprocessing pipeline.
#'
#' @export
regularize <- function(
  eyeris,
  max_inflation = 2,
  verbose = TRUE,
  call_info = NULL
) {
  call_info <- if (is.null(call_info)) {
    list(
      call_stack = match.call(),
      parameters = list(max_inflation = max_inflation, verbose = verbose)
    )
  } else {
    call_info
  }

  # handle binocular objects (process each eye independently)
  if (is_binocular_object(eyeris)) {
    list_out <- list(
      left = regularize(
        eyeris$left,
        max_inflation = max_inflation,
        verbose = verbose,
        call_info = call_info
      ),
      right = regularize(
        eyeris$right,
        max_inflation = max_inflation,
        verbose = verbose,
        call_info = call_info
      ),
      original_file = eyeris$original_file,
      raw_binocular_object = eyeris$raw_binocular_object
    )
    class(list_out) <- "eyeris"
    return(list_out)
  }

  # multiblock (named list of block data frames) vs single-frame fallback;
  # track whether any block was actually reconstructed (regularize_block adds
  # the `is_resampled` column only when it inserts rows) so that already-uniform
  # data stays a true no-op
  acted <- FALSE
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    for (block_name in names(eyeris$timeseries)) {
      eyeris$timeseries[[block_name]] <- regularize_block(
        eyeris$timeseries[[block_name]],
        max_inflation = max_inflation,
        block_label = block_name,
        verbose = verbose
      )
      acted <- acted ||
        "is_resampled" %in% colnames(eyeris$timeseries[[block_name]])
    }
  } else {
    eyeris$timeseries <- regularize_block(
      eyeris$timeseries,
      max_inflation = max_inflation,
      block_label = NULL,
      verbose = verbose
    )
    acted <- "is_resampled" %in% colnames(eyeris$timeseries)
  }

  # record provenance only when the grid was actually repaired, so that
  # already-uniform data is returned untouched (the `is_resampled` column's
  # presence is the true indicator that resampling occurred)
  if (acted) {
    if (!is.list(eyeris$params)) {
      eyeris$params <- list()
    }
    eyeris$params[["regularize"]] <- call_info
  }

  eyeris
}

#' Reconstruct a uniform time grid for a single block
#'
#' Inserts placeholder rows at the timestamps a tracker dropped, so that the
#' block's `time_orig` is evenly spaced at the inferred sampling period.
#'
#' This function is called by the exposed wrapper [eyeris::regularize()].
#'
#' @param block_df A single block's timeseries data frame (must contain a
#' `time_orig` column in milliseconds)
#' @param max_inflation The maximum allowed row-count growth factor (see
#' [eyeris::regularize()])
#' @param block_label Optional character label used in messages
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return The block data frame placed on a uniform grid, with inserted rows
#' marked in a logical `is_resampled` column. Returned unchanged (aside from
#' adding an all-`FALSE` `is_resampled` column) when no reconstruction is
#' needed or when reconstruction would be pathological.
#'
#' @keywords internal
regularize_block <- function(
  block_df,
  max_inflation = 2,
  block_label = NULL,
  verbose = TRUE
) {
  seg <- if (!is.null(block_label)) paste0(" in ", block_label) else ""

  # nothing to do without a time axis
  if (!is.data.frame(block_df) || !("time_orig" %in% colnames(block_df))) {
    return(block_df)
  }

  t <- block_df$time_orig
  n <- nrow(block_df)
  if (n < 3 || any(is.na(t))) {
    return(block_df)
  }

  # robust gate: only reconstruct when the detector flags genuine irregularity.
  # The detector treats high-rate trackers that report integer-millisecond
  # timestamps for sub-millisecond samples as uniform (duplicate / zero-length
  # intervals are a sub-ms-resolution tell-tale, not dropped samples), so we
  # never collapse genuine samples. It also distinguishes sporadic dropout from
  # systematic decimation, and already-uniform data (e.g., EyeLink) is uniform.
  hz <- if ("hz" %in% colnames(block_df)) block_df$hz[1] else NULL
  detected <- check_uniform_sampling_intervals(t, hz = hz, verbose = FALSE)
  if (isTRUE(detected$uniform)) {
    return(block_df)
  }

  period <- detected$expected_interval
  if (!is.finite(period) || period <= 0) {
    return(block_df)
  }

  # map each existing sample onto an integer grid index anchored at the start
  start <- t[1]
  idx <- round((t - start) / period)

  # defensive: the detector gate excludes zero-length intervals (sub-ms
  # resolution), but within-period jitter could still round two samples onto the
  # same grid slot. Rather than collapse genuine samples, leave the block as-is.
  if (anyDuplicated(idx)) {
    return(block_df)
  }

  n_present <- length(idx)
  n_grid <- max(idx) + 1L
  n_inserted <- n_grid - n_present
  if (n_inserted <= 0) {
    # detector flagged irregularity, but the survivors already form a uniform
    # (coarser) grid -- e.g., pure systematic decimation. Reconstructing would
    # mean fabricating data, so leave it as-is (the load-time guardrail warns).
    return(block_df)
  }

  # guard against fabricating an implausible amount of data (systematic dropout
  # or an unsplit recording pause): leave the block untouched and warn
  if (is.finite(max_inflation) && n_grid > n_present * max_inflation) {
    log_warn(
      paste0(
        "Skipping grid reconstruction{seg}: rebuilding the uniform grid would ",
        "grow the data from {n_present} to {n_grid} rows ",
        "(> {max_inflation}x). This usually indicates systematic sample ",
        "dropout or a large recording gap rather than sporadic dropped ",
        "samples; the time axis was left as-is. Inspect the recording and, if ",
        "appropriate, raise `max_inflation`."
      ),
      verbose = verbose
    )
    return(block_df)
  }

  # we are reconstructing: add the marker column now (kept absent on every
  # no-op path so already-uniform data is returned byte-identical)
  if (!("is_resampled" %in% colnames(block_df))) {
    block_df$is_resampled <- FALSE
  }

  # reindex existing rows onto the uniform grid; slots with no source sample
  # become NA rows (tibble-safe single indexing op)
  src <- match(seq_len(n_grid) - 1L, idx)
  inserted <- is.na(src)
  out <- block_df[src, , drop = FALSE]
  rownames(out) <- NULL

  # mark inserted rows (preserving any pre-existing flags on real rows)
  out$is_resampled[inserted] <- TRUE

  # rebuild the time axis on the uniform grid
  new_time_orig <- start + (seq_len(n_grid) - 1L) * period
  out$time_orig <- new_time_orig
  if ("time_secs" %in% colnames(out)) {
    out$time_secs <- (new_time_orig - new_time_orig[1]) / 1000
  }
  if ("time_scaled" %in% colnames(out)) {
    out$time_scaled <- (new_time_orig - new_time_orig[1]) / 1000
  }

  # carry forward block-constant metadata columns into inserted rows
  for (col in intersect(c("block", "eye", "hz", "type"), colnames(out))) {
    fill <- block_df[[col]][!is.na(block_df[[col]])][1]
    out[[col]][inserted] <- fill
  }

  pct <- round(100 * n_inserted / n_grid, 2)
  log_info(
    paste0(
      "Regularized sampling grid{seg}: inserted {n_inserted} missing sample(s) ",
      "({pct}%) at the inferred {period} ms sampling period. Inserted samples ",
      "are flagged in `is_resampled` and left as `NA` for interpolation."
    ),
    verbose = verbose
  )

  out
}
