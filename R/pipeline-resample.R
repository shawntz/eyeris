#' Resample an irregularly-sampled pupil timeseries onto a uniform grid
#'
#' Places each recording segment onto the expected uniform (consistently-spaced)
#' sampling grid: local sub-period timing jitter is smoothed by linear
#' interpolation, and longer-than-expected gaps (dropped samples) become explicit
#' `NA` rows for the later [eyeris::interpolate()] step to handle.
#'
#' @details
#' Most of the `eyeris` pipeline (e.g., [eyeris::detransient()],
#' [eyeris::lpfilt()], [eyeris::downsample()]) assumes a fixed sampling rate.
#' EyeLink trackers honor that assumption by zero-filling missing pupil samples,
#' but some hardware instead *drops* samples entirely when pupil data is
#' missing, leaving holes in the otherwise evenly-spaced time vector. Those
#' holes silently distort any rate-dependent step.
#'
#' `resample()` repairs the **time axis** in two stages. Whether a block needs
#' repair is decided by the robust [check_uniform_sampling_intervals()] detector,
#' which distinguishes genuine dropped samples from data that only *looks*
#' irregular -- notably high-rate trackers that report integer-millisecond
#' timestamps for sub-millisecond samples (these are left untouched, so genuine
#' samples are never collapsed). For blocks it does repair:
#'
#' 1. **Build the target grid.** The uniform grid is anchored on the first
#'    *reliable* regular interval -- the first observed interval that matches the
#'    expected sampling period -- rather than on the first timestamp. This keeps
#'    early sub-period jitter (e.g., intervals of 3, 3, 4, 4 ms at a 4 ms period)
#'    from offsetting the whole grid. The grid is then extended in both
#'    directions at the expected period so that it spans every observed
#'    timestamp, including any samples that precede the anchor (back-extension).
#'
#' 2. **Resample onto the grid.** Observed samples are placed on the grid by
#'    linear interpolation: samples that land on a grid point are kept verbatim,
#'    and short/jittered intervals are interpolated across so their values
#'    contribute to the regular grid. Any observed interval longer than the
#'    expected period is treated as a real gap: the missing grid sample(s) inside
#'    it are inserted as `NA` rather than interpolated across, and flagged in a
#'    new logical `is_resampled` column so they can be tracked downstream.
#'
#' `resample()` does **not** fill the inserted `NA` values itself; that is the
#' job of [eyeris::interpolate()], which decides how much of a missing span to
#' fill according to its own missing-data policy. Running `resample()` therefore
#' turns the "dropped-sample" problem into the ordinary "missing-value" (`NA`)
#' problem that the rest of the pipeline already handles.
#'
#' For data that is already uniformly sampled (e.g., EyeLink), `resample()` is a
#' guaranteed no-op: no rows are inserted, no `is_resampled` column is added, and
#' the data is returned unchanged. The same is true of a block whose surviving
#' samples already form a uniform (coarser) grid -- e.g., pure systematic
#' decimation -- where there is nothing to insert without fabricating data.
#'
#' @note
#' This step is part of the `glassbox()` preprocessing pipeline and runs
#' **automatically by default** (it is a no-op unless irregular sampling is
#' detected). Opt out with `glassbox(resample = FALSE)`. Advanced users may call
#' it directly if needed.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`
#' @param call_info A list of call information and parameters. If not provided,
#' it will be generated from the function call
#'
#' @return An `eyeris` object whose `timeseries` blocks have been placed on a
#' uniform time grid, with a new logical `is_resampled` column marking inserted
#' (gap) rows.
#'
#' @seealso [eyeris::interpolate()] for filling the gaps left by dropped
#' samples, and [eyeris::glassbox()] for the recommended way to run this step
#' as part of the full `eyeris` glassbox preprocessing pipeline.
#'
#' @export
resample <- function(eyeris, verbose = TRUE, call_info = NULL) {
  call_info <- if (is.null(call_info)) {
    list(call_stack = match.call(), parameters = list(verbose = verbose))
  } else {
    call_info
  }

  # handle binocular objects (process each eye independently)
  if (is_binocular_object(eyeris)) {
    list_out <- list(
      left = resample(eyeris$left, verbose = verbose, call_info = call_info),
      right = resample(eyeris$right, verbose = verbose, call_info = call_info),
      original_file = eyeris$original_file,
      raw_binocular_object = eyeris$raw_binocular_object
    )
    class(list_out) <- "eyeris"
    return(list_out)
  }

  # multiblock (named list of block data frames) vs single-frame fallback;
  # track whether *this* call actually resampled any block. resample_block()
  # returns the block untouched on every no-op path, so a block that changed is
  # exactly one that differs from its input -- this keeps already-uniform data,
  # and idempotent re-runs on already-resampled data (whose `is_resampled`
  # column persists), true no-ops rather than keying off the column's presence
  acted <- FALSE
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    for (block_name in names(eyeris$timeseries)) {
      before <- eyeris$timeseries[[block_name]]
      after <- resample_block(
        before,
        block_label = block_name,
        verbose = verbose
      )
      eyeris$timeseries[[block_name]] <- after
      acted <- acted || !identical(before, after)
    }
  } else {
    before <- eyeris$timeseries
    after <- resample_block(before, block_label = NULL, verbose = verbose)
    eyeris$timeseries <- after
    acted <- !identical(before, after)
  }

  # record provenance only when this call actually repaired the grid, so that
  # already-uniform data -- and idempotent re-runs on already-resampled data --
  # are returned untouched without overwriting the original provenance
  if (acted) {
    if (!is.list(eyeris$params)) {
      eyeris$params <- list()
    }
    eyeris$params[["resample"]] <- call_info
  }

  eyeris
}

#' Resample a single block onto a uniform time grid
#'
#' Places one block's samples onto the expected uniform sampling grid:
#' interpolates values for sub-period timing jitter and inserts `NA` rows at
#' longer-than-expected gaps (dropped samples).
#'
#' This function is called by the exposed wrapper [eyeris::resample()].
#'
#' @param block_df A single block's timeseries data frame (must contain a
#' `time_orig` column in milliseconds)
#' @param block_label Optional character label used in messages
#' @param verbose A flag to indicate whether to print detailed logging messages
#'
#' @return The block data frame placed on a uniform grid, with inserted (gap)
#' rows marked in a logical `is_resampled` column. Returned unchanged (with no
#' `is_resampled` column added) when no reconstruction is needed.
#'
#' @keywords internal
resample_block <- function(block_df, block_label = NULL, verbose = TRUE) {
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

  # robust gate: only resample when the detector flags genuine irregularity.
  # Already-uniform data (e.g., EyeLink) is uniform, and high-rate trackers that
  # report integer-millisecond timestamps for sub-millisecond samples are treated
  # as uniform too (their duplicate / zero-length intervals are a sub-ms
  # tell-tale, not dropped samples), so genuine samples are never collapsed.
  hz <- if ("hz" %in% colnames(block_df)) block_df$hz[1] else NULL
  detected <- check_uniform_sampling_intervals(t, hz = hz, verbose = FALSE)
  if (isTRUE(detected$uniform)) {
    return(block_df)
  }

  period <- detected$expected_interval
  if (!is.finite(period) || period <= 0) {
    return(block_df)
  }

  intervals <- diff(t)

  # never collapse genuine sub-period samples: a zero-length (duplicate) interval
  # is the sub-millisecond-resolution tell-tale of a high-rate tracker reporting
  # integer-ms timestamps. The detector only lets such a block through when it
  # *also* contains a real gap; forcing it onto the coarser modal grid would
  # merge genuine samples, so leave it untouched (the load-time guardrail warns).
  if (any(intervals <= 0)) {
    return(block_df)
  }

  # tolerances, expressed as fractions of one sampling period
  anchor_tol <- 1e-6 # an interval "equals" the period within this rel. tolerance
  grid_tol <- 1e-6 # snap grid endpoints that sit ~exactly on t[1] / t[n]
  coincide_tol <- 0.1 # a sample within this frac of a grid point is "on-grid"

  # ---- Stage 1: build the target grid from the first *reliable* regular interval
  # Anchor the grid's phase on the first interval that matches the expected
  # period, rather than on the first timestamp, so that early sub-period jitter
  # (e.g., 3, 3, 4, 4 ms intervals at a 4 ms period) does not offset the whole
  # grid. Fall back to the first sample if no interval matches exactly.
  near_period <- abs(intervals - period) <= period * anchor_tol
  anchor_i <- if (any(near_period)) which(near_period)[1] else 1L
  anchor_time <- t[anchor_i]

  # Extend the grid in both directions from the anchor so it spans every observed
  # timestamp -- including any samples that precede the anchor (back-extension) --
  # without fabricating points outside the observed range.
  k_first <- ceiling((t[1] - anchor_time) / period - grid_tol)
  k_last <- floor((t[n] - anchor_time) / period + grid_tol)
  grid_time <- anchor_time + (k_first:k_last) * period
  n_grid <- length(grid_time)

  # ---- Stage 2: classify each observed interval and resample onto the grid
  # An observed interval spanning >= 2 grid steps is a real gap (dropped
  # samples); a shorter, jittered interval spans ~1 step and is interpolated
  # across. `round()` keeps both robust to sub-period timing jitter.
  gap_steps <- round(intervals / period)
  is_gap <- gap_steps >= 2

  # Map each observed sample onto the grid slot it lands on, and record which
  # samples sit (near) exactly on a grid point -- those are genuine samples kept
  # verbatim; off-phase (jittered) samples instead feed the interpolation.
  slot <- round((t - anchor_time) / period)
  frac <- (t - anchor_time) / period - slot
  on_grid_sample <- abs(frac) <= coincide_tol
  grid_index <- slot - k_first + 1L

  # source row for each grid point (NA where no genuine sample lands on it)
  src <- rep(NA_integer_, n_grid)
  keep <- on_grid_sample & grid_index >= 1L & grid_index <= n_grid
  ki <- grid_index[keep]
  kj <- which(keep)
  first <- !duplicated(ki) # defensive: if two on-grid samples share a slot
  src[ki[first]] <- kj[first]
  on_sample <- !is.na(src)

  # which grid points fall inside a real gap (and are not themselves a genuine
  # sample) -> these become the inserted NA rows for interpolate() to fill
  i_left <- findInterval(grid_time, t)
  gap_at_grid <- logical(n_grid)
  in_range <- i_left >= 1L & i_left <= (n - 1L)
  gap_at_grid[in_range] <- is_gap[i_left[in_range]]
  inserted <- gap_at_grid & !on_sample

  # if resampling would not insert any NA gap rows there is nothing genuine to
  # repair (e.g., pure systematic decimation, whose survivors already form a
  # uniform coarser grid) -- leave the block byte-identical, no marker column
  if (!any(inserted)) {
    return(block_df)
  }

  # ---- assemble the resampled block
  # start from the genuine rows (carries metadata / any extra columns); slots
  # with no source sample begin as all-NA and are filled below
  out <- block_df[src, , drop = FALSE]
  rownames(out) <- NULL

  # provenance marker (kept absent on every no-op path above so already-uniform
  # data is returned byte-identical); preserve any pre-existing flags on real rows
  prior <- if ("is_resampled" %in% colnames(block_df)) {
    block_df$is_resampled[src]
  } else {
    rep(NA, n_grid)
  }
  out$is_resampled <- inserted | (!is.na(prior) & prior)

  # rebuild the time axis on the uniform grid
  out$time_orig <- grid_time
  if ("time_secs" %in% colnames(out)) {
    out$time_secs <- (grid_time - grid_time[1]) / 1000
  }
  if ("time_scaled" %in% colnames(out)) {
    out$time_scaled <- (grid_time - grid_time[1]) / 1000
  }

  # carry block-constant metadata into every synthesized row
  synthesized <- is.na(src)
  for (col in intersect(c("block", "eye", "hz", "type"), colnames(out))) {
    fill <- block_df[[col]][!is.na(block_df[[col]])][1]
    out[[col]][synthesized] <- fill
  }

  # resample the data channels onto the grid: linear interpolation carries the
  # observed samples (including jitter correction) onto the grid timestamps; then
  # real-gap interiors are blanked to NA so interpolate() decides how to fill them.
  # `na.rm = FALSE` keeps source NAs (missing observations) from being silently
  # interpolated over, so a missing sample stays missing for interpolate() rather
  # than being filled here.
  data_cols <- colnames(block_df)[
    vapply(block_df, is.numeric, logical(1)) &
      !(colnames(block_df) %in%
        c("time_orig", "time_secs", "time_scaled", "block", "hz"))
  ]
  for (col in data_cols) {
    v <- block_df[[col]]
    yg <- stats::approx(
      x = t,
      y = v,
      xout = grid_time,
      method = "linear",
      rule = 2,
      ties = "ordered",
      na.rm = FALSE
    )$y
    yg[inserted] <- NA_real_ # real-gap interior -> missing, for interpolate()
    yg[on_sample] <- v[src[on_sample]] # keep genuine samples exact
    out[[col]] <- yg
  }

  n_inserted <- sum(inserted)
  pct <- round(100 * n_inserted / n_grid, 2)
  log_info(
    paste0(
      "Resampled onto a uniform grid{seg}: inserted {n_inserted} missing ",
      "sample(s) ({pct}%) at the expected {period} ms sampling period. Inserted ",
      "samples are flagged in `is_resampled` and left as `NA` for interpolation."
    ),
    verbose = verbose
  )

  out
}
