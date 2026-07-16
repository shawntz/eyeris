#' Load pupillometry data from a non-EyeLink eye tracker
#'
#' Construct a valid `eyeris` S3 object from standardized data frames so that
#' data from eye trackers other than SR Research EyeLink can enter the
#' `eyeris` preprocessing pipeline. While [eyeris::load_asc()] parses EyeLink
#' `.asc` files specifically, `load_generic()` provides a tracker-agnostic
#' ingestion path: you supply the raw samples (and, optionally, event messages,
#' gaze coordinates, and blink intervals) as plain R data frames, and
#' `load_generic()` assembles them into the same object structure that the rest
#' of `eyeris` (including [eyeris::glassbox()]) expects.
#'
#' @details
#' `eyeris` was designed to be extensible, but historically `load_asc()` was the
#' only implemented loader. `load_generic()` closes that gap. Native support for
#' specific tracker formats (e.g., Tobii, SMI, Pupil Labs, GazePoint) can be
#' layered on top of this function incrementally: a format-specific reader only
#' needs to produce the standardized data frames documented below and then call
#' `load_generic()`.
#'
#' The resulting object is structurally identical to one returned by
#' [eyeris::load_asc()], so it is a drop-in input to [eyeris::glassbox()] and the
#' individual preprocessing steps ([eyeris::deblink()], [eyeris::detransient()],
#' [eyeris::interpolate()], [eyeris::lpfilt()], [eyeris::downsample()],
#' [eyeris::bin()], [eyeris::detrend()], [eyeris::zscore()]),
#' [eyeris::epoch()], [eyeris::bidsify()], and the plotting methods.
#'
#' ## The three standardized data frames
#'
#' Following the conceptual model that most trackers export, `load_generic()`
#' accepts three core data frames (plus an optional fourth for gaze that is
#' exported separately):
#'
#' 1. **`pupil`** (required) -- the raw sample stream. Must contain a timestamp
#'    column and a pupil-size column. May *also* carry gaze coordinate columns
#'    (`eye_x`, `eye_y`) if your tracker exports samples as one wide table.
#' 2. **`events`** (optional) -- experimental event messages. Must contain a
#'    timestamp column (on the same clock as `pupil`) and a message-text column.
#'    Required only if you intend to epoch on event messages later.
#' 3. **`blinks`** (optional) -- blink intervals reported by the tracker. Must
#'    contain blink start and end timestamp columns. Note that blink padding via
#'    [eyeris::deblink()] does *not* depend on this table -- it reconstructs
#'    missing/blink regions directly from `NA` (and `0`) values in the pupil
#'    column -- so this table is purely for record-keeping and export.
#'
#' A fourth, **`gaze`**, data frame is accepted for the less common case where
#' gaze coordinates are exported separately from pupil size (timestamp + `x`/`y`
#' columns); it is joined onto `pupil` by timestamp. If your `pupil` data frame
#' already contains gaze columns, leave this `NULL`.
#'
#' If a column in your data frames does not use the default name expected by
#' `load_generic()`, remap it via the `mapping` argument (see below) rather than
#' renaming your data by hand.
#'
#' ## Column requirements and defaults
#'
#' By default `load_generic()` looks for these columns (override any of them with
#' `mapping`):
#'
#' \tabular{lll}{
#'   **data frame** \tab **role** \tab **default column** \cr
#'   `pupil`  \tab timestamp           \tab `time`  \cr
#'   `pupil`  \tab pupil size          \tab `pupil` \cr
#'   `pupil`  \tab gaze x (optional)   \tab `eye_x` \cr
#'   `pupil`  \tab gaze y (optional)   \tab `eye_y` \cr
#'   `pupil`  \tab block (optional)    \tab `block` \cr
#'   `events` \tab timestamp           \tab `time`  \cr
#'   `events` \tab message text        \tab `text`  \cr
#'   `blinks` \tab blink start         \tab `stime` \cr
#'   `blinks` \tab blink end           \tab `etime` \cr
#'   `gaze`   \tab timestamp           \tab `time`  \cr
#'   `gaze`   \tab gaze x              \tab `eye_x` \cr
#'   `gaze`   \tab gaze y              \tab `eye_y` \cr
#' }
#'
#' ## Handling tracker quirks
#'
#' `eyeris` assumes EyeLink-style regularly-sampled data. Two practical notes for
#' other trackers:
#'
#' * **Missing samples.** [eyeris::deblink()] reconstructs missing/blink regions
#'   directly from `NA` (and `0`) values in the pupil column -- it does *not*
#'   require the `blinks` table. If your tracker drops samples or encodes missing
#'   pupil data some other way, set those samples to `NA` in the `pupil`
#'   column so deblinking and the confound calculations behave correctly.
#' * **Irregular sampling.** If consecutive timestamps are not uniformly spaced,
#'   `load_generic()` emits a warning, because several downstream steps assume a
#'   fixed sampling interval. (A fuller guardrail is tracked separately.)
#'
#' @param pupil A data frame of raw samples. Must contain a timestamp column and
#'   a pupil-size column (see `mapping`). May optionally contain gaze coordinate
#'   and `block` columns.
#' @param events An optional data frame of event messages with a timestamp column
#'   (on the same clock as `pupil`) and a message-text column. If `NULL`
#'   (default), the object is created with empty event tables.
#' @param blinks An optional data frame of blink intervals with start and end
#'   timestamp columns. If `NULL` (default), empty blink tables are created.
#'   Note that blink padding via [eyeris::deblink()] does not depend on this
#'   table.
#' @param gaze An optional data frame of gaze coordinate samples (timestamp,
#'   `x`, `y`), used only when gaze is exported separately from pupil size. It is
#'   left-joined onto `pupil` by timestamp. If `NULL` (default), gaze is taken
#'   from `pupil` if those columns are present, otherwise filled with `NA`.
#' @param sample_rate Numeric sampling rate of the tracker in Hz. If `NULL`
#'   (default), it is inferred from the median spacing of the `pupil` timestamps;
#'   inference is reported and we recommend supplying the true rate explicitly.
#' @param time_unit Unit of all timestamp columns (in `pupil`, `events`, `gaze`,
#'   and `blinks`). Either `"ms"` (milliseconds, the default, matching EyeLink)
#'   or `"s"` (seconds). Timestamps are stored internally in milliseconds.
#' @param block Block specification, mirroring [eyeris::load_asc()]:
#'   * `"auto"` (default): if the `pupil` data contains a block column with more
#'     than one unique value, the data are split into multiple blocks;
#'     otherwise a single block (`block_1`) is created.
#'   * `NULL`: omit the block column and create a single block.
#'   * Numeric value: assign this block number to all samples.
#' @param eye Which eye the data correspond to: `"L"` (left, default), `"R"`
#'   (right), or `"LR"` (both, e.g., averaged). Recorded as metadata.
#' @param pupil_type Pupil measurement units: `"area"` (default) or
#'   `"diameter"`. Recorded as metadata (the `type` column).
#' @param screen_width,screen_height Optional screen dimensions in pixels, used
#'   for gaze heatmaps and gaze-based confounds. Leave as `NA` (default) if
#'   unknown; the gaze heatmap is simply skipped.
#' @param tracker Character label for the source tracker/system (default
#'   `"generic"`). Recorded in `info$version`.
#' @param model Optional character label for the specific tracker model.
#'   Recorded in `info$model`.
#' @param mapping An optional named list remapping the default column names to
#'   the ones present in your data frames. Recognized names are `time`, `pupil`,
#'   `eye_x`, `eye_y`, `text`, `stime`, `etime`, and `block`. For example,
#'   `mapping = list(time = "t_ms", pupil = "pup_size")`.
#' @param path Optional character path/identifier stored in the object's `file`
#'   slot (used in report titles). Defaults to the value of `tracker`.
#' @param verbose Logical. Whether to print verbose output (default `TRUE`).
#'
#' @return An object of S3 class `eyeris` with the same structure as
#' [eyeris::load_asc()]:
#' \enumerate{
#'   \item `file`: The `path` identifier for the source data.
#'   \item `timeseries`: A named list of per-block data frames of raw time series
#'   data (`time_orig`, `time_secs`, `time_scaled`, `eye_x`, `eye_y`, `eye`,
#'   `hz`, `type`, `pupil_raw`).
#'   \item `events`: A named list of per-block event-message data frames.
#'   \item `blinks`: A named list of per-block blink data frames.
#'   \item `info`: Tracker metadata (`sample.rate`, `mono`, `left`, `right`,
#'   `pupil.dtype`, `version`, `model`, `screen.x`, `screen.y`).
#'   \item `latest`: `eyeris` pointer for tracking pipeline run history.
#'   \item `binocular`, `binocular_mode`, `decimated.sample.rate`, `params`.
#' }
#'
#' @seealso [eyeris::load_asc()] for loading SR Research EyeLink `.asc` files.
#'
#' @seealso [eyeris::glassbox()] for running the full `eyeris` preprocessing
#' pipeline on the object returned by this function.
#'
#' @examples
#' # build three small standardized data frames from any non-EyeLink tracker
#' set.seed(1)
#' n <- 1000
#' samples <- data.frame(
#'   time = seq(0, by = 1, length.out = n), # 1000 Hz -> 1 ms spacing
#'   pupil = 1000 + cumsum(rnorm(n, 0, 5)),
#'   eye_x = 960 + rnorm(n, 0, 10),
#'   eye_y = 540 + rnorm(n, 0, 10)
#' )
#'
#' events <- data.frame(
#'   time = c(100, 500),
#'   text = c("TRIALID 1", "TRIALID 2")
#' )
#'
#' # construct a valid eyeris object
#' eye <- eyeris::load_generic(
#'   pupil = samples,
#'   events = events,
#'   sample_rate = 1000,
#'   screen_width = 1920,
#'   screen_height = 1080,
#'   tracker = "my-tracker"
#' )
#'
#' # ...and run it straight through the glassbox pipeline
#' eye |>
#'   eyeris::glassbox(lpfilt = list(plot_freqz = FALSE))
#'
#' @export
load_generic <- function(
  pupil,
  events = NULL,
  blinks = NULL,
  gaze = NULL,
  sample_rate = NULL,
  time_unit = c("ms", "s"),
  block = "auto",
  eye = c("L", "R", "LR"),
  pupil_type = c("area", "diameter"),
  screen_width = NA_real_,
  screen_height = NA_real_,
  tracker = "generic",
  model = NA_character_,
  mapping = NULL,
  path = NULL,
  verbose = TRUE
) {
  time_unit <- match.arg(time_unit)
  eye <- match.arg(eye)
  pupil_type <- match.arg(pupil_type)

  # column-name remapping ----------------------------------------------------
  default_mapping <- list(
    time = "time",
    pupil = "pupil",
    eye_x = "eye_x",
    eye_y = "eye_y",
    text = "text",
    stime = "stime",
    etime = "etime",
    block = "block"
  )
  mapping <- utils::modifyList(
    default_mapping,
    if (is.null(mapping)) list() else mapping
  )

  # validate and standardize the (required) pupil samples --------------------
  if (missing(pupil) || !is.data.frame(pupil)) {
    log_error("`pupil` must be a data frame of raw samples.")
  }
  pupil <- as.data.frame(pupil)

  require_col(pupil, mapping$time, "pupil", "timestamp")
  require_col(pupil, mapping$pupil, "pupil", "pupil-size")

  ms_scale <- if (time_unit == "s") 1000 else 1

  samples <- data.frame(
    time_orig = as.numeric(pupil[[mapping$time]]) * ms_scale,
    pupil_raw = as.numeric(pupil[[mapping$pupil]])
  )

  # carry over a block column from `pupil` if present -- assigned before the
  # gaze join so a separately-exported gaze table can be routed per block
  if (mapping$block %in% names(pupil)) {
    samples$block <- as.numeric(pupil[[mapping$block]])
  } else {
    samples$block <- 1
  }

  # gaze: prefer columns within `pupil`, else a separate `gaze` df, else NA ----
  if (mapping$eye_x %in% names(pupil) && mapping$eye_y %in% names(pupil)) {
    samples$eye_x <- as.numeric(pupil[[mapping$eye_x]])
    samples$eye_y <- as.numeric(pupil[[mapping$eye_y]])
  } else if (!is.null(gaze)) {
    gaze <- as.data.frame(gaze)
    require_col(gaze, mapping$time, "gaze", "timestamp")
    require_col(gaze, mapping$eye_x, "gaze", "gaze-x")
    require_col(gaze, mapping$eye_y, "gaze", "gaze-y")
    gaze_std <- data.frame(
      time_orig = as.numeric(gaze[[mapping$time]]) * ms_scale,
      eye_x = as.numeric(gaze[[mapping$eye_x]]),
      eye_y = as.numeric(gaze[[mapping$eye_y]])
    )
    # preserve gaze block identity: join on block + time when gaze carries a
    # block column, else fall back to timestamp -- but reject the timestamp-only
    # join when the pupil data spans multiple blocks, since routing is ambiguous
    if (mapping$block %in% names(gaze)) {
      gaze_std$block <- as.numeric(gaze[[mapping$block]])
      join_by <- c("block", "time_orig")
    } else {
      if (length(unique(samples$block)) > 1) {
        log_error(paste0(
          "`gaze` has no block column but `pupil` spans multiple blocks, so ",
          "joining gaze by timestamp alone is ambiguous. Add a block column to ",
          "`gaze` (see `mapping`) so gaze samples can be routed per block."
        ))
      }
      join_by <- "time_orig"
    }
    samples <- dplyr::left_join(
      samples,
      gaze_std,
      by = join_by,
      relationship = "many-to-one"
    )
  } else {
    # gaze not available: keep columns present (all-NA) for downstream safety
    samples$eye_x <- NA_real_
    samples$eye_y <- NA_real_
  }

  # guarantee monotonic, non-decreasing time per block (required downstream) --
  ord <- order(samples$block, samples$time_orig)
  if (is.unsorted(ord)) {
    log_warn(
      "Reordering samples by block and timestamp to ensure monotonic time.",
      verbose = verbose
    )
  }
  samples <- samples[ord, , drop = FALSE]
  rownames(samples) <- NULL

  # sampling rate ------------------------------------------------------------
  hz <- resolve_sample_rate(sample_rate, samples$time_orig, verbose)

  # assemble the canonical raw_df (column order mirrors process_eyeris_data) --
  raw_df <- samples |>
    dplyr::select(block, time_orig, pupil_raw, eye_x, eye_y) |>
    dplyr::mutate(eye = eye, hz = hz, type = pupil_type) |>
    dplyr::relocate(pupil_raw, .after = type)

  # standardize events -------------------------------------------------------
  events_df <- standardize_events(events, mapping, ms_scale)

  # standardize blinks -------------------------------------------------------
  blinks_df <- standardize_blinks(blinks, mapping, ms_scale)

  # ensure block columns on events/blinks for splitting ----------------------
  events_df <- ensure_block_col(events_df, "time", raw_df)
  blinks_df <- ensure_block_col(blinks_df, "stime", raw_df)

  # split into blocks (mirrors load_asc semantics) ---------------------------
  assembled <- assemble_generic_blocks(raw_df, events_df, blinks_df, block)

  list_out <- vector("list", length = 8)
  names(list_out) <- c(
    "file",
    "timeseries",
    "events",
    "blinks",
    "info",
    "latest",
    "binocular",
    "binocular_mode"
  )

  list_out$timeseries <- assembled$timeseries
  list_out$events <- add_unique_event_identifiers(assembled$events)
  list_out$blinks <- assembled$blinks

  # metadata -----------------------------------------------------------------
  list_out$file <- if (is.null(path)) tracker else path
  list_out$info <- list(
    sample.rate = hz,
    mono = eye != "LR",
    left = grepl("L", eye),
    right = grepl("R", eye),
    pupil.dtype = pupil_type,
    version = tracker,
    model = model,
    screen.x = screen_width,
    screen.y = screen_height
  )
  list_out$binocular <- FALSE
  list_out$binocular_mode <- NULL

  # latest pointer (mirror process_eyeris_data) ------------------------------
  if (is.list(list_out$timeseries) && !is.data.frame(list_out$timeseries)) {
    list_out$latest <- setNames(
      as.list(rep("pupil_raw", length(list_out$timeseries))),
      names(list_out$timeseries)
    )
  } else {
    list_out$latest <- "pupil_raw"
  }

  list_out$decimated.sample.rate <- NA_integer_
  list_out$params <- list(
    load_generic = list(
      call = match.call(),
      parameters = list(
        sample_rate = hz,
        time_unit = time_unit,
        block = block,
        eye = eye,
        pupil_type = pupil_type,
        tracker = tracker
      )
    )
  )

  list_out <- normalize_time_orig(list_out)
  class(list_out) <- "eyeris"

  # heads-up if sampling looks irregular (downstream assumes uniform spacing) -
  warn_irregular_sampling(list_out$timeseries, verbose)

  log_info(
    paste0(
      "Loaded generic '",
      tracker,
      "' data: ",
      length(list_out$timeseries),
      " block(s), ",
      hz,
      " Hz."
    ),
    verbose = verbose
  )

  list_out
}

#' Standardize a user-supplied events data frame
#'
#' Coerces an optional events data frame to the canonical `time`/`text` columns
#' used internally by `eyeris`, scaling timestamps to milliseconds.
#'
#' @param events Optional events data frame (or `NULL`).
#' @param mapping Resolved column-name mapping list.
#' @param ms_scale Numeric factor to convert timestamps to milliseconds.
#'
#' @return A data frame with `time` and `text` columns (zero-row if `events` is
#' `NULL`).
#'
#' @keywords internal
standardize_events <- function(events, mapping, ms_scale) {
  if (is.null(events)) {
    return(data.frame(
      time = numeric(0),
      text = character(0),
      stringsAsFactors = FALSE
    ))
  }

  events <- as.data.frame(events)
  require_col(events, mapping$time, "events", "timestamp")
  require_col(events, mapping$text, "events", "message-text")

  out <- data.frame(
    time = as.numeric(events[[mapping$time]]) * ms_scale,
    text = as.character(events[[mapping$text]]),
    stringsAsFactors = FALSE
  )

  # retain an explicit block assignment when supplied, so events are routed by
  # block identity rather than re-derived from timestamp ranges
  if (mapping$block %in% names(events)) {
    out$block <- as.numeric(events[[mapping$block]])
  }

  out
}

#' Standardize a user-supplied blinks data frame
#'
#' Coerces an optional blinks data frame to the canonical `stime`/`etime`
#' columns used internally by `eyeris`, scaling timestamps to milliseconds.
#'
#' @param blinks Optional blinks data frame (or `NULL`).
#' @param mapping Resolved column-name mapping list.
#' @param ms_scale Numeric factor to convert timestamps to milliseconds.
#'
#' @return A data frame with `stime` and `etime` columns (zero-row if `blinks`
#' is `NULL`).
#'
#' @keywords internal
standardize_blinks <- function(blinks, mapping, ms_scale) {
  if (is.null(blinks)) {
    return(data.frame(stime = numeric(0), etime = numeric(0)))
  }

  blinks <- as.data.frame(blinks)
  require_col(blinks, mapping$stime, "blinks", "blink-start")
  require_col(blinks, mapping$etime, "blinks", "blink-end")

  out <- data.frame(
    stime = as.numeric(blinks[[mapping$stime]]) * ms_scale,
    etime = as.numeric(blinks[[mapping$etime]]) * ms_scale
  )

  # retain an explicit block assignment when supplied, so blinks are routed by
  # block identity rather than re-derived from timestamp ranges
  if (mapping$block %in% names(blinks)) {
    out$block <- as.numeric(blinks[[mapping$block]])
  }

  out
}

#' Ensure an events/blinks data frame carries a block column
#'
#' Adds a `block` column to an events or blinks data frame so that it can be
#' split into the same per-block structure as the time series. If the data
#' span a single block, every row is assigned to that block; otherwise rows are
#' assigned to whichever block's timestamp range contains them. Routing fails
#' (with an informative error) when a timestamp matches no block or falls within
#' more than one overlapping block range -- supply an explicit `block` column to
#' resolve the ambiguity.
#'
#' @param df An events or blinks data frame.
#' @param time_col Name of the timestamp column to match against block ranges.
#' @param raw_df The assembled timeseries data frame (with a `block` column).
#'
#' @return `df` with a `block` column added (if not already present).
#'
#' @keywords internal
ensure_block_col <- function(df, time_col, raw_df) {
  if ("block" %in% names(df)) {
    return(df)
  }

  blocks <- sort(unique(raw_df$block))

  if (nrow(df) == 0) {
    df$block <- numeric(0)
    return(df)
  }

  if (length(blocks) == 1) {
    df$block <- blocks
    return(df)
  }

  ranges <- lapply(blocks, function(b) {
    range(raw_df$time_orig[raw_df$block == b], na.rm = TRUE)
  })

  df$block <- vapply(
    df[[time_col]],
    function(t) {
      matches <- if (is.na(t)) {
        integer(0)
      } else {
        which(vapply(ranges, function(r) t >= r[1] && t <= r[2], logical(1)))
      }
      if (length(matches) == 1L) {
        return(blocks[matches])
      }
      if (length(matches) == 0L) {
        log_error(paste0(
          "An events/blinks timestamp (",
          t,
          ") falls outside every block's time range, so it cannot be routed ",
          "to a block. Supply an explicit `block` column (see `mapping`) to ",
          "assign rows to blocks."
        ))
      }
      log_error(paste0(
        "An events/blinks timestamp (",
        t,
        ") falls within multiple overlapping block time ranges, so block ",
        "routing is ambiguous. Supply an explicit `block` column (see ",
        "`mapping`) to assign rows to blocks."
      ))
    },
    numeric(1)
  )

  df
}

#' Split standardized data frames into per-block eyeris structures
#'
#' Mirrors the block-handling logic of `process_eyeris_data()` for the generic
#' loader, producing the named per-block lists for time series, events, and
#' blinks.
#'
#' @param raw_df Assembled timeseries data frame with a `block` column.
#' @param events_df Standardized events data frame with a `block` column.
#' @param blinks_df Standardized blinks data frame with a `block` column.
#' @param block Block specification (`"auto"`, `NULL`, or numeric).
#'
#' @return A list with `timeseries`, `events`, and `blinks` named per-block
#' lists.
#'
#' @keywords internal
assemble_generic_blocks <- function(raw_df, events_df, blinks_df, block) {
  out <- list()

  if (!is.null(block)) {
    if (identical(block, "auto")) {
      existing_blocks <- unique(raw_df$block)
      if (length(existing_blocks) > 1) {
        out$timeseries <- split(raw_df, paste0("block_", raw_df$block))
        out$events <- split(events_df, paste0("block_", events_df$block))
        out$blinks <- split(blinks_df, paste0("block_", blinks_df$block))
      } else {
        out$timeseries <- list("block_1" = raw_df)
        out$events <- list("block_1" = events_df)
        out$blinks <- list("block_1" = blinks_df)
      }
    } else if (is.numeric(block)) {
      if (length(block) != 1 || !is.finite(block)) {
        log_error("`block` must be one finite numeric value.")
      }
      bn <- paste0("block_", as.character(block))
      out$timeseries <- setNames(
        list(raw_df |> dplyr::mutate(block = !!as.numeric(block))),
        bn
      )
      out$events <- setNames(
        list(events_df |> dplyr::mutate(block = !!as.numeric(block))),
        bn
      )
      out$blinks <- setNames(
        list(blinks_df |> dplyr::mutate(block = !!as.numeric(block))),
        bn
      )
    } else {
      log_error("`block` must be either: NULL, numeric, or 'auto'.")
    }
  } else {
    # single block, omit the block column from all tables
    out$timeseries <- list("block_1" = raw_df |> dplyr::select(-block))
    out$events <- list(
      "block_1" = events_df |> dplyr::select(-dplyr::any_of("block"))
    )
    out$blinks <- list(
      "block_1" = blinks_df |> dplyr::select(-dplyr::any_of("block"))
    )
  }

  out
}

#' Resolve the sampling rate for a generic load
#'
#' Returns the user-supplied sampling rate, or infers it from the median spacing
#' of the timestamps when not provided.
#'
#' @param sample_rate User-supplied sampling rate in Hz, or `NULL`.
#' @param time_ms Numeric vector of timestamps in milliseconds.
#' @param verbose Logical. Whether to print verbose output.
#'
#' @return A positive numeric sampling rate in Hz.
#'
#' @keywords internal
resolve_sample_rate <- function(sample_rate, time_ms, verbose) {
  if (!is.null(sample_rate)) {
    if (
      !is.numeric(sample_rate) ||
        length(sample_rate) != 1 ||
        !is.finite(sample_rate) ||
        sample_rate <= 0
    ) {
      log_error("`sample_rate` must be a single positive number (in Hz).")
    }
    return(sample_rate)
  }

  deltas <- diff(time_ms[!is.na(time_ms)])
  deltas <- deltas[deltas > 0]
  if (length(deltas) < 1) {
    log_error(
      "Unable to infer `sample_rate` from timestamps; please supply it explicitly (in Hz)."
    )
  }

  hz <- round(1000 / stats::median(deltas))
  log_warn(
    paste0(
      "`sample_rate` not provided; inferred ",
      hz,
      " Hz from the median ",
      "timestamp spacing. We recommend passing the true rate explicitly."
    ),
    verbose = verbose
  )
  hz
}

#' Warn when sampling intervals are not uniform
#'
#' Several `eyeris` steps assume a fixed sampling interval (an EyeLink-style
#' quirk). This emits a soft warning when consecutive timestamps within a block
#' are not uniformly spaced, which can indicate dropped samples.
#'
#' @param timeseries A named list of per-block time series data frames.
#' @param verbose Logical. Whether to print verbose output.
#'
#' @return Invisibly `NULL`; called for its side effect (warning).
#'
#' @keywords internal
warn_irregular_sampling <- function(timeseries, verbose) {
  for (bn in names(timeseries)) {
    t <- timeseries[[bn]]$time_orig
    deltas <- diff(t[!is.na(t)])
    if (length(deltas) < 2) {
      next
    }
    md <- stats::median(deltas)
    if (md > 0 && mean(abs(deltas - md) > 0.5 * md) > 0.01) {
      log_warn(
        paste0(
          "Non-uniform sampling intervals detected in ",
          bn,
          ". Several ",
          "preprocessing steps assume a fixed sampling rate; if your tracker ",
          "drops samples, consider resampling onto a uniform grid first."
        ),
        verbose = verbose
      )
    }
  }
  invisible(NULL)
}

#' Assert that a required column exists in an input data frame
#'
#' @param df The data frame to check.
#' @param col The required column name.
#' @param df_name The name of the data frame (for the error message).
#' @param role A human-readable description of the column's role.
#'
#' @return No return value; throws an error if the column is missing.
#'
#' @keywords internal
require_col <- function(df, col, df_name, role) {
  if (!col %in% names(df)) {
    log_error(paste0(
      "The `",
      df_name,
      "` data frame must contain a ",
      role,
      " column named '",
      col,
      "'. Use `mapping` to point eyeris at a differently-named column."
    ))
  }
}
