# Pure adapters for saved eyeris objects. Scientific processing stays in eyeris.
review_objects <- function(x) {
  if (inherits(x, "eyeris") && any(grepl("^epoch_", names(x)))) return(list(main = x))
  # load_asc(binocular_mode = "both") stores the processed eyes here.
  candidates <- if (all(c("left", "right") %in% names(x))) x else if (is.list(x$raw_binocular_object)) x$raw_binocular_object else x
  eyes <- candidates[intersect(c("left", "right"), names(candidates))]
  if (length(eyes) && all(vapply(eyes, inherits, logical(1), "eyeris"))) return(eyes)
  stop("Choose an RDS containing an epoched eyeris object. Run eyeris::epoch() before saving it.")
}

# Epoch-level variables, such as event-pattern placeholders ({trial}, {stim})
# and matched_event: columns other than signals and timing whose value is the
# same at an epoch's first and last sample.
review_field_columns <- function(df) {
  skip <- c("timebin", "block", "eye", "hz", "type", "text_unique", "template", "matching_pattern")
  setdiff(names(df)[!grepl("^(pupil_|time|eye_)", names(df))], skip)
}
review_fields <- function(df, a, b, columns = review_field_columns(df)) {
  out <- list()
  for (col in columns) {
    first <- df[[col]][a]
    if (length(first) == 1 && !is.na(first) && identical(first, df[[col]][b]))
      out[[col]] <- as.character(first)
  }
  if (length(out)) out else structure(list(), names = character())
}

review_index <- function(x) {
  objects <- review_objects(x)
  out <- list()
  for (eye in names(objects)) {
    object <- objects[[eye]]
    # bidsify() names runs by block unless the recording has exactly one block.
    blocks <- if (is.list(object$timeseries)) length(object$timeseries) else NA_integer_
    for (label in grep("^epoch_", names(object), value = TRUE)) {
      for (block in names(object[[label]])) {
        df <- object[[label]][[block]]
        if (!is.data.frame(df) || !nrow(df)) next
        stages <- names(df)[grepl("^pupil_", names(df)) & vapply(df, is.numeric, logical(1))]
        field_columns <- review_field_columns(df)
        if (!length(stages) || !is.numeric(df$timebin) || any(!is.finite(df$timebin))) {
          stop(sprintf("%s/%s must contain numeric pupil_* stages and finite timebin values.", label, block))
        }
        # Repeated event messages and overlapping windows are distinct epochs.
        starts <- c(1L, which(diff(df$timebin) <= 0) + 1L)
        ends <- c(starts[-1L] - 1L, nrow(df))
        for (i in seq_along(starts)) {
          a <- starts[i]; b <- ends[i]
          value <- function(cols, fallback = "") {
            for (col in cols) if (col %in% names(df) && !is.na(df[[col]][a])) return(as.character(df[[col]][a]))
            fallback
          }
          final <- tail(stages, 1)
          ys <- df[[final]][a:b]
          # Missing samples per stored stage, for automatic exclusion rules.
          stage_missing <- lapply(stages, function(s) mean(!is.finite(df[[s]][a:b])))
          names(stage_missing) <- stages
          out[[length(out) + 1L]] <- list(
            key = paste(eye, label, block, a, b, sep = "/"), eye = eye,
            label = label, block = block, start = a, end = b, ordinal = i,
            trial = value(c("trial", "start_trial"), as.character(i)),
            event = value(c("matched_event", "start_matched_event", "start_msg", "text_unique"), paste("Epoch", i)),
            stages = unname(as.list(stages)), finalStage = final,
            samples = b - a + 1L, duration = df$timebin[b] - df$timebin[a],
            missing = mean(!is.finite(ys)), stageMissing = stage_missing, blocks = blocks,
            fields = review_fields(df, a, b, field_columns),
            limits = object[[label]]$info[[block]]$epoch_limits
          )
        }
      }
    }
  }
  if (!length(out)) stop("No nonempty epoch tables found in this RDS.")
  out
}

# Recompute missing fractions per stage for epochs indexed before they were stored.
review_missing <- function(x, epochs) {
  lapply(epochs, function(epoch) {
    df <- review_frame(x, epoch)
    stages <- unlist(epoch$stages)
    out <- lapply(stages, function(s) mean(!is.finite(df[[s]])))
    names(out) <- stages
    out
  })
}

# Average epochs on a shared time grid, for run diagnostics. Each epoch is
# sampled at the grid's nearest stored sample, so missing samples stay missing
# instead of being interpolated. Returns every epoch's trace, and the mean,
# standard error and number of finite values at each time.
review_average <- function(x, epochs, stage, points = 600L) {
  frames <- lapply(epochs, function(e) review_frame(x, e))
  t0 <- frames[[1]]$timebin
  grid <- seq(min(t0), max(t0), length.out = min(length(t0), as.integer(points)))
  traces <- vapply(frames, function(df) {
    t <- df$timebin
    if (!stage %in% names(df)) stop("An epoch does not contain the selected stage.")
    nearest <- round(stats::approx(t, seq_along(t), grid, rule = 2, ties = "ordered")$y)
    y <- df[[stage]][nearest]
    y[!is.finite(y) | grid < min(t) | grid > max(t)] <- NA_real_
    y
  }, numeric(length(grid)))
  traces <- matrix(traces, nrow = length(grid))
  n <- rowSums(is.finite(traces))
  mean <- ifelse(n > 0, rowSums(traces, na.rm = TRUE) / pmax(n, 1), NA_real_)
  sd <- apply(traces, 1, stats::sd, na.rm = TRUE)
  se <- ifelse(n > 1, sd / sqrt(n), NA_real_)
  list(
    time = unname(as.list(grid)),
    traces = lapply(seq_len(ncol(traces)), function(i) unname(as.list(traces[, i]))),
    mean = unname(as.list(mean)), se = unname(as.list(se)), n = unname(as.list(n))
  )
}

# Epoch fields for epochs indexed before they were stored.
review_epoch_fields <- function(x, epochs) {
  lapply(epochs, function(epoch) {
    df <- review_frame(x, epoch)
    review_fields(df, 1L, nrow(df))
  })
}

# Per-group means, sums of squared deviations and counts of finite values on a
# time grid (seconds from each epoch's start), updated one epoch at a time
# (Welford), so groups can be pooled across sources without losing precision.
review_group_moments <- function(x, epochs, stage, grid, groups) {
  grid <- unlist(grid)
  means <- matrix(0, length(grid), groups)
  m2 <- means
  counts <- means
  for (e in epochs) {
    df <- review_frame(x, e)
    if (!stage %in% names(df)) stop("An epoch does not contain the selected stage.")
    t <- df$timebin - df$timebin[1]
    nearest <- round(stats::approx(t, seq_along(t), grid, rule = 2, ties = "ordered")$y)
    y <- df[[stage]][nearest]
    ok <- is.finite(y) & grid <= max(t)
    g <- e$group
    counts[ok, g] <- counts[ok, g] + 1
    delta <- y[ok] - means[ok, g]
    means[ok, g] <- means[ok, g] + delta / counts[ok, g]
    m2[ok, g] <- m2[ok, g] + delta * (y[ok] - means[ok, g])
  }
  columns <- function(m) lapply(seq_len(groups), function(g) unname(as.list(m[, g])))
  list(mean = columns(means), m2 = columns(m2), n = columns(counts))
}

# Each epoch's values of one stage on a time grid (seconds from the epoch's
# start), using the nearest sample as review_group_moments() does, and NA where
# the value is missing or the epoch is shorter than the grid. The app caches
# these to average epochs in any grouping without reading the source again.
review_traces <- function(x, epochs, stage, grid) {
  grid <- unlist(grid)
  objects <- review_objects(x)
  lapply(epochs, function(e) {
    df <- objects[[e$eye]][[e$label]][[e$block]]
    if (is.null(df) || e$start < 1 || e$end > nrow(df)) stop("Epoch locator is no longer valid.")
    if (!stage %in% names(df)) stop("An epoch does not contain the selected stage.")
    rows <- seq.int(e$start, e$end)
    t <- df$timebin[rows]
    t <- t - t[1]
    nearest <- round(stats::approx(t, seq_along(t), grid, rule = 2, ties = "ordered")$y)
    y <- df[[stage]][rows][nearest]
    y[!is.finite(y) | grid > max(t)] <- NA_real_
    y
  })
}

review_frame <- function(x, epoch) {
  df <- review_objects(x)[[epoch$eye]][[epoch$label]][[epoch$block]]
  if (is.null(df) || epoch$start < 1 || epoch$end > nrow(df)) stop("Epoch locator is no longer valid.")
  df[seq.int(epoch$start, epoch$end), , drop = FALSE]
}

# Preserve local extrema and both sides of every missing-data boundary.
# The cap is soft: preserving real gaps is more important than a strict point cap.
review_reduce <- function(t, y, budget = 5000L) {
  n <- length(t)
  if (n <= budget) return(seq_len(n))
  buckets <- split(seq_len(n), ceiling(seq_len(n) / ceiling(n / (budget / 4))))
  keep <- unlist(lapply(buckets, function(ii) {
    finite <- ii[is.finite(y[ii])]
    c(head(ii, 1), tail(ii, 1), if (length(finite)) c(finite[which.min(y[finite])], finite[which.max(y[finite])]))
  }), use.names = FALSE)
  gaps <- which(diff(is.finite(y)) != 0)
  sort(unique(c(keep, gaps, gaps + 1L)))
}

review_trace <- function(x, epoch, stage, range = NULL) {
  df <- review_frame(x, epoch)
  if (!stage %in% unlist(epoch$stages)) stop("Unknown preprocessing stage.")
  t <- df$timebin; y <- df[[stage]]
  # Fully qualified because range is also a request parameter.
  domain <- base::range(t)
  if (!is.null(range)) {
    if (length(range) != 2 || any(!is.finite(unlist(range))) || range[[1]] >= range[[2]]) stop("Invalid plot range.")
    keep <- which(t >= range[[1]] & t <= range[[2]])
    if (length(keep)) keep <- seq.int(max(1L, min(keep) - 1L), min(length(t), max(keep) + 1L))
    t <- t[keep]; y <- y[keep]
  }
  ii <- review_reduce(t, y)
  list(time = unname(as.list(t[ii])), signal = unname(as.list(y[ii])),
       domain = unname(as.list(domain)), samples = nrow(df), displayed = length(ii),
       missing = mean(!is.finite(df[[stage]])), stage = stage)
}

# Write each table to <status>/<file>.rds and .csv. `file` is a relative path
# chosen by the app; only statuses with epochs are written.
review_export_source <- function(x, epochs, destination) {
  groups <- split(epochs, vapply(epochs, function(e) paste(e$status, e$file, sep = "\r"), character(1)))
  for (group in groups) {
    frames <- lapply(group, function(e) {
      df <- review_frame(x, e)
      if (".review_epoch_id" %in% names(df)) stop("Reserved column .review_epoch_id already exists.")
      df$.review_epoch_id <- e$id
      df
    })
    df <- do.call(rbind, frames)
    folder <- switch(group[[1]]$status, keep = "retained", exclude = "excluded", unreviewed = "unreviewed")
    file <- file.path(destination, folder, group[[1]]$file)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(df, paste0(file, ".rds"))
    utils::write.csv(df, paste0(file, ".csv"), row.names = FALSE, na = "")
  }
  TRUE
}
