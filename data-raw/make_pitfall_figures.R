# ==============================================================================
# make_pitfall_figures.R
#
# Standalone, reproducible generator for six single-aspect preprocessing-pitfall
# figures built on the `eyeris` seeded pupil simulator. Each figure is a stacked
# 3x1 panel -- context (raw) -> SUBOPTIMAL -> OPTIMAL -- that isolates one
# mistake for one preprocessing step:
#
#   1. deblink     -- an over-aggressive NA mask that "bleeds over" real signal
#   2. interpolate -- linearly bridging a gap that is far too long
#   3. lpfilt      -- a rectangular (FFT brick-wall) filter instead of Butterworth
#   4. detransient -- low-pass filtering BEFORE removing transients
#   5. detrend     -- a linear detrend on a curved (exponential-decay) drift,
#                     vs a spline detrend, seen through z-scored epoch means
#   6. zscore      -- comparing two blocks with different mean/variance without
#                     per-block standardization
#
# Usage (from the package root):
#   Rscript data-raw/make_pitfall_figures.R
# or, interactively:
#   source("data-raw/make_pitfall_figures.R")
#
# Spline detrending (figure 5) is computed INLINE here rather than via a package
# `detrend(method = "spline")`, which is being added in a separate PR. The inline
# formula mirrors that PR exactly:
#   spline_fit <- lm(pupil ~ splines::ns(time, df = 5)); resid <- pupil - predict
# ==============================================================================

suppressWarnings(suppressMessages({
  if (
    requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")
  ) {
    devtools::load_all(quiet = TRUE)
  } else {
    library(eyeris)
  }
}))

# ---- configuration ----------------------------------------------------------

out_dir <- file.path("man", "figures", "pitfalls")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fig_w <- 7 # inches
fig_h <- 8.5 # inches
fig_res <- 300 # dpi

RED <- "#C0271F"
GREEN <- "#1A7F37"
GREY <- "grey45"
BLUE <- "#1F5FA8"

# ---- shared helpers ---------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

.quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# single-block accessors (the simulator emits one block: "block_1")
.blk <- function(o) o$timeseries[["block_1"]]
.latest_name <- function(o) {
  lt <- o$latest
  if (is.list(lt)) lt[["block_1"]] else lt
}
.latest_col <- function(o) .blk(o)[[.latest_name(o)]]

# translucent full-panel wash to code a panel wrong (red) / right (green)
tint_bg <- function(color, alpha = 0.07) {
  usr <- graphics::par("usr")
  graphics::rect(
    usr[1],
    usr[3],
    usr[2],
    usr[4],
    col = grDevices::adjustcolor(color, alpha.f = alpha),
    border = NA
  )
}

# contiguous runs of TRUE in a logical vector -> list(start, end) index pairs
.true_runs <- function(mask) {
  mask[is.na(mask)] <- FALSE
  if (!any(mask)) {
    return(list())
  }
  d <- diff(c(FALSE, mask, FALSE))
  starts <- which(d == 1)
  ends <- which(d == -1) - 1L
  Map(function(s, e) c(s, e), starts, ends)
}

# shade the x-spans of masked (NA) samples within a plotted window
shade_na_runs <- function(t, mask, color, alpha = 0.22) {
  usr <- graphics::par("usr")
  for (run in .true_runs(mask)) {
    x0 <- t[run[1]]
    x1 <- t[run[2]]
    graphics::rect(
      x0,
      usr[3],
      x1,
      usr[4],
      col = grDevices::adjustcolor(color, alpha.f = alpha),
      border = NA
    )
  }
}

# open a 3x1 device with the shared showcase layout
open_panel <- function(path) {
  grDevices::png(path, width = fig_w, height = fig_h, units = "in", res = fig_res)
  graphics::par(
    mfrow = c(3, 1),
    mar = c(3.4, 4.2, 2.6, 1),
    oma = c(1, 0, 3.6, 0),
    mgp = c(2.2, 0.7, 0),
    cex.main = 1.0 # the "SUBOPTIMAL/OPTIMAL PROCESSING" labels are long
  )
}

close_panel <- function(title, subtitle = NULL) {
  lab <- title
  if (!is.null(subtitle)) {
    lab <- paste0(title, "\n", subtitle)
  }
  graphics::mtext(lab, outer = TRUE, cex = 1.05, font = 2, line = 0.4)
  grDevices::dev.off()
}

# a green-boxed "RIGHT" panel and a red-boxed "WRONG" panel
box_wrong <- function() graphics::box(col = RED, lwd = 3)
box_right <- function() graphics::box(col = GREEN, lwd = 3)
box_ctx <- function() graphics::box(col = "grey65")

# window subset of a block on time_secs
win_sel <- function(b, win) {
  sel <- b$time_secs >= win[1] & b$time_secs <= win[2]
  if (sum(sel, na.rm = TRUE) < 2) sel <- rep(TRUE, nrow(b))
  sel
}

# inject a run of missing samples into the base pupil_raw column
inject_gap <- function(o, t0, t1) {
  b <- .blk(o)
  sel <- b$time_secs >= t0 & b$time_secs <= t1
  b[["pupil_raw"]][sel] <- NA_real_
  o$timeseries[["block_1"]] <- b
  o
}

lp <- function(o) .quiet(lpfilt(o, plot_freqz = FALSE))

msg <- function(...) cat(sprintf(...), "\n")

# ==============================================================================
# 1. deblink -- too short a mask leaves the occlusion flanks
# ==============================================================================
fig_deblink <- function(path) {
  params <- sim_params(
    duration_secs = 30,
    transients = FALSE,
    dropout_frac = 0, # blinks are the only missing data, so the metric is clean
    n_blinks = 6,
    blink_flank_ms = 40 # occlusion artifacts span 40 ms on each side of the core
  )
  sim <- simulate_eyeris(seed = 1, params = params, verbose = FALSE)
  truth <- attr(sim, "sim_truth")
  ts <- truth$time_secs

  # center on the first blink
  i <- which(truth$core_mask)[1]
  win <- c(max(0, ts[i] - 0.8), min(truth$params$duration_secs, ts[i] + 0.8))

  # too-SHORT a mask (5 ms) leaves the 40 ms occlusion flanks behind; a 50 ms
  # extend clears the 40 ms flanks with a little room to spare
  wrong <- .quiet(deblink(sim, extend = 5))
  right <- .quiet(deblink(sim, extend = 50))

  b <- .blk(sim)
  sel <- win_sel(b, win)
  t <- b$time_secs[sel]
  raw <- b$pupil_raw[sel]
  r_col <- .latest_col(right)[sel]
  w_col <- .latest_col(wrong)[sel]
  ylim <- range(c(raw, r_col, w_col), na.rm = TRUE)

  open_panel(path)

  # panel 1: raw with the blink (missing core + occlusion flank spikes)
  plot(t, raw,
    type = "l", col = GREY, lwd = 1.4, ylim = ylim,
    xlab = "", ylab = "pupil (a.u.)",
    main = "1) RAW INPUT  -  blink: missing core + occlusion flank spikes"
  )
  box_ctx()

  # panel 2: SUBOPTIMAL -- extend = 5 ms leaves the occlusion flank spikes
  plot(
    t,
    w_col,
    type = "n",
    ylim = ylim,
    xlab = "",
    ylab = "pupil (a.u.)",
    main = "2) SUBOPTIMAL PROCESSING  -  deblink(extend = 5): flank artifacts survive",
    col.main = RED, font.main = 2
  )
  tint_bg(RED)
  graphics::abline(h = truth$params$baseline_mean, col = "grey55", lty = 3)
  shade_na_runs(t, is.na(w_col), RED, alpha = 0.22)
  graphics::lines(t, w_col, col = RED, lwd = 2.4)
  graphics::text(
    ts[i],
    min(w_col, na.rm = TRUE),
    labels = "occlusion flanks\nnot removed",
    col = RED,
    font = 2,
    cex = 0.85,
    adj = c(0.5, -0.35)
  )
  box_wrong()

  # panel 3: OPTIMAL -- extend = 50 ms clears the 40 ms flanks (plus a little)
  plot(t, r_col,
    type = "n", ylim = ylim, xlab = "time (s)", ylab = "pupil (a.u.)",
    main = "3) OPTIMAL PROCESSING  -  deblink(extend = 50): clears the 40 ms flanks (+ a bit extra)",
    col.main = GREEN, font.main = 2
  )
  tint_bg(GREEN)
  graphics::abline(h = truth$params$baseline_mean, col = "grey55", lty = 3)
  shade_na_runs(t, is.na(r_col), GREEN, alpha = 0.22)
  graphics::lines(t, r_col, col = GREEN, lwd = 2.4)
  box_right()

  # metric: how large an occlusion-flank artifact remains in the kept signal.
  # extend = 5 ms leaves most of the 40 ms flank; extend = 50 ms removes it all.
  flank <- truth$region_mask & !truth$core_mask
  base_lvl <- truth$params$baseline_mean
  fl_resid <- function(o) {
    r <- abs(.latest_col(o)[flank] - base_lvl)
    if (all(is.na(r))) 0 else max(r, na.rm = TRUE)
  }
  resid_wrong <- fl_resid(wrong)
  resid_right <- fl_resid(right)
  close_panel(
    "eyeris pitfall - deblink: too short a mask leaves the occlusion flanks",
    sprintf(
      "occlusion-flank artifact remaining:  suboptimal = %.0f a.u.,  optimal = %.0f a.u.",
      resid_wrong, resid_right
    )
  )
  msg(
    "  [deblink]  flank artifact remaining: suboptimal=%.0f optimal=%.0f",
    resid_wrong, resid_right
  )
  invisible(path)
}

# ==============================================================================
# 2. interpolate -- a long mid-trial dropout cannot be honestly bridged
# ==============================================================================
fig_interpolate <- function(path) {
  # one trial that loses > 250 ms of pupil mid-way (e.g. a large gaze shift or
  # movement) and whose surviving ends sit at DIFFERENT levels: it begins high
  # and ends low. Nothing is known about the signal during the gap, so a
  # straight interpolation line fabricates data -- the gap should stay NA.
  params <- sim_params(
    duration_secs = 6,
    baseline_mean = 5000,
    blinks = FALSE,
    transients = FALSE,
    drift = FALSE,
    hippus = FALSE,
    phasic = FALSE,
    dropout_frac = 0,
    noise_sd = 5
  )
  sim <- simulate_eyeris(seed = 7, params = params, verbose = FALSE)

  gap0 <- 1.8
  gap1 <- 4.2 # 2.4 s of contiguous data lost (40% of a 6 s trial)

  # a single dilation waveform (a smooth rise-and-fall bump) whose PEAK falls
  # inside the gap: the surviving start shows the rising limb, the surviving end
  # shows the falling limb -- and the falling limb is shifted DOWN (a baseline
  # change from the movement) so the two ends no longer line up
  b <- .blk(sim)
  ts <- b$time_secs
  bump <- 470 * exp(-((ts - 3.0)^2) / (2 * 1.35^2)) # dilation centered in the gap
  b[["pupil_raw"]] <- b[["pupil_raw"]] + bump
  b[["pupil_raw"]][ts >= gap1] <- b[["pupil_raw"]][ts >= gap1] - 300 # post-gap shift
  sim$timeseries[["block_1"]] <- b

  gapped <- inject_gap(sim, gap0, gap1)
  interp <- .quiet(interpolate(gapped, verbose = FALSE))

  b <- .blk(gapped)
  t <- b$time_secs
  raw <- b[["pupil_raw"]]
  filled <- .latest_col(interp)
  ylim <- range(c(raw, filled), na.rm = TRUE)
  gap_mask <- t > gap0 & t < gap1
  bridge_sel <- t >= gap0 & t <= gap1
  lost_s <- gap1 - gap0
  pct <- 100 * lost_s / (max(t) - min(t))

  open_panel(path)

  # panel 1: raw -- surviving high start, long gap, surviving low end
  plot(t, raw,
    type = "l", col = GREY, lwd = 1.4, ylim = ylim,
    xlab = "", ylab = "pupil (a.u.)",
    main = "1) RAW INPUT  -  > 250 ms of pupil lost mid-trial (starts high, ends low)"
  )
  shade_na_runs(t, is.na(raw), "grey40", alpha = 0.16)
  graphics::text(mean(c(gap0, gap1)), ylim[1],
    labels = sprintf("%.1f s lost", lost_s),
    col = "grey30", adj = c(0.5, -0.6), font = 2, cex = 0.9
  )
  box_ctx()

  # panel 2: SUBOPTIMAL -- linear interpolation invents a descending ramp
  real_series <- filled
  real_series[gap_mask] <- NA_real_ # break the line across the gap
  plot(t, filled,
    type = "n", ylim = ylim, xlab = "", ylab = "pupil (a.u.)",
    main = "2) SUBOPTIMAL PROCESSING  -  interpolation fabricates 2.4 s of data",
    col.main = RED, font.main = 2
  )
  tint_bg(RED)
  usr <- graphics::par("usr")
  graphics::rect(
    gap0,
    usr[3],
    gap1,
    usr[4],
    col = grDevices::adjustcolor(RED, 0.10),
    border = NA
  )
  graphics::lines(t, real_series, col = "grey55", lwd = 1.6) # real, measured
  graphics::lines(t[bridge_sel], filled[bridge_sel], col = RED, lwd = 2.8) # invented
  graphics::text(mean(c(gap0, gap1)), mean(filled[bridge_sel]),
    labels = "not real data\n(should stay NA)", col = RED, font = 2, cex = 0.85,
    adj = c(0.5, -0.15)
  )
  graphics::legend(
    "topright",
    legend = c("real (measured)", "interpolated (fabricated)"),
    col = c("grey55", RED),
    lty = 1,
    lwd = c(1.6, 2.8),
    bty = "n",
    cex = 0.8
  )
  box_wrong()

  # panel 3: OPTIMAL -- reject the trial (too much contiguous data lost)
  plot(t, raw,
    type = "n", ylim = ylim, xlab = "time (s)", ylab = "pupil (a.u.)",
    main = "3) OPTIMAL PROCESSING  -  leave the long gap as NA (do not interpolate)",
    col.main = GREEN, font.main = 2
  )
  tint_bg(GREEN)
  graphics::lines(t, raw, col = "grey65", lwd = 1.4) # faded surviving data
  shade_na_runs(t, is.na(raw), "grey40", alpha = 0.16)
  graphics::text(mean(range(t)), mean(ylim),
    labels = "GAP REMAINS NA", col = GREEN, font = 2, cex = 2.1
  )
  graphics::text(mean(range(t)), mean(ylim),
    labels = "\n\n\n> 250 ms of contiguous data lost -> left as NA, not interpolated",
    col = GREEN, font = 1, cex = 0.95
  )
  box_right()

  close_panel(
    "eyeris pitfall - interpolate: do not bridge a long mid-trial dropout",
    sprintf(
      "contiguous data lost: %.1f s (%.0f%% of the trial) -> leave as NA, don't interpolate",
      lost_s, pct
    )
  )
  msg("  [interpolate]  gap = %.1f s (%.0f%% of trial)", lost_s, pct)
  invisible(path)
}

# ==============================================================================
# 3. lpfilt -- a rectangular (FFT brick-wall) filter instead of Butterworth
# ==============================================================================

# demo-only rectangular brick-wall low-pass: zero every FFT bin above the
# cutoff (a rectangular window in the frequency domain) and invert. The sharp
# edge convolves the signal with a sinc in time -> Gibbs ringing. No eyeris
# function filters this way; this exists only to show why you should not.
.brickwall_fft <- function(x, fs, cutoff) {
  n <- length(x)
  xd <- x - mean(x)
  X <- stats::fft(xd)
  freq <- (seq_len(n) - 1) * fs / n
  keep <- freq <= cutoff | freq >= (fs - cutoff)
  X[!keep] <- 0 + 0i
  Re(stats::fft(X, inverse = TRUE)) / n + mean(x)
}

fig_lpfilt <- function(path) {
  cutoff <- 1.5 # Hz -- SAME nominal cutoff for both filters, so the only
  # difference is the transition SHAPE (rectangular vs Butterworth roll-off)
  onset <- 6
  # a pupil dilation carrying broadband high-frequency noise: a hard
  # (rectangular) cutoff rings on it, while the Butterworth roll-off does not
  params <- sim_params(
    duration_secs = 20,
    blinks = FALSE,
    transients = FALSE,
    drift = FALSE,
    hippus = FALSE,
    dropout_frac = 0,
    phasic_onsets_ms = c(onset * 1000, 12000),
    phasic_amp = c(430, 430),
    phasic_tmax = 0.5,
    noise_sd = 10
  )
  sim <- simulate_eyeris(seed = 5, params = params, verbose = FALSE)
  base <- .quiet(interpolate(sim, verbose = FALSE)) # gap-free input to filter
  # Butterworth designed at the SAME cutoff for a fair, type-only comparison
  butter <- .quiet(lpfilt(base, wp = cutoff, ws = cutoff * 2, plot_freqz = FALSE))

  b <- .blk(base)
  fs <- base$info$sample.rate
  x <- b[[.latest_name(base)]]
  bw <- .brickwall_fft(x, fs, cutoff)
  bt <- .latest_col(butter)

  win <- c(onset - 2, onset + 3)
  sel <- win_sel(b, win)
  t <- b$time_secs[sel]

  # pre-onset baseline + the acausal pre-ringing dip that precedes the stimulus
  pre_lvl_sel <- b$time_secs >= (onset - 2) & b$time_secs < (onset - 0.5)
  base_lvl <- mean(x[pre_lvl_sel])
  ring_sel <- b$time_secs >= (onset - 1.4) & b$time_secs < onset
  dip_i <- which(ring_sel)[which.min(bw[ring_sel])]
  dip_t <- b$time_secs[dip_i]
  dip_y <- bw[dip_i]

  open_panel(path)

  # panel 1: the raw input -- a pupil dilation buried in high-frequency noise
  plot(t, x[sel],
    type = "l", col = GREY, lwd = 1.0, ylim = range(x[sel]),
    xlab = "", ylab = "pupil (a.u.)",
    main = "1) RAW INPUT  -  a pupil dilation with high-frequency noise"
  )
  graphics::abline(v = onset, col = BLUE, lty = 3)
  graphics::text(onset, max(x[sel]),
    labels = " stimulus onset", col = BLUE, adj = c(0, 1.1), cex = 0.8
  )
  box_ctx()

  ylim <- range(c(bw[sel], bt[sel]), na.rm = TRUE)

  # panel 2: WRONG -- brick-wall rings, including BEFORE the stimulus (acausal)
  plot(t, bw[sel],
    type = "n", ylim = ylim,
    xlab = "", ylab = "pupil (a.u.)",
    main = "2) SUBOPTIMAL PROCESSING  -  rectangular FFT filter -> ringing",
    col.main = RED, font.main = 2
  )
  tint_bg(RED)
  graphics::abline(h = base_lvl, col = "grey55", lty = 3)
  graphics::abline(v = onset, col = BLUE, lty = 3)
  graphics::lines(t, bt[sel], col = "grey45", lwd = 1.4, lty = 2)
  graphics::lines(t, bw[sel], col = RED, lwd = 2.4)
  # annotate the acausal pre-ringing dip
  graphics::points(dip_t, dip_y, col = RED, pch = 19, cex = 0.9)
  graphics::text(dip_t, dip_y,
    labels = "pre-ringing\n(before onset)", col = RED, cex = 0.78,
    adj = c(1.05, 0.4), font = 2
  )
  graphics::text(onset, ylim[2], labels = " stimulus onset",
    col = BLUE, adj = c(0, 1.1), cex = 0.8
  )
  graphics::legend("topright",
    legend = c("brick-wall result", "Butterworth (reference)"),
    col = c(RED, "grey45"), lty = c(1, 2), lwd = c(2.4, 1.4), bty = "n", cex = 0.8
  )
  box_wrong()

  # panel 3: RIGHT -- Butterworth is smooth, no ringing
  plot(t, bt[sel],
    type = "n", ylim = ylim,
    xlab = "time (s)", ylab = "pupil (a.u.)",
    main = "3) OPTIMAL PROCESSING  -  Butterworth retains the dilation shape",
    col.main = GREEN, font.main = 2
  )
  tint_bg(GREEN)
  graphics::abline(h = base_lvl, col = "grey55", lty = 3)
  graphics::abline(v = onset, col = BLUE, lty = 3)
  graphics::lines(t, bt[sel], col = GREEN, lwd = 2.4)
  box_right()

  dip <- base_lvl - dip_y # depth of the pre-stimulus ringing dip
  close_panel(
    "eyeris pitfall - lpfilt: a rectangular filter rings; use Butterworth",
    sprintf(
      "acausal pre-ringing dip before the stimulus = %.0f a.u.  (matched %.1f Hz cutoff)",
      dip, cutoff
    )
  )
  msg("  [lpfilt]  pre-ring dip = %.1f a.u.", dip)
  invisible(path)
}

# ==============================================================================
# 4. detransient -- low-pass filtering BEFORE removing transients
# ==============================================================================
# add a narrow transient spike to the base pupil_raw column
inject_transient <- function(o, t_center, amp, width_ms = 3) {
  b <- .blk(o)
  fs <- o$info$sample.rate
  ci <- which.min(abs(b$time_secs - t_center))
  w <- width_ms * fs / 1000
  idx <- seq_len(nrow(b))
  b[["pupil_raw"]] <- b[["pupil_raw"]] + amp * exp(-((idx - ci)^2) / (2 * w^2))
  o$timeseries[["block_1"]] <- b
  o
}

fig_detransient <- function(path) {
  t1 <- 8.72 # a downward glitch
  t2 <- 9.02 # an upward glitch
  # Phasic responses live OUTSIDE the preview window so the block-wide
  # speed-MAD stays healthy (a pathologically quiet block makes detransient
  # over-sensitive); the window itself is a clean baseline carrying just the
  # two transients we inject.
  params <- sim_params(
    duration_secs = 20,
    phasic = TRUE,
    phasic_onsets_ms = c(3000, 15000),
    phasic_amp = c(300, 300),
    drift = FALSE,
    blinks = FALSE,
    transients = FALSE,
    hippus = TRUE,
    hippus_amp = 25,
    dropout_frac = 0,
    noise_sd = 3
  )
  sim <- simulate_eyeris(seed = 1, params = params, verbose = FALSE)
  sim <- inject_transient(sim, t1, -700)
  sim <- inject_transient(sim, t2, 630)

  # optimal: detransient removes both spikes BEFORE they can be smeared
  optimal <- .quiet(
    sim |> detransient() |> interpolate(verbose = FALSE) |> lp()
  )
  # suboptimal: low-pass first spreads each spike across neighboring samples;
  # a later detransient can nibble at the smear but can no longer remove it
  # (the trailing interpolate just bridges detransient's few NAs for a clean line)
  suboptimal <- .quiet(
    sim |>
      interpolate(verbose = FALSE) |>
      lp() |>
      detransient() |>
      interpolate(verbose = FALSE)
  )

  b <- .blk(sim)
  win <- c(8.3, 9.4)
  sel <- win_sel(b, win)
  t <- b$time_secs[sel]
  raw <- b$pupil_raw[sel]
  w_col <- .latest_col(suboptimal)[sel]
  r_col <- .latest_col(optimal)[sel]

  open_panel(path)

  # panel 1: raw with TWO transient spikes
  plot(t, raw,
    type = "l", col = GREY, lwd = 1.2,
    xlab = "", ylab = "pupil (a.u.)",
    main = "1) RAW INPUT  -  two isolated transient spikes (tracker glitches)"
  )
  graphics::abline(v = c(t1, t2), col = "grey70", lty = 3)
  box_ctx()

  ylim <- range(c(w_col, r_col), na.rm = TRUE)

  # panel 2: SUBOPTIMAL -- filter first; the smeared spikes survive
  plot(t, w_col,
    type = "n", ylim = ylim, xlab = "", ylab = "pupil (a.u.)",
    main = "2) SUBOPTIMAL PROCESSING  -  interpolate -> lpfilt -> detransient",
    col.main = RED, font.main = 2
  )
  tint_bg(RED)
  graphics::abline(v = c(t1, t2), col = grDevices::adjustcolor(RED, 0.4), lty = 3)
  graphics::lines(t, r_col, col = "grey45", lwd = 1.4, lty = 2)
  graphics::lines(t, w_col, col = RED, lwd = 2.4)
  graphics::legend("topright",
    legend = c("suboptimal result", "optimal result (reference)"),
    col = c(RED, "grey45"), lty = c(1, 2), lwd = c(2.4, 1.4), bty = "n", cex = 0.8
  )
  box_wrong()

  # panel 3: OPTIMAL -- remove transients first; the trace is clean
  plot(t, r_col,
    type = "n", ylim = ylim, xlab = "time (s)", ylab = "pupil (a.u.)",
    main = "3) OPTIMAL PROCESSING  -  detransient -> interpolate -> lpfilt",
    col.main = GREEN, font.main = 2
  )
  tint_bg(GREEN)
  graphics::abline(v = c(t1, t2), col = grDevices::adjustcolor(GREEN, 0.4), lty = 3)
  graphics::lines(t, r_col, col = GREEN, lwd = 2.4)
  box_right()

  # metric: how much of the smeared transient survives (vs the clean result)
  resid <- max(abs(w_col - r_col), na.rm = TRUE)
  close_panel(
    "eyeris pitfall - detransient: remove transients BEFORE low-pass filtering",
    sprintf(
      "smeared transient remaining after processing:  suboptimal = %.0f a.u.,  optimal ~ 0",
      resid
    )
  )
  msg("  [detransient]  surviving smeared transient = %.1f a.u.", resid)
  invisible(path)
}

# ==============================================================================
# 5. detrend -- linear vs spline on a curved (exponential-decay) drift
# ==============================================================================

# z-score helper mirroring eyeris::zscore_pupil ((x - mean) / sd)
.z <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)

# shade only the area ABOVE the zero line (the dilation peaks researchers care
# about); negative excursions are left unshaded
shade_to_zero <- function(t, y, color, alpha = 0.28) {
  ok <- is.finite(t) & is.finite(y)
  yy <- pmax(y[ok], 0) # clip to positive -> only peaks above 0 get filled
  graphics::polygon(
    c(t[ok], rev(t[ok])), c(yy, rep(0, sum(ok))),
    col = grDevices::adjustcolor(color, alpha.f = alpha), border = NA
  )
}

fig_detrend <- function(path) {
  # steep linear drift PLUS a strong exponential decay after ~trial 5
  onsets <- seq(2000, 38000, by = 4000) # ~10 evenly spaced trials
  params <- sim_params(
    duration_secs = 42,
    drift = TRUE,
    drift_slope = 10,
    decay = TRUE,
    decay_amp = 800,
    decay_tau = 6,
    phasic_onsets_ms = onsets,
    phasic_amp = c(220, 220),
    n_blinks = 4
  )
  sim <- simulate_eyeris(seed = 2, params = params, verbose = FALSE)
  decay_onset <- onsets[5] / 1000

  pre <- .quiet(sim |> deblink() |> detransient() |> interpolate() |> lp())
  b <- .blk(pre)
  t <- b$time_secs
  pupil <- .latest_col(pre)

  # WRONG: linear detrend then z-score
  lin_fit <- stats::lm(pupil ~ t)
  z_lin <- .z(stats::residuals(lin_fit))

  # RIGHT: spline detrend then z-score (inline; mirrors the separate PR)
  spline_fit <- stats::lm(pupil ~ splines::ns(t, df = 5))
  z_spl <- .z(pupil - stats::predict(spline_fit))

  ylim <- range(c(z_lin, z_spl), na.rm = TRUE)

  open_panel(path)

  # panel 1: the preprocessed signal, drift + post-trial-5 decay visible
  plot(t, pupil,
    type = "l", col = GREY, lwd = 1.3,
    xlab = "", ylab = "pupil (a.u.)",
    main = "1) PRE-DETREND  -  steep drift + exponential decay after trial 5"
  )
  graphics::abline(v = decay_onset, col = BLUE, lty = 3, lwd = 1.4)
  graphics::text(decay_onset, graphics::par("usr")[4],
    labels = " decay onset (trial 5)", col = BLUE, adj = c(0, 1.3), cex = 0.85
  )
  box_ctx()

  # post-decay epoch mean tells the story
  post <- t >= decay_onset
  m_lin <- mean(z_lin[post], na.rm = TRUE)
  m_spl <- mean(z_spl[post], na.rm = TRUE)

  # panel 2: WRONG -- linear detrend leaves the curve; z-scores biased low
  plot(t, z_lin,
    type = "l", col = RED, lwd = 1.9, ylim = ylim,
    xlab = "", ylab = "pupil (z)",
    main = "2) SUBOPTIMAL PROCESSING  -  linear detrend leaves the decay; z biased below 0",
    col.main = RED, font.main = 2
  )
  tint_bg(RED)
  shade_to_zero(t, z_lin, RED)
  graphics::lines(t, z_lin, col = RED, lwd = 1.9)
  graphics::abline(h = 0, col = "grey30", lwd = 1.1)
  graphics::abline(v = decay_onset, col = BLUE, lty = 3)
  graphics::text(graphics::par("usr")[2], ylim[1],
    labels = sprintf("post-trial-5 mean = %+.2f z ", m_lin),
    col = RED, adj = c(1, -0.6), font = 2, cex = 0.9
  )
  box_wrong()

  # panel 3: RIGHT -- spline detrend removes the curve; z sits at ~0
  plot(t, z_spl,
    type = "l", col = GREEN, lwd = 1.9, ylim = ylim,
    xlab = "time (s)", ylab = "pupil (z)",
    main = "3) OPTIMAL PROCESSING  -  spline detrend removes the decay; z centered on 0",
    col.main = GREEN, font.main = 2
  )
  tint_bg(GREEN)
  shade_to_zero(t, z_spl, GREEN)
  graphics::lines(t, z_spl, col = GREEN, lwd = 1.9)
  graphics::abline(h = 0, col = "grey30", lwd = 1.1)
  graphics::abline(v = decay_onset, col = BLUE, lty = 3)
  graphics::text(graphics::par("usr")[2], ylim[1],
    labels = sprintf("post-trial-5 mean = %+.2f z ", m_spl),
    col = GREEN, adj = c(1, -0.6), font = 2, cex = 0.9
  )
  box_right()

  close_panel(
    "eyeris pitfall - detrend: a linear fit cannot remove a curved drift",
    sprintf(
      "post-decay z-scored mean:  linear = %+.2f (biased),  spline = %+.2f (unbiased)",
      m_lin, m_spl
    )
  )
  msg("  [detrend]  post-decay z mean: linear=%+.3f spline=%+.3f", m_lin, m_spl)
  invisible(path)
}

# ==============================================================================
# 6. zscore -- two blocks with different mean / variance
# ==============================================================================

# simulate one short block with two phasic trials, return a data frame
.sim_block <- function(seed, baseline, amp, noise_sd) {
  p <- sim_params(
    duration_secs = 14,
    baseline_mean = baseline,
    drift = FALSE,
    hippus = FALSE,
    blinks = FALSE,
    transients = FALSE,
    dropout_frac = 0,
    phasic_onsets_ms = c(3000, 9000),
    phasic_amp = c(amp, amp),
    noise_sd = noise_sd
  )
  s <- simulate_eyeris(seed = seed, params = p, verbose = FALSE)
  b <- .blk(s)
  data.frame(time = b$time_secs, pupil = b$pupil_raw)
}

fig_zscore <- function(path) {
  gap <- 6 # seconds of flatline spacer between blocks
  # block 1: lower mean, tighter variance; block 2: higher mean, larger variance
  b1 <- .sim_block(seed = 11, baseline = 4200, amp = 140, noise_sd = 3)
  b2 <- .sim_block(seed = 12, baseline = 5800, amp = 420, noise_sd = 9)

  # per-block z-scores (this is what eyeris does by processing blocks separately)
  b1$z <- .z(b1$pupil)
  b2$z <- .z(b2$pupil)

  # flatline spacer segments (constant, so blocks don't overlap on the axis)
  flat_dt <- b1$time[2] - b1$time[1]
  mk_flat <- function(t0, level) {
    tt <- seq(0, gap, by = flat_dt)
    data.frame(time = t0 + tt, pupil = level, z = 0)
  }

  dur1 <- max(b1$time)
  f1_start <- dur1
  f2_start <- dur1 + gap
  b2_start <- dur1 + 2 * gap

  flat1_level <- b1$pupil[nrow(b1)]
  flat2_level <- b2$pupil[1]
  flat1 <- mk_flat(f1_start, flat1_level)
  flat2 <- mk_flat(f2_start, flat2_level)

  # assemble a common time axis: b1 | flat | flat | b2
  b1$T <- b1$time
  flat1$T <- flat1$time
  flat2$T <- flat2$time
  b2$T <- b2$time + b2_start

  raw <- rbind(
    data.frame(T = b1$T, y = b1$pupil, blk = 1),
    data.frame(T = flat1$T, y = flat1$pupil, blk = 0),
    data.frame(T = flat2$T, y = flat2$pupil, blk = 0),
    data.frame(T = b2$T, y = b2$pupil, blk = 2)
  )
  # global (single-pool) z-score -- the WRONG standardization
  raw$zg <- .z(raw$y)
  # per-block z-score -- the RIGHT standardization (flatlines -> 0)
  zp <- c(b1$z, flat1$z, flat2$z, b2$z)
  raw$zp <- zp

  blk1_span <- range(b1$T)
  blk2_span <- range(b2$T)
  mark_blocks <- function(labs = TRUE) {
    usr <- graphics::par("usr")
    graphics::rect(blk1_span[1], usr[3], blk1_span[2], usr[4],
      col = grDevices::adjustcolor(BLUE, 0.06), border = NA
    )
    graphics::rect(blk2_span[1], usr[3], blk2_span[2], usr[4],
      col = grDevices::adjustcolor("darkorange", 0.08), border = NA
    )
    if (labs) {
      graphics::text(mean(blk1_span), usr[4], "block 1", col = BLUE,
        adj = c(0.5, 1.4), font = 2, cex = 0.9
      )
      graphics::text(mean(blk2_span), usr[4], "block 2", col = "darkorange3",
        adj = c(0.5, 1.4), font = 2, cex = 0.9
      )
    }
  }

  open_panel(path)

  # panel 1: raw a.u. -- blocks sit at different means with different spread
  plot(raw$T, raw$y,
    type = "l", col = GREY, lwd = 1.3,
    xlab = "", ylab = "pupil (a.u.)",
    main = "1) RAW  -  block 1 (low mean/var) vs block 2 (high mean/var)"
  )
  mark_blocks()
  box_ctx()

  # panel 2: WRONG -- one global z-score; variance mismatch remains
  plot(raw$T, raw$zg,
    type = "l", col = RED, lwd = 1.6,
    xlab = "", ylab = "pupil (z)",
    main = "2) SUBOPTIMAL PROCESSING  -  single global z-score: blocks still not comparable",
    col.main = RED, font.main = 2
  )
  tint_bg(RED)
  mark_blocks(labs = FALSE)
  graphics::lines(raw$T, raw$zg, col = RED, lwd = 1.6)
  graphics::abline(h = 0, col = "grey30")
  box_wrong()

  # panel 3: RIGHT -- per-block z-score; blocks now share a common scale
  plot(raw$T, raw$zp,
    type = "l", col = GREEN, lwd = 1.6,
    xlab = "time (s)", ylab = "pupil (z)",
    main = "3) OPTIMAL PROCESSING  -  per-block z-score: blocks now look alike",
    col.main = GREEN, font.main = 2
  )
  tint_bg(GREEN)
  mark_blocks(labs = FALSE)
  graphics::lines(raw$T, raw$zp, col = GREEN, lwd = 1.6)
  graphics::abline(h = 0, col = "grey30")
  box_right()

  # metric: gap between the two blocks' mean levels after each standardization.
  # A single global z-score leaves the blocks sitting at different levels;
  # per-block z-scoring centers each block on 0, making them comparable.
  bmean <- function(z, k) mean(z[raw$blk == k], na.rm = TRUE)
  gap_g <- abs(bmean(raw$zg, 1) - bmean(raw$zg, 2))
  gap_p <- abs(bmean(raw$zp, 1) - bmean(raw$zp, 2))
  close_panel(
    "eyeris pitfall - zscore: standardize within each block, not across blocks",
    sprintf("between-block mean gap:  global z = %.2f,  per-block z = %.2f", gap_g, gap_p)
  )
  msg("  [zscore]  between-block mean gap: global=%.3f per-block=%.3f", gap_g, gap_p)
  invisible(path)
}

# ---- driver -----------------------------------------------------------------

main <- function() {
  figs <- list(
    `pitfall-deblink.png` = fig_deblink,
    `pitfall-interpolate.png` = fig_interpolate,
    `pitfall-lpfilt.png` = fig_lpfilt,
    `pitfall-detransient.png` = fig_detransient,
    `pitfall-detrend.png` = fig_detrend,
    `pitfall-zscore.png` = fig_zscore
  )
  msg("Writing %d pitfall figures to %s ...", length(figs), out_dir)
  for (nm in names(figs)) {
    path <- file.path(out_dir, nm)
    figs[[nm]](path)
    sz <- file.info(path)$size
    msg("  wrote %s (%.0f KB)", path, sz / 1024)
  }
  msg("Done.")
}

if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  main()
}
