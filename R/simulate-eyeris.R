#' Configure the synthetic pupil signal model
#'
#' @description
#' Builds the parameter list that controls [eyeris::simulate_eyeris()]. Every
#' component of the synthetic pupil time series (tonic baseline, slow drift,
#' hippus, task-evoked phasic responses, blinks, transient spikes, measurement
#' noise, and optional line noise) is independently toggle-able and
#' parameterized here, so a single scenario can isolate exactly the feature it
#' needs to teach.
#'
#' @details
#' The synthetic signal is composed additively in arbitrary units (a.u.) matched
#' to real EyeLink **pupil diameter** data (which typically ranges from roughly
#' 3600--7000 a.u.):
#'
#' \deqn{L(t) = B_0 + \mathrm{drift}(t) + \mathrm{hippus}(t) +
#'   \sum_k \mathrm{phasic}_k(t)}
#' \deqn{\mathrm{pupil}(t) = L(t) + \sum_j \mathrm{transient}_j(t) +
#'   \mathrm{line}(t) + \mathrm{noise}(t)}
#'
#' after which blink and dropout artifacts are stamped on (see
#' [eyeris::simulate_eyeris()]).
#'
#' **Components (each independently toggle-able):**
#' \itemize{
#'   \item **Tonic baseline** (`baseline`): a constant offset `baseline_mean`.
#'   \item **Linear drift** (`drift`): a slow tonic decline
#'     \eqn{-\mathrm{drift\_slope} \cdot t} (a.u. per second), plus an optional
#'     quadratic term `drift_curv`. This is precisely what
#'     [eyeris::detrend()] removes.
#'   \item **Hippus** (`hippus`): a slow arousal oscillation
#'     \eqn{A_h \sin(2\pi f_h t + \phi)} that sits below the low-pass cutoff and
#'     therefore survives [eyeris::lpfilt()].
#'   \item **Phasic response** (`phasic`): the canonical task-evoked pupil
#'     response modeled with the Hoeks & Levelt (1993) Erlang gamma kernel
#'     \eqn{h(\tau) = \tau^n e^{-n\tau / t_{max}}}, peak-normalized and scaled by
#'     `phasic_amp`, time-locked to stimulus onsets.
#'   \item **Blinks** (`blinks`): runs of **missing data (`NA`)** flanked by the
#'     rapid partial-occlusion down-then-up spikes that surround real blinks ---
#'     the artifact [eyeris::deblink()] is designed to remove.
#'   \item **Transients** (`transients`): isolated, physiologically implausible
#'     fast spikes (tracker glitches) that [eyeris::detransient()] targets.
#'   \item **Noise** (`noise`): broadband white measurement noise. Always keep
#'     `noise_sd > 0` --- a perfectly noiseless signal makes the median absolute
#'     deviation of the pupil speed zero, which aborts [eyeris::detransient()].
#'   \item **Line noise** (`line`): an optional high-frequency tone used to
#'     demonstrate aliasing when decimating without an anti-alias filter.
#' }
#'
#' @param fs Sampling rate in Hz. Must divide 1000 evenly (e.g. `1000`, `500`,
#'   `250`) so sample timestamps remain integer milliseconds. Defaults to `1000`
#' @param duration_secs Recording duration in seconds. Defaults to `60`
#' @param baseline Logical; include the tonic baseline offset. Defaults to `TRUE`
#' @param baseline_mean Tonic baseline pupil size in a.u. Defaults to `5000`
#' @param drift Logical; include a slow linear drift. Defaults to `FALSE`
#' @param drift_slope Drift slope magnitude in a.u. per second (applied as a
#'   decline). Defaults to `8` (i.e. a 480 a.u. decline over 60 s)
#' @param drift_curv Quadratic drift coefficient in a.u. per second squared.
#'   Defaults to `0`
#' @param hippus Logical; include the slow hippus oscillation. Defaults to `TRUE`
#' @param hippus_amp Hippus amplitude in a.u. Defaults to `40`
#' @param hippus_freq Hippus frequency in Hz. Defaults to `0.12`
#' @param phasic Logical; include task-evoked phasic responses. Defaults to
#'   `TRUE`
#' @param phasic_n Erlang shape parameter (unitless). Defaults to `10.1`
#' @param phasic_tmax Time-to-peak of the phasic response in seconds. Defaults
#'   to `0.930`
#' @param phasic_amp Length-2 numeric `c(min, max)`; peak phasic amplitudes are
#'   drawn uniformly from this range (a.u.). Defaults to `c(120, 300)`
#' @param phasic_isi Length-2 numeric `c(min, max)`; inter-stimulus intervals
#'   (seconds) are drawn uniformly from this range when `phasic_onsets_ms` is
#'   `NULL`. Defaults to `c(6, 8)`
#' @param phasic_onsets_ms Optional numeric vector of explicit stimulus onset
#'   times in milliseconds. When `NULL` (default), onsets are generated from
#'   `phasic_isi`
#' @param blinks Logical; include blink artifacts. Defaults to `TRUE`
#' @param n_blinks Number of blinks. Defaults to `6`
#' @param blink_dur_ms Length-2 numeric `c(min, max)`; blink core (missing-data)
#'   durations in milliseconds are drawn uniformly from this range. Defaults to
#'   `c(100, 300)`
#' @param blink_flank_ms Duration in milliseconds of the occlusion spike on each
#'   side of a blink core. Keep below the [eyeris::deblink()] `extend` value
#'   (default 50 ms) so deblinking fully removes it. Defaults to `40`
#' @param blink_depth Depth in a.u. of the leading occlusion dip. Defaults to
#'   `400`
#' @param blink_overshoot Height in a.u. of the trailing recovery overshoot.
#'   Defaults to `300`
#' @param blink_flank_shape Shape of the occlusion ramp: `"linear"` or
#'   `"cosine"`. Defaults to `"linear"`
#' @param transients Logical; include isolated transient spikes. Defaults to
#'   `TRUE`
#' @param n_transients Number of transient spikes. Defaults to `3`
#' @param transient_amp Length-2 numeric `c(min, max)`; transient amplitudes are
#'   drawn uniformly from this range (a.u.) with random sign. Defaults to
#'   `c(200, 350)`
#' @param transient_width_ms Gaussian width (standard deviation) of each
#'   transient in milliseconds. Defaults to `1.5`
#' @param noise Logical; include broadband measurement noise. Should remain
#'   `TRUE` for any signal that will be passed to [eyeris::detransient()].
#'   Defaults to `TRUE`
#' @param noise_sd Standard deviation of the white measurement noise in a.u.
#'   Defaults to `3`
#' @param noise_ar Lag-1 autocorrelation for optional AR(1) noise coloring in
#'   `[0, 1)`. Defaults to `0` (white noise)
#' @param line Logical; include a high-frequency line-noise tone. Defaults to
#'   `FALSE`
#' @param line_freq Line-noise frequency in Hz. Defaults to `96`
#' @param line_amp Line-noise amplitude in a.u. Defaults to `20`
#' @param dropout_frac Fraction of (non-blink) samples to mark missing as short
#'   scattered dropout runs. Defaults to `0.003`
#' @param dropout_run_ms Length-2 numeric `c(min, max)`; dropout run lengths in
#'   milliseconds. Defaults to `c(1, 5)`
#' @param clip Length-2 numeric `c(min, max)`; observed (non-missing) samples are
#'   clamped to this a.u. range. Defaults to `c(3200, 7200)`
#' @param eye Which eye to label the data as (`"L"` or `"R"`). Defaults to `"R"`
#' @param pupil_dtype Pupil data type label (`"DIAMETER"` or `"AREA"`). Defaults
#'   to `"DIAMETER"`
#'
#' @return A named list of class `eyeris_sim_params`.
#'
#' @seealso [eyeris::simulate_eyeris()] to generate data from these parameters.
#'
#' @examples
#' # default parameters
#' p <- sim_params()
#'
#' # a short recording with a strong linear drift for a detrend demo
#' p2 <- sim_params(duration_secs = 20, drift = TRUE, drift_slope = 12)
#'
#' @export
sim_params <- function(
  fs = 1000L,
  duration_secs = 60,
  baseline = TRUE,
  baseline_mean = 5000,
  drift = FALSE,
  drift_slope = 8,
  drift_curv = 0,
  hippus = TRUE,
  hippus_amp = 40,
  hippus_freq = 0.12,
  phasic = TRUE,
  phasic_n = 10.1,
  phasic_tmax = 0.930,
  phasic_amp = c(120, 300),
  phasic_isi = c(6, 8),
  phasic_onsets_ms = NULL,
  blinks = TRUE,
  n_blinks = 6,
  blink_dur_ms = c(100, 300),
  blink_flank_ms = 40,
  blink_depth = 400,
  blink_overshoot = 300,
  blink_flank_shape = c("linear", "cosine"),
  transients = TRUE,
  n_transients = 3,
  transient_amp = c(200, 350),
  transient_width_ms = 1.5,
  noise = TRUE,
  noise_sd = 3,
  noise_ar = 0,
  line = FALSE,
  line_freq = 96,
  line_amp = 20,
  dropout_frac = 0.003,
  dropout_run_ms = c(1, 5),
  clip = c(3200, 7200),
  eye = c("R", "L"),
  pupil_dtype = c("DIAMETER", "AREA")
) {
  blink_flank_shape <- match.arg(blink_flank_shape)
  eye <- match.arg(eye)
  pupil_dtype <- match.arg(pupil_dtype)

  # ---- validation ----
  if (1000 %% fs != 0) {
    log_error("`fs` must divide 1000 evenly (e.g. 1000, 500, 250); got {fs}.")
  }
  if (duration_secs <= 0) {
    log_error("`duration_secs` must be positive; got {duration_secs}.")
  }
  if (isTRUE(noise) && !(noise_sd > 0)) {
    log_error(paste0(
      "`noise_sd` must be > 0 when `noise = TRUE`. A noiseless signal makes ",
      "the pupil-speed MAD zero, which aborts `detransient()`."
    ))
  }
  if (!(noise_ar >= 0 && noise_ar < 1)) {
    log_error("`noise_ar` must be in [0, 1); got {noise_ar}.")
  }
  if (!(dropout_frac >= 0 && dropout_frac < 0.5)) {
    log_error("`dropout_frac` must be in [0, 0.5); got {dropout_frac}.")
  }
  if (isTRUE(line) && !(fs > 2 * line_freq)) {
    log_error(
      "`fs` ({fs}) must exceed 2 * `line_freq` ({line_freq}) to represent the tone."
    )
  }
  check_len2_increasing <- function(x, nm) {
    if (length(x) != 2 || anyNA(x) || x[2] < x[1]) {
      log_error("`{nm}` must be an increasing length-2 numeric `c(min, max)`.")
    }
  }
  check_len2_increasing(phasic_amp, "phasic_amp")
  check_len2_increasing(phasic_isi, "phasic_isi")
  check_len2_increasing(blink_dur_ms, "blink_dur_ms")
  check_len2_increasing(transient_amp, "transient_amp")
  check_len2_increasing(dropout_run_ms, "dropout_run_ms")
  check_len2_increasing(clip, "clip")

  structure(
    list(
      fs = as.numeric(fs),
      duration_secs = duration_secs,
      baseline = baseline,
      baseline_mean = baseline_mean,
      drift = drift,
      drift_slope = drift_slope,
      drift_curv = drift_curv,
      hippus = hippus,
      hippus_amp = hippus_amp,
      hippus_freq = hippus_freq,
      phasic = phasic,
      phasic_n = phasic_n,
      phasic_tmax = phasic_tmax,
      phasic_amp = phasic_amp,
      phasic_isi = phasic_isi,
      phasic_onsets_ms = phasic_onsets_ms,
      blinks = blinks,
      n_blinks = n_blinks,
      blink_dur_ms = blink_dur_ms,
      blink_flank_ms = blink_flank_ms,
      blink_depth = blink_depth,
      blink_overshoot = blink_overshoot,
      blink_flank_shape = blink_flank_shape,
      transients = transients,
      n_transients = n_transients,
      transient_amp = transient_amp,
      transient_width_ms = transient_width_ms,
      noise = noise,
      noise_sd = noise_sd,
      noise_ar = noise_ar,
      line = line,
      line_freq = line_freq,
      line_amp = line_amp,
      dropout_frac = dropout_frac,
      dropout_run_ms = dropout_run_ms,
      clip = clip,
      eye = eye,
      pupil_dtype = pupil_dtype
    ),
    class = "eyeris_sim_params"
  )
}

#' Print a synthetic pupil parameter list
#'
#' @param x An object of class `eyeris_sim_params` from [eyeris::sim_params()]
#' @param ... Unused; included for S3 consistency
#'
#' @return `x`, invisibly
#'
#' @export
print.eyeris_sim_params <- function(x, ...) {
  on_off <- function(flag) if (isTRUE(flag)) "on" else "off"
  cli::cli_h2("eyeris synthetic pupil parameters")
  cli::cli_text(
    "{.field fs}: {x$fs} Hz | {.field duration}: {x$duration_secs} s | ",
    "{.field type}: {x$pupil_dtype}"
  )
  cli::cli_ul(c(
    "baseline ({on_off(x$baseline)}): mean {x$baseline_mean} a.u.",
    "drift ({on_off(x$drift)}): slope -{x$drift_slope} a.u./s",
    "hippus ({on_off(x$hippus)}): {x$hippus_amp} a.u. @ {x$hippus_freq} Hz",
    "phasic ({on_off(x$phasic)}): amp {x$phasic_amp[1]}-{x$phasic_amp[2]} a.u.",
    "blinks ({on_off(x$blinks)}): n = {x$n_blinks}",
    "transients ({on_off(x$transients)}): n = {x$n_transients}",
    "noise ({on_off(x$noise)}): sd {x$noise_sd} a.u.",
    "line ({on_off(x$line)}): {x$line_amp} a.u. @ {x$line_freq} Hz"
  ))
  invisible(x)
}

#' Peak-normalized Hoeks & Levelt (1993) pupil response kernel
#'
#' @param tau Numeric vector of times (seconds) relative to stimulus onset
#' @param n Erlang shape parameter
#' @param t_max Time-to-peak in seconds
#'
#' @return A numeric vector the same length as `tau`, zero for `tau < 0` and
#'   peak-normalized to 1
#'
#' @keywords internal
.ppr_kernel <- function(tau, n, t_max) {
  # evaluate the power/exponential only for nonnegative tau: computing
  # tau^n for negative tau with fractional n yields NaN, which `ifelse()`
  # would otherwise generate eagerly (for every element) and then discard
  h <- numeric(length(tau))
  pos <- tau >= 0
  h[pos] <- tau[pos]^n * exp(-n * tau[pos] / t_max)
  peak <- max(h)
  if (peak > 0) h / peak else h
}

#' Build the additive phasic (task-evoked) pupil response component
#'
#' @param t Numeric vector of sample times in seconds
#' @param onsets_s Numeric vector of stimulus onset times in seconds
#' @param amps Numeric vector of per-onset peak amplitudes (a.u.)
#' @param n Erlang shape parameter
#' @param t_max Time-to-peak in seconds
#'
#' @return A numeric vector the same length as `t`
#'
#' @keywords internal
.sim_phasic <- function(t, onsets_s, amps, n, t_max) {
  out <- numeric(length(t))
  for (k in seq_along(onsets_s)) {
    out <- out + amps[k] * .ppr_kernel(t - onsets_s[k], n, t_max)
  }
  out
}

#' Stamp blink artifacts (missing cores + occlusion flank spikes) onto a signal
#'
#' @param pupil Numeric pupil vector to modify
#' @param centers Integer sample indices of blink centers
#' @param core_samps Integer vector of blink core durations in samples
#' @param flank_samps Blink flank duration in samples
#' @param depth Leading occlusion dip depth (a.u.)
#' @param overshoot Trailing recovery overshoot height (a.u.)
#' @param shape Flank ramp shape: `"linear"` or `"cosine"`
#'
#' @return A list with the modified `pupil`, a logical `core_mask` (TRUE over
#'   missing cores), a logical `region_mask` (TRUE over cores + flanks), and a
#'   data frame `df` of blink `start`/`end` sample indices
#'
#' @keywords internal
.sim_blinks <- function(
  pupil,
  centers,
  core_samps,
  flank_samps,
  depth,
  overshoot,
  shape = "linear"
) {
  n <- length(pupil)
  core_mask <- logical(n)
  region_mask <- logical(n)
  starts <- integer(0)
  ends <- integer(0)

  ramp <- function(m, decreasing = FALSE) {
    if (m <= 0) {
      return(numeric(0))
    }
    w <- seq_len(m) / m
    if (shape == "cosine") {
      w <- (1 - cos(pi * w)) / 2
    }
    if (decreasing) rev(w) else w
  }

  for (b in seq_along(centers)) {
    c_i <- centers[b]
    half <- floor(core_samps[b] / 2)
    core_start <- c_i - half
    core_end <- core_start + core_samps[b] - 1L
    core_start <- max(core_start, 1L + flank_samps)
    core_end <- min(core_end, n - flank_samps)
    if (core_end < core_start) {
      next
    }

    pre_idx <- (core_start - flank_samps):(core_start - 1L)
    post_idx <- (core_end + 1L):(core_end + flank_samps)

    # leading dip reaches -depth at the sample just before the core
    pupil[pre_idx] <- pupil[pre_idx] - depth * ramp(length(pre_idx))
    # trailing overshoot decays back to baseline moving away from the core
    pupil[post_idx] <- pupil[post_idx] +
      overshoot * ramp(length(post_idx), decreasing = TRUE)

    pupil[core_start:core_end] <- NA_real_

    core_mask[core_start:core_end] <- TRUE
    region_mask[c(pre_idx, core_start:core_end, post_idx)] <- TRUE
    starts <- c(starts, core_start)
    ends <- c(ends, core_end)
  }

  list(
    pupil = pupil,
    core_mask = core_mask,
    region_mask = region_mask,
    df = data.frame(start = starts, end = ends)
  )
}

#' Generate a synthetic `eyeris` object with realistic pupil characteristics
#'
#' @description
#' Simulates a pupil time series whose statistical and morphological
#' characteristics resemble real EyeLink recordings --- a tonic baseline with
#' slow drift and hippus, canonical task-evoked phasic dilations, blinks with
#' partial-occlusion flank spikes, isolated transient artifacts, measurement
#' noise, and optional high-frequency line noise --- and wraps it in a fully
#' valid S3 `eyeris` object.
#'
#' Because the returned object is byte-compatible with the output of
#' [eyeris::load_asc()], it flows unchanged through the entire `eyeris`
#' pipeline: [eyeris::deblink()], [eyeris::detransient()],
#' [eyeris::interpolate()], [eyeris::lpfilt()], [eyeris::downsample()],
#' [eyeris::detrend()], [eyeris::zscore()], [eyeris::plot.eyeris()],
#' [eyeris::epoch()], and [eyeris::summarize_confounds()].
#'
#' @details
#' The signal is generated deterministically given `seed`: the same `seed` and
#' `params` always yield an identical object, and the global random number
#' generator state is left untouched (generation is confined via
#' [withr::with_seed()]). See [eyeris::sim_params()] for the full generative
#' model and every tunable component.
#'
#' The clean latent signal (before artifacts and noise) and the indices of the
#' injected artifacts are attached to the returned object as the attribute
#' `"sim_truth"` for use as ground truth in quantitative demonstrations; note
#' that this attribute does **not** survive the pipeline step functions (which
#' rebuild the time series data frame), so downstream tooling recomputes ground
#' truth from a retained copy rather than relying on the attribute.
#'
#' @param seed Integer random seed for reproducible generation. Defaults to `1`
#' @param params A parameter list from [eyeris::sim_params()]. Defaults to
#'   `sim_params()`
#' @param block Numeric block label written into the time series. Defaults to
#'   `1`
#' @param verbose Logical; print a short status message. Defaults to `TRUE`
#'
#' @return An object of S3 class `eyeris` (see the *Anatomy of an `eyeris`
#'   Object* vignette --- \code{vignette("anatomy", package = "eyeris")}), with
#'   a `"sim_truth"` attribute describing the ground-truth signal.
#'
#' @seealso [eyeris::sim_params()] to configure the synthetic signal.
#'
#' @examples
#' # generate a synthetic recording and run it through the pipeline
#' sim <- simulate_eyeris(seed = 1, params = sim_params(duration_secs = 20))
#'
#' \donttest{
#' out <- sim |>
#'   eyeris::deblink() |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt() |>
#'   eyeris::zscore()
#'
#' pdf(tempfile(fileext = ".pdf"))
#' plot(out, seed = 1)
#' dev.off()
#' }
#'
#' @export
simulate_eyeris <- function(
  seed = 1L,
  params = sim_params(),
  block = 1L,
  verbose = TRUE
) {
  if (!inherits(params, "eyeris_sim_params")) {
    log_error("`params` must be created with `sim_params()`.")
  }

  fs <- params$fs
  n <- as.integer(round(params$duration_secs * fs))
  if (n < 2L) {
    log_error("`duration_secs * fs` must yield at least 2 samples.")
  }
  t <- (seq_len(n) - 1L) / fs # seconds, starting at 0
  idx <- seq_len(n)

  truth <- withr::with_seed(seed, {
    # ---- clean latent signal L(t) ----
    latent <- numeric(n)
    if (isTRUE(params$baseline)) {
      latent <- latent + params$baseline_mean
    }
    if (isTRUE(params$drift)) {
      latent <- latent - params$drift_slope * t + params$drift_curv * t^2
    }

    # RNG draws follow a FIXED order so toggling one component does not
    # reshuffle the random stream used by the others. Blink PLACEMENT is drawn
    # before transients (so transients can be placed clear of blink regions),
    # but blink artifacts are APPLIED last (after noise) so their occlusion
    # spikes and missing cores overwrite the observed signal.

    # (1) hippus phase
    hippus_phase <- stats::runif(1, 0, 2 * pi)
    if (isTRUE(params$hippus)) {
      latent <- latent +
        params$hippus_amp * sin(2 * pi * params$hippus_freq * t + hippus_phase)
    }

    # (2) phasic responses
    onsets_ms <- params$phasic_onsets_ms
    if (is.null(onsets_ms)) {
      # generate onsets from ISIs across the recording, leaving lead/tail room
      onsets_ms <- numeric(0)
      cur <- 2000
      max_ms <- params$duration_secs * 1000 - 4000
      while (cur <= max_ms) {
        onsets_ms <- c(onsets_ms, cur)
        cur <- cur +
          stats::runif(1, params$phasic_isi[1], params$phasic_isi[2]) * 1000
      }
    }
    n_ev <- length(onsets_ms)
    phasic_amps <- if (n_ev > 0) {
      stats::runif(n_ev, params$phasic_amp[1], params$phasic_amp[2])
    } else {
      numeric(0)
    }
    if (isTRUE(params$phasic) && n_ev > 0) {
      latent <- latent +
        .sim_phasic(
          t,
          onsets_ms / 1000,
          phasic_amps,
          params$phasic_n,
          params$phasic_tmax
        )
    }

    # (3) blink placement (positions/durations drawn now; applied below)
    flank_s <- as.integer(round(params$blink_flank_ms * fs / 1000))
    blink_centers <- integer(0)
    blink_cores <- integer(0)
    blink_region_plan <- logical(n)
    if (isTRUE(params$blinks) && params$n_blinks > 0) {
      blink_cores <- as.integer(round(
        stats::runif(
          params$n_blinks,
          params$blink_dur_ms[1],
          params$blink_dur_ms[2]
        ) *
          fs /
          1000
      ))
      # spread blink centers across evenly sized segments (with jitter) so they
      # do not overlap, even for many blinks in a short recording
      seg <- n / params$n_blinks
      blink_centers <- integer(params$n_blinks)
      for (b in seq_len(params$n_blinks)) {
        lo <- (b - 1) * seg + seg * 0.25
        hi <- (b - 1) * seg + seg * 0.75
        blink_centers[b] <- as.integer(round(stats::runif(1, lo, hi)))
      }
      blink_centers <- pmin(pmax(blink_centers, flank_s + 5L), n - flank_s - 5L)
      for (b in seq_along(blink_centers)) {
        half <- floor(blink_cores[b] / 2)
        lo <- max(1L, blink_centers[b] - half - flank_s)
        hi <- min(n, blink_centers[b] + half + flank_s)
        blink_region_plan[lo:hi] <- TRUE
      }
    }

    # (4) transients (isolated fast spikes; kept clear of edges and blinks)
    transient_idx <- integer(0)
    trans_sig <- numeric(n)
    if (isTRUE(params$transients) && params$n_transients > 0) {
      w <- params$transient_width_ms * fs / 1000
      margin <- as.integer(max(ceiling(6 * w), 0.05 * fs))
      eligible <- rep(TRUE, n)
      # exclude an edge margin at each end; guard against a margin so large
      # (relative to a very short recording) that the ranges would overlap or
      # produce negative subscripts
      if (2L * margin >= n) {
        eligible[] <- FALSE
        if (n > 2L) {
          eligible[2:(n - 1L)] <- TRUE
        }
      } else {
        eligible[c(seq_len(margin), (n - margin + 1L):n)] <- FALSE
      }
      # exclude blink regions plus a guard band so detransient (not deblink) is
      # unambiguously responsible for the transient
      guard <- as.integer(max(margin, flank_s + 5L))
      blocked <- which(blink_region_plan)
      if (length(blocked) > 0) {
        eligible[unique(pmin(
          pmax(
            rep(blocked, each = 2 * guard + 1) +
              rep(-guard:guard, times = length(blocked)),
            1L
          ),
          n
        ))] <- FALSE
      }
      pool <- which(eligible)
      # place as many transients as fit; on recordings too short to hold the
      # requested number clear of the margins/blinks, place fewer rather than
      # erroring out
      n_place <- min(params$n_transients, length(pool))
      if (n_place > 0) {
        transient_idx <- sort(sample(pool, n_place))
        t_amps <- stats::runif(
          n_place,
          params$transient_amp[1],
          params$transient_amp[2]
        ) *
          sample(c(-1, 1), n_place, replace = TRUE)
        for (j in seq_along(transient_idx)) {
          trans_sig <- trans_sig +
            t_amps[j] * exp(-((idx - transient_idx[j])^2) / (2 * w^2))
        }
      }
    }

    # (5) line noise
    line_phase <- stats::runif(1, 0, 2 * pi)
    line_sig <- if (isTRUE(params$line)) {
      params$line_amp * sin(2 * pi * params$line_freq * t + line_phase)
    } else {
      numeric(n)
    }

    # (6) broadband measurement noise
    noise_sig <- numeric(n)
    if (isTRUE(params$noise)) {
      e <- stats::rnorm(n, 0, params$noise_sd)
      if (params$noise_ar > 0) {
        e <- as.numeric(stats::filter(
          e,
          filter = params$noise_ar,
          method = "recursive"
        ))
      }
      noise_sig <- e
    }

    obs <- latent + trans_sig + line_sig + noise_sig

    # (7) blinks (missing cores + occlusion flank spikes), applied last
    core_mask <- logical(n)
    region_mask <- logical(n)
    blink_df <- data.frame(start = integer(0), end = integer(0))
    if (isTRUE(params$blinks) && length(blink_centers) > 0) {
      bl <- .sim_blinks(
        obs,
        blink_centers,
        blink_cores,
        flank_s,
        params$blink_depth,
        params$blink_overshoot,
        params$blink_flank_shape
      )
      obs <- bl$pupil
      core_mask <- bl$core_mask
      region_mask <- bl$region_mask
      blink_df <- bl$df
    }

    # clamp observed (non-missing) samples to the physiological range
    finite <- !is.na(obs)
    obs[finite] <- pmin(pmax(obs[finite], params$clip[1]), params$clip[2])

    # (7) scattered dropouts (extra NA runs away from blinks/transients)
    dropout_idx <- integer(0)
    if (params$dropout_frac > 0) {
      run_lo <- max(1L, as.integer(round(params$dropout_run_ms[1] * fs / 1000)))
      run_hi <- max(
        run_lo,
        as.integer(round(params$dropout_run_ms[2] * fs / 1000))
      )
      target <- as.integer(round(params$dropout_frac * n))
      protect <- region_mask
      if (length(transient_idx) > 0) {
        # keep dropouts clear of transients by more than a typical deblink
        # `extend` reach (~50 ms) so deblink cannot swallow a transient via a
        # neighboring dropout gap; this keeps detransient responsible for it
        t_guard <- as.integer(max(60, ceiling(0.06 * fs)))
        protect[unlist(lapply(transient_idx, function(ti) {
          seq.int(max(1L, ti - t_guard), min(n, ti + t_guard))
        }))] <- TRUE
      }
      placed <- 0L
      guard <- 0L
      while (placed < target && guard < 10L * target + 100L) {
        guard <- guard + 1L
        s <- sample.int(n, 1L)
        len <- sample.int(run_hi - run_lo + 1L, 1L) + run_lo - 1L
        e <- min(n, s + len - 1L)
        run <- s:e
        if (any(protect[run])) {
          next
        }
        obs[run] <- NA_real_
        protect[run] <- TRUE
        dropout_idx <- c(dropout_idx, run)
        placed <- placed + length(run)
      }
    }

    # gaze coordinates: a gentle random walk, missing during blink cores
    ex <- 960 + cumsum(stats::rnorm(n, 0, 0.3))
    ey <- 540 + cumsum(stats::rnorm(n, 0, 0.3))
    ex <- pmin(pmax(ex, 0), 1920)
    ey <- pmin(pmax(ey, 0), 1080)
    ex[core_mask] <- NA_real_
    ey[core_mask] <- NA_real_

    list(
      pupil = obs,
      clean = latent,
      eye_x = ex,
      eye_y = ey,
      core_mask = core_mask,
      region_mask = region_mask,
      transient_idx = transient_idx,
      dropout_idx = sort(unique(dropout_idx)),
      onsets_ms = onsets_ms,
      blink_df = blink_df
    )
  })

  # ---- assemble an eyelinker-shaped object and delegate to the loader ----
  t0 <- 1000000L
  time_ms <- as.integer(seq(t0, by = 1000L / fs, length.out = n))

  x <- list()
  x$raw <- data.frame(
    block = 1L,
    time = time_ms,
    ps = truth$pupil,
    xp = truth$eye_x,
    yp = truth$eye_y
  )
  onset_times <- t0 +
    as.integer(round(truth$onsets_ms * fs / 1000)) * (1000L / fs)
  n_ev <- length(onset_times)
  x$msg <- data.frame(
    block = rep(1L, n_ev),
    time = as.integer(onset_times),
    text = if (n_ev > 0) paste0("STIM_ONSET_", seq_len(n_ev)) else character(0),
    stringsAsFactors = FALSE
  )
  n_bl <- nrow(truth$blink_df)
  x$blinks <- data.frame(
    block = rep(1L, n_bl),
    stime = time_ms[pmax(truth$blink_df$start, 1L)],
    etime = time_ms[pmin(truth$blink_df$end, n)],
    dur = (truth$blink_df$end - truth$blink_df$start) * (1000L / fs),
    eye = rep(params$eye, n_bl),
    stringsAsFactors = FALSE
  )
  x$info <- data.frame(
    date = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    model = "EyeLink 1000 (Simulated)",
    version = "eyeris-sim 1.0",
    sample.rate = fs,
    cr = TRUE,
    left = identical(params$eye, "L"),
    right = identical(params$eye, "R"),
    mono = TRUE,
    screen.x = 1920,
    screen.y = 1080,
    mount = "Simulated / Monocular",
    filter.level = 0,
    sample.dtype = "GAZE",
    event.dtype = "GAZE",
    pupil.dtype = params$pupil_dtype,
    velocity = FALSE,
    resolution = FALSE,
    htarg = FALSE,
    input = FALSE,
    buttons = FALSE,
    stringsAsFactors = FALSE
  )

  obj <- process_eyeris_data(
    x,
    block = block,
    eye = if (identical(params$eye, "L")) "left" else "right",
    hz = fs,
    pupil_type = tolower(params$pupil_dtype),
    file = sprintf("simulated_seed-%d.asc", as.integer(seed)),
    binoc = FALSE,
    binoc_mode = NULL
  )

  attr(obj, "sim_truth") <- list(
    clean = truth$clean,
    pupil = truth$pupil,
    core_mask = truth$core_mask,
    region_mask = truth$region_mask,
    transient_idx = truth$transient_idx,
    dropout_idx = truth$dropout_idx,
    event_onsets_ms = truth$onsets_ms,
    time_secs = t,
    params = params,
    seed = as.integer(seed)
  )

  if (isTRUE(verbose)) {
    n_na <- sum(is.na(truth$pupil))
    log_info(
      sprintf(
        "Simulated %.1f s @ %d Hz (%d samples, %.2f%% missing, %d blinks, %d transients).",
        params$duration_secs,
        as.integer(fs),
        n,
        100 * n_na / n,
        nrow(truth$blink_df),
        length(truth$transient_idx)
      ),
      verbose = verbose
    )
  }

  obj
}
