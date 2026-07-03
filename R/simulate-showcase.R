#' Educational before/after demonstrations of preprocessing-step ordering
#'
#' @description
#' `eyeris` preprocessing steps must be run in a particular order, and running
#' them out of order can silently corrupt the pupil signal. These showcase
#' helpers generate a synthetic recording with [eyeris::simulate_eyeris()] whose
#' features are deliberately designed to expose a specific ordering pitfall, run
#' the pipeline both the *wrong* way and the *right* way on the exact same data,
#' and quantify the difference so the lesson is unmistakable.
#'
#' @details
#' Because [eyeris::simulate_eyeris()] returns a fully valid `eyeris` object, the
#' wrong- and right-order pipelines are realized by calling the ordinary
#' exported step functions (e.g. [eyeris::deblink()], [eyeris::detransient()])
#' directly --- so the only thing that differs between the two runs is the order
#' of the steps, never the underlying signal.
#'
#' The bundled pitfalls (see the `name` argument of [eyeris::sim_pitfall()]) are:
#' \describe{
#'   \item{`interp_before_deblink`}{Interpolating before deblinking. Blinks are
#'     runs of missing data flanked by rapid partial-occlusion spikes.
#'     Interpolation only bridges the missing core; if you interpolate first,
#'     the flank spikes survive as large spurious deflections that deblinking
#'     can no longer remove.}
#'   \item{`lpfilt_before_interp`}{Low-pass filtering before interpolating.
#'     [eyeris::lpfilt()] requires a gap-free signal and errors on any missing
#'     sample --- a guardrail demonstrating that interpolation must come first.}
#'   \item{`lpfilt_before_detransient`}{Low-pass filtering before removing
#'     transient spikes. [eyeris::detransient()] identifies spikes by their high
#'     sample-to-sample speed; once the signal has been smoothed, that tell-tale
#'     speed is gone and the detector can no longer flag the artifacts.}
#'   \item{`naive_downsample`}{Decimating without an anti-alias filter. Picking
#'     one sample out of every k folds high-frequency content into the pupil band
#'     as a spurious low-frequency oscillation; [eyeris::downsample()] applies an
#'     anti-alias filter first and avoids it.}
#'   \item{`omit_detrend`}{Standardizing a signal that carries a slow linear
#'     drift without detrending it first. The drift survives z-scoring and
#'     contaminates the standardized signal; [eyeris::detrend()] removes it.}
#' }
#'
#' A note on step *ordering* versus step *presence*: `eyeris` intentionally runs
#' [eyeris::deblink()] before [eyeris::detransient()]. With blinks encoded as
#' missing data, however, this ordering has only a subtle numerical effect
#' (deblinking's `extend` padding removes the blink flanks either way, and the
#' median-based transient threshold is robust to the sparse flank samples), so
#' it is documented here rather than dramatized as a before/after metric that
#' would overstate the effect.
#'
#' @name eyeris-showcase
#' @seealso [eyeris::simulate_eyeris()], [eyeris::sim_params()]
NULL

# ---- internal helpers -------------------------------------------------------

#' Run an expression while suppressing routine step messages/warnings
#' @param expr Expression to evaluate
#' @return The value of `expr`
#' @keywords internal
.quiet <- function(expr) {
  suppressWarnings(suppressMessages(expr))
}

#' The single simulated block of an eyeris object
#' @param o An `eyeris` object with a `block_1` time series
#' @return The `block_1` data frame
#' @keywords internal
.blk <- function(o) o$timeseries[["block_1"]]

#' Name of the most recent pupil column
#' @param o An `eyeris` object
#' @return A single column-name string
#' @keywords internal
.latest_name <- function(o) {
  lt <- o$latest
  if (is.list(lt)) lt[["block_1"]] else lt
}

#' The most recent pupil column values
#' @param o An `eyeris` object
#' @return A numeric vector
#' @keywords internal
.latest_col <- function(o) .blk(o)[[.latest_name(o)]]

#' Count how many true transients a detransient step flagged
#'
#' @param before An `eyeris` object immediately before a [eyeris::detransient()]
#'   step
#' @param transient_idx Integer indices of the true transient centers
#' @param tol Tolerance (in samples) around each transient center
#'
#' @return The number of transients newly set to `NA` by detransient
#'
#' @keywords internal
.count_flagged <- function(before, transient_idx, tol = 3L) {
  after <- .quiet(detransient(before))
  b <- .latest_col(before)
  a <- .latest_col(after)
  newly <- which(is.na(a) & !is.na(b))
  if (length(newly) == 0 || length(transient_idx) == 0) {
    return(0L)
  }
  sum(vapply(
    transient_idx,
    function(ti) any(abs(newly - ti) <= tol),
    logical(1)
  ))
}

#' Decimate a signal by simple sample-picking, with NO anti-alias filter
#'
#' Produces an `eyeris` object structurally identical to the output of
#' [eyeris::downsample()] but whose decimated pupil column is a naive
#' decimation that keeps one sample out of every k (no anti-alias low-pass).
#' This is a
#' deliberately incorrect baseline used only to demonstrate aliasing; no
#' `eyeris` preprocessing function decimates without anti-aliasing.
#'
#' @param eyeris An `eyeris` object (must be gap-free / interpolated)
#' @param target_fs Target sampling rate in Hz
#'
#' @return An `eyeris` object with a naively decimated pupil column
#'
#' @keywords internal
.naive_decimate <- function(eyeris, target_fs) {
  prev_op <- .latest_name(eyeris)
  full <- .blk(eyeris)
  factor <- eyeris$info$sample.rate / target_fs
  indices <- seq(1, nrow(full), by = factor)
  # borrow the correct decimated structure from the real downsample(), then
  # overwrite the values with the naive (unfiltered) decimation
  ds <- .quiet(downsample(eyeris, target_fs = target_fs, plot_freqz = FALSE))
  new_col <- .latest_name(ds)
  ds$timeseries[["block_1"]][[new_col]] <- full[[prev_op]][indices]
  ds
}

#' Effective sampling rate of an eyeris object (post-decimation aware)
#' @param o An `eyeris` object
#' @return A single numeric sampling rate in Hz
#' @keywords internal
.eff_fs <- function(o) {
  if (!is.null(o$decimated.sample.rate) && !is.na(o$decimated.sample.rate)) {
    o$decimated.sample.rate
  } else {
    o$info$sample.rate
  }
}

#' Names of the bundled ordering pitfalls
#' @return A character vector of pitfall names
#' @keywords internal
.pitfall_names <- function() {
  c(
    "interp_before_deblink",
    "lpfilt_before_interp",
    "lpfilt_before_detransient",
    "naive_downsample",
    "omit_detrend"
  )
}

#' Scenario-tuned default parameters for a pitfall
#' @param name A pitfall name
#' @return An `eyeris_sim_params` object
#' @keywords internal
.pitfall_params <- function(name) {
  switch(
    name,
    interp_before_deblink = sim_params(duration_secs = 30, transients = FALSE),
    lpfilt_before_interp = sim_params(duration_secs = 30),
    lpfilt_before_detransient = sim_params(duration_secs = 30),
    naive_downsample = sim_params(
      duration_secs = 30,
      line = TRUE,
      line_freq = 96,
      line_amp = 25
    ),
    omit_detrend = sim_params(
      duration_secs = 40,
      drift = TRUE,
      drift_slope = 8
    ),
    log_error("Unknown pitfall: {name}")
  )
}

#' Realize the wrong- and right-order pipelines for a pitfall
#' @param name A pitfall name
#' @param sim A simulated `eyeris` object
#' @return A list describing the two runs (objects, step labels, extras)
#' @keywords internal
.run_pitfall <- function(name, sim) {
  lp <- function(o) lpfilt(o, plot_freqz = FALSE)

  if (name == "interp_before_deblink") {
    right <- .quiet(sim |> deblink() |> detransient() |> interpolate() |> lp())
    wrong <- .quiet(
      sim |>
        interpolate() |>
        deblink() |>
        detransient() |>
        interpolate() |>
        lp()
    )
    return(list(
      wrong = wrong,
      right = right,
      wrong_steps = c(
        "interpolate",
        "deblink",
        "detransient",
        "interpolate",
        "lpfilt"
      ),
      right_steps = c("deblink", "detransient", "interpolate", "lpfilt"),
      extra = list()
    ))
  }

  if (name == "lpfilt_before_interp") {
    right <- .quiet(sim |> deblink() |> detransient() |> interpolate() |> lp())
    wrong <- tryCatch(
      .quiet(sim |> deblink() |> detransient() |> lp()),
      error = function(e) e
    )
    return(list(
      wrong = wrong,
      right = right,
      wrong_steps = c("deblink", "detransient", "lpfilt"),
      right_steps = c("deblink", "detransient", "interpolate", "lpfilt"),
      extra = list(
        wrong_errored = inherits(wrong, "condition"),
        error_message = if (inherits(wrong, "condition")) {
          conditionMessage(wrong)
        } else {
          NA_character_
        }
      )
    ))
  }

  if (name == "lpfilt_before_detransient") {
    truth <- attr(sim, "sim_truth")
    tol <- as.integer(max(
      3L,
      ceiling(3 * truth$params$transient_width_ms * sim$info$sample.rate / 1000)
    ))
    right <- .quiet(sim |> deblink() |> detransient() |> interpolate() |> lp())
    wrong <- .quiet(sim |> deblink() |> interpolate() |> lp() |> detransient())
    flags_right <- .count_flagged(
      .quiet(sim |> deblink()),
      truth$transient_idx,
      tol
    )
    flags_wrong <- .count_flagged(
      .quiet(sim |> deblink() |> interpolate() |> lp()),
      truth$transient_idx,
      tol
    )
    return(list(
      wrong = wrong,
      right = right,
      wrong_steps = c("deblink", "interpolate", "lpfilt", "detransient"),
      right_steps = c("deblink", "detransient", "interpolate", "lpfilt"),
      extra = list(
        flags_right = flags_right,
        flags_wrong = flags_wrong,
        n_transients = length(truth$transient_idx)
      )
    ))
  }

  if (name == "naive_downsample") {
    target_fs <- 100
    base <- .quiet(sim |> deblink() |> detransient() |> interpolate())
    right <- .quiet(downsample(base, target_fs = target_fs, plot_freqz = FALSE))
    wrong <- .naive_decimate(base, target_fs)
    return(list(
      wrong = wrong,
      right = right,
      wrong_steps = c(
        "deblink",
        "detransient",
        "interpolate",
        "decimate (naive)"
      ),
      right_steps = c(
        "deblink",
        "detransient",
        "interpolate",
        "downsample (anti-aliased)"
      ),
      extra = list(target_fs = target_fs)
    ))
  }

  if (name == "omit_detrend") {
    right <- .quiet(
      sim |>
        deblink() |>
        detransient() |>
        interpolate() |>
        lp() |>
        detrend() |>
        zscore()
    )
    wrong <- .quiet(
      sim |> deblink() |> detransient() |> interpolate() |> lp() |> zscore()
    )
    recovered <- tryCatch(
      right$detrend_coefs[["block_1"]][[2]],
      error = function(e) NA_real_
    )
    return(list(
      wrong = wrong,
      right = right,
      wrong_steps = c(
        "deblink",
        "detransient",
        "interpolate",
        "lpfilt",
        "zscore"
      ),
      right_steps = c(
        "deblink",
        "detransient",
        "interpolate",
        "lpfilt",
        "detrend",
        "zscore"
      ),
      extra = list(recovered_slope = recovered)
    ))
  }

  log_error("Unknown pitfall: {name}")
}

#' Compute the quantitative metric table for a pitfall run
#' @param name A pitfall name
#' @param sim The simulated `eyeris` object
#' @param res The result of [.run_pitfall()]
#' @return A data frame with columns `metric`, `wrong`, `right`, `better`
#' @keywords internal
.pitfall_metrics <- function(name, sim, res) {
  truth <- attr(sim, "sim_truth")

  if (name == "interp_before_deblink") {
    mask <- truth$region_mask & !truth$core_mask
    resid <- function(o) {
      max(abs(.latest_col(o)[mask] - truth$clean[mask]), na.rm = TRUE)
    }
    return(data.frame(
      metric = "max residual at blink flanks (a.u.)",
      wrong = resid(res$wrong),
      right = resid(res$right),
      better = "lower",
      stringsAsFactors = FALSE
    ))
  }

  if (name == "lpfilt_before_interp") {
    return(data.frame(
      metric = "lpfilt completed (1 = ok, 0 = crashed on NA)",
      wrong = if (isTRUE(res$extra$wrong_errored)) 0 else 1,
      right = 1,
      better = "higher",
      stringsAsFactors = FALSE
    ))
  }

  if (name == "lpfilt_before_detransient") {
    return(data.frame(
      metric = sprintf(
        "transients flagged by detransient (of %d)",
        res$extra$n_transients
      ),
      wrong = res$extra$flags_wrong,
      right = res$extra$flags_right,
      better = "higher",
      stringsAsFactors = FALSE
    ))
  }

  if (name == "naive_downsample") {
    tfs <- res$extra$target_fs
    lf <- truth$params$line_freq
    alias <- lf %% tfs
    if (alias > tfs / 2) {
      alias <- tfs - alias
    }
    band_power <- function(o) {
      v <- .latest_col(o)
      v <- v - mean(v, na.rm = TRUE)
      pg <- stats::spec.pgram(
        v,
        taper = 0,
        fast = FALSE,
        detrend = FALSE,
        plot = FALSE
      )
      fr <- pg$freq * tfs
      sum(pg$spec[fr >= alias - 0.6 & fr <= alias + 0.6])
    }
    return(data.frame(
      metric = sprintf("aliased %.0f Hz band power (a.u.^2)", alias),
      wrong = band_power(res$wrong),
      right = band_power(res$right),
      better = "lower",
      stringsAsFactors = FALSE
    ))
  }

  if (name == "omit_detrend") {
    slope <- function(o) {
      d <- .blk(o)
      abs(unname(stats::coef(stats::lm(.latest_col(o) ~ d$time_secs))[2]))
    }
    return(data.frame(
      metric = "residual linear slope of final signal (per s)",
      wrong = slope(res$wrong),
      right = slope(res$right),
      better = "lower",
      stringsAsFactors = FALSE
    ))
  }

  log_error("Unknown pitfall: {name}")
}

#' Default preview window (seconds) bracketing a pitfall's key feature
#' @param name A pitfall name
#' @param sim The simulated `eyeris` object
#' @param res The result of [.run_pitfall()]
#' @return A length-2 numeric `c(start, end)` in seconds
#' @keywords internal
.pitfall_window <- function(name, sim, res) {
  truth <- attr(sim, "sim_truth")
  dur <- truth$params$duration_secs
  ts <- truth$time_secs
  clamp <- function(w) c(max(0, w[1]), min(dur, w[2]))

  if (name %in% c("interp_before_deblink", "lpfilt_before_interp")) {
    i <- which(truth$core_mask)[1]
    if (is.na(i)) {
      return(c(0, min(dur, 4)))
    }
    return(clamp(c(ts[i] - 0.6, ts[i] + 0.6)))
  }
  if (name == "lpfilt_before_detransient") {
    i <- truth$transient_idx[1]
    if (is.na(i)) {
      return(c(0, min(dur, 4)))
    }
    return(clamp(c(ts[i] - 0.4, ts[i] + 0.4)))
  }
  if (name == "naive_downsample") {
    return(clamp(c(dur / 2, dur / 2 + 1.5)))
  }
  # omit_detrend: show the whole recording so the drift is visible
  c(0, dur)
}

#' One-line metric headline for a showcase
#' @param x An `eyeris_showcase` object
#' @return A single character string
#' @keywords internal
.metric_headline <- function(x) {
  m <- x$metrics[1, ]
  sprintf("%s  —  wrong = %.4g, right = %.4g", m$metric, m$wrong, m$right)
}

# ---- exported entrypoints ---------------------------------------------------

#' Demonstrate what a single preprocessing step does to synthetic pupil data
#'
#' @description
#' Generates a synthetic recording with [eyeris::simulate_eyeris()], runs the
#' canonical pipeline up to and including the requested `step`, and plots the
#' before/after of each step (via [eyeris::plot.eyeris()]) on a preview window
#' chosen to bracket the artifact that `step` targets.
#'
#' @param step Which preprocessing step to showcase. One of `"deblink"`,
#'   `"detransient"`, `"interpolate"`, `"lpfilt"`, `"downsample"`, `"detrend"`,
#'   or `"zscore"`
#' @param seed Integer random seed passed to [eyeris::simulate_eyeris()].
#'   Defaults to `1`
#' @param params A parameter list from [eyeris::sim_params()]. Defaults to
#'   `sim_params()`
#' @param preview_window Optional length-2 numeric `c(start, end)` in seconds. If
#'   `NULL` (default), a window bracketing the relevant artifact is chosen
#'   automatically
#' @param verbose Logical; print status messages. Defaults to `TRUE`
#'
#' @return Invisibly, a list with the `eyeris` object (`obj`), the applied
#'   `steps`, and the preview `window`.
#'
#' @seealso [eyeris::sim_pitfall()] for wrong-vs-right step-ordering
#'   demonstrations.
#'
#' @examples
#' \donttest{
#' pdf(tempfile(fileext = ".pdf"))
#' sim_step_showcase("deblink", seed = 1)
#' dev.off()
#' }
#'
#' @export
sim_step_showcase <- function(
  step = c(
    "deblink",
    "detransient",
    "interpolate",
    "lpfilt",
    "downsample",
    "detrend",
    "zscore"
  ),
  seed = 1L,
  params = sim_params(),
  preview_window = NULL,
  verbose = TRUE
) {
  step <- match.arg(step)
  sim <- simulate_eyeris(seed = seed, params = params, verbose = FALSE)
  lp <- function(o) lpfilt(o, plot_freqz = FALSE)

  obj <- .quiet({
    o <- sim |> deblink()
    if (step == "deblink") {
      o
    } else {
      o <- o |> detransient()
      if (step == "detransient") {
        o
      } else {
        o <- o |> interpolate()
        if (step == "interpolate") {
          o
        } else {
          o <- o |> lp()
          if (step == "lpfilt") {
            o
          } else if (step == "downsample") {
            o |> downsample(target_fs = 100, plot_freqz = FALSE)
          } else if (step == "detrend") {
            o |> detrend()
          } else {
            o |> zscore()
          }
        }
      }
    }
  })

  truth <- attr(sim, "sim_truth")
  if (is.null(preview_window)) {
    ts <- truth$time_secs
    dur <- truth$params$duration_secs
    if (step %in% c("detrend", "zscore", "downsample")) {
      preview_window <- c(0, dur)
    } else {
      i <- which(truth$core_mask)[1]
      if (is.na(i)) {
        i <- round(length(ts) / 3)
      }
      preview_window <- c(max(0, ts[i] - 0.6), min(dur, ts[i] + 0.6))
    }
  }

  if (isTRUE(verbose)) {
    log_info(
      "Showcasing the `{step}` step (preview {round(preview_window[1], 2)}-{round(preview_window[2], 2)} s).",
      verbose = verbose
    )
  }

  plot(obj, seed = seed, preview_window = preview_window, verbose = FALSE)
  invisible(list(obj = obj, step = step, window = preview_window))
}

#' Demonstrate a preprocessing-step ordering pitfall (wrong vs right)
#'
#' @description
#' Generates a synthetic recording tuned to expose a specific ordering pitfall,
#' runs the pipeline both the wrong way and the right way on the same data, and
#' returns an `eyeris_showcase` object carrying both results and a quantitative
#' metric that captures the damage the wrong order causes. See
#' [eyeris-showcase] for the catalog of pitfalls.
#'
#' @param name Which pitfall to demonstrate. One of `"interp_before_deblink"`,
#'   `"lpfilt_before_interp"`, `"lpfilt_before_detransient"`,
#'   `"naive_downsample"`, or `"omit_detrend"`
#' @param seed Integer random seed passed to [eyeris::simulate_eyeris()].
#'   Defaults to `1`
#' @param params Optional parameter list from [eyeris::sim_params()]. When
#'   `NULL` (default), scenario-tuned parameters are used so the pitfall is
#'   clearly exposed
#' @param plot Logical; draw the wrong-vs-right comparison. Defaults to `TRUE`
#' @param preview_window Optional length-2 numeric `c(start, end)` in seconds. If
#'   `NULL` (default), a window bracketing the key feature is chosen
#'   automatically
#' @param verbose Logical; print a short summary. Defaults to `TRUE`
#'
#' @return An object of class `eyeris_showcase` (a list) with elements: `name`,
#'   `seed`, `params`, `sim` (the simulated object), `wrong` (the wrong-order
#'   `eyeris` object, or the captured error condition for the crash guardrail),
#'   `right` (the right-order `eyeris` object), `wrong_steps`/`right_steps`
#'   (applied step orders), `metrics` (a data frame), `window`, and `extra`.
#'
#' @seealso [eyeris::sim_step_showcase()], [eyeris::simulate_eyeris()],
#'   [eyeris::showcase_metric()]
#'
#' @examples
#' # compute the metric without plotting
#' res <- sim_pitfall("interp_before_deblink", seed = 1, plot = FALSE)
#' res$metrics
#'
#' \donttest{
#' pdf(tempfile(fileext = ".pdf"))
#' sim_pitfall("lpfilt_before_detransient", seed = 1)
#' dev.off()
#' }
#'
#' @export
sim_pitfall <- function(
  name = c(
    "interp_before_deblink",
    "lpfilt_before_interp",
    "lpfilt_before_detransient",
    "naive_downsample",
    "omit_detrend"
  ),
  seed = 1L,
  params = NULL,
  plot = TRUE,
  preview_window = NULL,
  verbose = TRUE
) {
  name <- match.arg(name)
  if (is.null(params)) {
    params <- .pitfall_params(name)
  }
  if (!inherits(params, "eyeris_sim_params")) {
    log_error("`params` must be created with `sim_params()`.")
  }

  sim <- simulate_eyeris(seed = seed, params = params, verbose = FALSE)
  res <- .run_pitfall(name, sim)
  metrics <- .pitfall_metrics(name, sim, res)
  window <- if (is.null(preview_window)) {
    .pitfall_window(name, sim, res)
  } else {
    preview_window
  }

  out <- structure(
    list(
      name = name,
      seed = as.integer(seed),
      params = params,
      sim = sim,
      wrong = res$wrong,
      right = res$right,
      wrong_steps = res$wrong_steps,
      right_steps = res$right_steps,
      metrics = metrics,
      window = window,
      extra = res$extra
    ),
    class = "eyeris_showcase"
  )

  if (isTRUE(verbose)) {
    print(out)
  }
  if (isTRUE(plot)) {
    plot(out)
  }

  invisible(out)
}

#' Quantitative metric(s) for a preprocessing-step ordering pitfall
#'
#' @description
#' A convenience wrapper that runs [eyeris::sim_pitfall()] without plotting and
#' returns just its metric table --- useful for programmatic checks and tests.
#'
#' @param name Which pitfall to evaluate (see [eyeris::sim_pitfall()])
#' @param seed Integer random seed. Defaults to `1`
#' @param params Optional [eyeris::sim_params()] list; `NULL` uses scenario-tuned
#'   defaults
#'
#' @return A data frame with columns `metric`, `wrong`, `right`, and `better`
#'   (whether a `"lower"` or `"higher"` value is the desired outcome).
#'
#' @seealso [eyeris::sim_pitfall()]
#'
#' @examples
#' showcase_metric("naive_downsample", seed = 1)
#'
#' @export
showcase_metric <- function(name, seed = 1L, params = NULL) {
  res <- sim_pitfall(
    name = name,
    seed = seed,
    params = params,
    plot = FALSE,
    verbose = FALSE
  )
  res$metrics
}

#' Print a preprocessing-step ordering showcase
#'
#' @param x An object of class `eyeris_showcase`
#' @param ... Unused; included for S3 consistency
#'
#' @return `x`, invisibly
#'
#' @export
print.eyeris_showcase <- function(x, ...) {
  cli::cli_h2("eyeris ordering pitfall: {.strong {x$name}}")
  cli::cli_text(
    "{.field WRONG order}: {paste(x$wrong_steps, collapse = ' → ')}"
  )
  cli::cli_text(
    "{.field RIGHT order}: {paste(x$right_steps, collapse = ' → ')}"
  )
  cli::cli_text("")
  m <- x$metrics
  for (i in seq_len(nrow(m))) {
    better_side <- if (m$better[i] == "lower") {
      if (m$right[i] <= m$wrong[i]) "right" else "wrong"
    } else {
      if (m$right[i] >= m$wrong[i]) "right" else "wrong"
    }
    cli::cli_text(
      "{.field {m$metric[i]}}: wrong = {.val {round(m$wrong[i], 4)}}, ",
      "right = {.val {round(m$right[i], 4)}} ",
      "({.emph {m$better[i]} is better → {better_side} order wins})"
    )
  }
  if (isTRUE(x$extra$wrong_errored)) {
    cli::cli_text("")
    cli::cli_alert_warning("Wrong order crashed: {x$extra$error_message}")
  }
  invisible(x)
}

#' Plot a preprocessing-step ordering showcase (wrong vs right)
#'
#' @description
#' Draws a stacked two-panel comparison of the final pupil signal produced by
#' the wrong-order pipeline (top) and the right-order pipeline (bottom) over a
#' shared preview window, annotated with the quantitative metric. For the
#' crash-guardrail pitfall, the top panel reports the captured error instead.
#'
#' @param x An object of class `eyeris_showcase`
#' @param ... Unused; included for S3 consistency
#'
#' @return `x`, invisibly
#'
#' @export
plot.eyeris_showcase <- function(x, ...) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

  panel <- function(o, main, col) {
    d <- .blk(o)
    sel <- d$time_secs >= x$window[1] & d$time_secs <= x$window[2]
    v <- .latest_col(o)
    if (sum(sel, na.rm = TRUE) < 2) {
      sel <- rep(TRUE, nrow(d))
    }
    plot(
      d$time_secs[sel],
      v[sel],
      type = "l",
      col = col,
      lwd = 1.4,
      xlab = "time (s)",
      ylab = "pupil",
      main = main
    )
  }

  wrong_lab <- paste0("WRONG: ", paste(x$wrong_steps, collapse = " → "))
  right_lab <- paste0("RIGHT: ", paste(x$right_steps, collapse = " → "))

  if (inherits(x$wrong, "condition")) {
    plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, main = wrong_lab)
    graphics::text(
      0,
      0,
      labels = paste0(
        "pipeline crashed on missing data:\n",
        conditionMessage(x$wrong)
      ),
      col = "#E41A1C"
    )
  } else {
    panel(x$wrong, wrong_lab, "#E41A1C")
  }
  panel(x$right, right_lab, "#377EB8")

  graphics::mtext(
    paste0("eyeris pitfall — ", x$name, "\n", .metric_headline(x)),
    outer = TRUE,
    cex = 1.05,
    font = 2
  )
  invisible(x)
}
