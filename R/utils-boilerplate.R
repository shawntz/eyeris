#' Generate a reproducible, copy-and-paste-ready methods boilerplate
#'
#' @description
#' Auto-generates human-readable, "fMRIPrep-style" methods-section boilerplate
#' text that describes -- in plain prose -- the exact preprocessing workflow
#' that was run on an `eyeris` object. Because every `eyeris` preprocessing step
#' records its own call stack and parameters (in `eyeris$params`), the pipeline
#' is able to self-document precisely what it did to your data, ready to paste
#' directly into the methods section of a manuscript.
#'
#' This reinforces the workflow-level transparency at the heart of the `eyeris`
#' "glassbox" philosophy: the code that ran and the prose that describes it are
#' generated from one and the same source of truth.
#'
#' @details
#' The generated text walks the recorded pipeline steps in the canonical order
#' in which `eyeris` applies them (`load_asc` -> `deblink` -> `detransient` ->
#' `interpolate` -> `lpfilt` -> `downsample`/`bin` -> `detrend` -> `zscore` ->
#' `epoch`) and emits one sentence per step, substituting in the actual
#' parameter values that were used. Steps that were skipped are omitted, and any
#' custom (user-supplied) pipeline extensions are appended at the end so that
#' multi-step and non-default pipelines are described faithfully. Multi-block
#' (multi-run) and binocular recordings are handled automatically.
#'
#' The boilerplate also references the per-run, machine-readable `.json`
#' metadata sidecar that `eyeris` writes for every run
#' (`source/logs/run-XX_metadata.json`), which together with the reported
#' package version is sufficient to reproduce the pipeline exactly.
#'
#' **License & attribution.** The auto-generated boilerplate text is licensed
#' under the Creative Commons Attribution 4.0 International (CC BY 4.0) license
#' (<https://creativecommons.org/licenses/by/4.0/>). It is explicitly safe to
#' copy and paste the generated Markdown content directly into your manuscript,
#' provided that you give appropriate credit by citing `eyeris` (run
#' `citation("eyeris")` for the reference); including that citation in your
#' references satisfies the attribution requirement. This mirrors the approach
#' taken by fMRIPrep and other reproducible-pipeline tools.
#'
#' When you run [eyeris::bidsify()] (or [eyeris::glassbox()] with reporting
#' enabled), this same boilerplate is embedded in the diagnostic HTML report and
#' written to `derivatives/.../source/logs/` as a standalone Markdown file.
#'
#' @param eyeris An object of class `eyeris` (e.g., the output of
#' [eyeris::glassbox()]). Binocular objects are supported.
#' @param version Optional character string giving the `eyeris` version to cite
#' in the boilerplate. Defaults to `NULL`, in which case the currently installed
#' `eyeris` version is used.
#' @param include_citation Logical. Whether to append a formatted citation for
#' `eyeris`. Defaults to `TRUE`.
#' @param include_license Logical. Whether to append the Creative Commons CC BY
#' 4.0 license note telling users it is safe to paste the text into a manuscript
#' as long as they cite `eyeris` for attribution. Defaults to `TRUE`.
#'
#' @return A length-one character string of Markdown-formatted methods
#' boilerplate (invisibly classed as `eyeris_boilerplate` so it prints nicely at
#' the console). The raw string can be passed to [base::cat()],
#' [base::writeLines()], or embedded directly in a report.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' eyeris_obj <- demo_data |>
#'   eyeris::glassbox(verbose = FALSE)
#'
#' # print the copy-and-paste-ready methods boilerplate
#' boilerplate(eyeris_obj)
#'
#' # capture it as plain Markdown text (e.g., to write to a file)
#' methods_md <- boilerplate(eyeris_obj)
#' cat(methods_md)
#'
#' @seealso [eyeris::glassbox()], [eyeris::bidsify()]
#'
#' @export
boilerplate <- function(
  eyeris,
  version = NULL,
  include_citation = TRUE,
  include_license = TRUE
) {
  if (!inherits(eyeris, "eyeris")) {
    log_error("`boilerplate()` requires an object of class `eyeris`.")
  }

  md <- build_boilerplate_md(
    eyeris = eyeris,
    version = version,
    include_citation = include_citation,
    include_license = include_license
  )

  class(md) <- c("eyeris_boilerplate", "character")
  md
}

#' Print method for `eyeris` methods boilerplate
#'
#' Renders the Markdown boilerplate text to the console as-is (rather than as a
#' single escaped string).
#'
#' @param x An `eyeris_boilerplate` object returned by [eyeris::boilerplate()]
#' @param ... Unused; included for S3 consistency
#'
#' @return The input `x`, invisibly
#'
#' @keywords internal
#'
#' @export
print.eyeris_boilerplate <- function(x, ...) {
  cat(unclass(x))
  cat("\n")
  invisible(x)
}

#' Build the Markdown methods boilerplate for an eyeris object
#'
#' Internal worker that assembles the full Markdown boilerplate body (note,
#' methods prose, reproducibility/sidecar reference, citation, and license).
#' Used both by the exported [eyeris::boilerplate()] function and by the HTML
#' report generator.
#'
#' @param eyeris An `eyeris` object
#' @param version Optional `eyeris` version string; defaults to the installed
#' version
#' @param n_runs Optional integer number of recording blocks/runs; inferred from
#' the object when `NULL`
#' @param include_citation Logical; whether to append a citation
#' @param include_license Logical; whether to append the CC BY 4.0 license note
#'
#' @return A length-one character string of Markdown
#'
#' @keywords internal
build_boilerplate_md <- function(
  eyeris,
  version = NULL,
  n_runs = NULL,
  include_citation = TRUE,
  include_license = TRUE
) {
  is_binoc <- is_binocular_object(eyeris)
  src <- if (is_binoc) eyeris$left else eyeris

  params <- src$params
  info <- src$info

  if (is.null(version) || is.na(version) || !nzchar(version)) {
    version <- tryCatch(
      as.character(utils::packageVersion("eyeris")),
      error = function(e) NA_character_
    )
  }

  if (is.null(n_runs)) {
    n_runs <- if (is.list(src$timeseries) && !is.data.frame(src$timeseries)) {
      length(src$timeseries)
    } else {
      1L
    }
  }

  # software intro sentence
  version_str <- if (!is.na(version) && nzchar(version)) {
    sprintf(" (version %s)", version)
  } else {
    ""
  }
  intro <- sprintf(
    paste0(
      "Pupillometry data were preprocessed in R using the `eyeris` package%s ",
      "(Schwartz et al., 2025), which implements a fully transparent, ",
      "parameterized \"glassbox\" preprocessing pipeline."
    ),
    version_str
  )

  # optional multi-run lead-in
  lead_in <- if (!is.null(n_runs) && n_runs > 1) {
    sprintf(
      paste0(
        "The pipeline described below was applied identically to each of the ",
        "%d recording blocks (runs)."
      ),
      n_runs
    )
  } else {
    NULL
  }

  step_sentences <- describe_boilerplate_steps(params, info)

  if (length(step_sentences) == 0) {
    step_sentences <- paste0(
      "No preprocessing steps were recorded for this object; please ensure the ",
      "pipeline was run via `glassbox()` (or the individual `eyeris` step ",
      "functions) before generating a methods boilerplate."
    )
  }

  binoc_note <- if (is_binoc) {
    paste0(
      "Because the source recording was binocular, the left and right eyes ",
      "were preprocessed independently through the identical pipeline ",
      "described above."
    )
  } else {
    NULL
  }

  methods_paragraph <- paste(
    c(intro, lead_in, step_sentences, binoc_note),
    collapse = " "
  )

  # top, copy-and-paste note
  note <- paste0(
    "> **Copy-and-paste-ready methods text.** The Markdown below was generated ",
    "automatically by `eyeris`",
    if (!is.na(version) && nzchar(version)) {
      sprintf(" (version %s)", version)
    } else {
      ""
    },
    " directly from the parameters recorded in your preprocessing pipeline. It ",
    "is provided so that your manuscript's methods section can faithfully and ",
    "reproducibly reflect exactly what `eyeris` did to your data. It is safe to ",
    "copy and paste this content directly into your manuscript and adapt it as ",
    "needed; please cite `eyeris` (see below) to provide attribution as ",
    "required by its license."
  )

  # reproducibility / sidecar reference
  sidecar <- paste0(
    "**Reproducibility.** A complete, machine-readable record of every ",
    "preprocessing operation and the exact parameter values used is stored ",
    "alongside your data in a per-run JSON metadata sidecar ",
    "(`source/logs/run-XX_metadata.json`). This call-stack provenance, together ",
    "with the `eyeris` version reported above, is sufficient to reproduce this ",
    "pipeline."
  )

  parts <- c(note, methods_paragraph, sidecar)

  if (isTRUE(include_citation)) {
    cite_txt <- tryCatch(
      suppressWarnings(paste(
        format(utils::citation("eyeris"), style = "text"),
        collapse = " "
      )),
      error = function(e) {
        paste0(
          "Schwartz, S. T., Yang, H., Xue, A. M., & He, M. (2025). eyeris: A ",
          "flexible, extensible, and reproducible pupillometry preprocessing ",
          "framework in R. bioRxiv. https://doi.org/10.1101/2025.06.01.657312"
        )
      }
    )
    # collapse the hard line-wraps that `format(style = "text")` inserts so the
    # citation reads as a single clean line in the boilerplate
    cite_txt <- gsub("[[:space:]]+", " ", trimws(cite_txt))
    parts <- c(
      parts,
      paste0(
        "**Citation.** If you use `eyeris` in your research, please cite: ",
        cite_txt
      )
    )
  }

  if (isTRUE(include_license)) {
    parts <- c(parts, boilerplate_license_note())
  }

  paste(parts, collapse = "\n\n")
}

#' Creative Commons CC BY 4.0 license note for the methods boilerplate
#'
#' @return A length-one character string of Markdown
#'
#' @keywords internal
boilerplate_license_note <- function() {
  paste0(
    "> **License & attribution.** This auto-generated boilerplate text is ",
    "licensed under the [Creative Commons Attribution 4.0 International ",
    "(CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license. You ",
    "are free to copy, adapt, and paste it directly into your manuscript, ",
    "provided that you give appropriate credit by citing `eyeris` (run ",
    "`citation('eyeris')` for the reference). Including the `eyeris` citation ",
    "in your references satisfies this attribution requirement."
  )
}

#' Describe each recorded pipeline step as a methods-section sentence
#'
#' Walks an `eyeris` object's recorded `params` in canonical pipeline order and
#' returns one prose sentence per step, with the actual parameter values
#' substituted in. Unknown/custom steps are appended at the end.
#'
#' @param params The `eyeris$params` list (named by step suffix)
#' @param info The `eyeris$info` list (used for context, e.g., sample rate)
#'
#' @return A character vector of sentences (possibly length 0)
#'
#' @keywords internal
describe_boilerplate_steps <- function(params, info) {
  if (is.null(params) || length(params) == 0) {
    return(character(0))
  }

  # NB: keys are the `new_suffix` values recorded by pipeline_handler(), which
  # is why z-scoring is keyed "z" (its output column suffix), not "zscore"
  canonical_order <- c(
    "load_asc",
    "deblink",
    "detransient",
    "interpolate",
    "lpfilt",
    "downsample",
    "bin",
    "detrend",
    "z",
    "zscore",
    "epoch"
  )

  present <- names(params)
  ordered_steps <- c(
    intersect(canonical_order, present),
    setdiff(present, canonical_order)
  )

  sentences <- character(0)
  for (step in ordered_steps) {
    step_params <- params[[step]]$parameters
    if (is.null(step_params)) {
      step_params <- list()
    }
    sentence <- describe_boilerplate_step(step, step_params, info)
    if (!is.null(sentence) && nzchar(sentence)) {
      sentences <- c(sentences, sentence)
    }
  }

  sentences
}

#' Format a numeric value for inclusion in prose
#'
#' @param x A scalar value
#'
#' @return A clean character representation (no scientific notation or trailing
#' zeros)
#'
#' @keywords internal
bp_num <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  if (is.numeric(x)) {
    format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  } else {
    as.character(x)
  }
}

#' Describe a single pipeline step as a methods-section sentence
#'
#' @param step The step name (suffix), e.g. `"deblink"`
#' @param p The recorded parameter list for that step
#' @param info The `eyeris$info` list
#'
#' @return A length-one character string, or `NULL` if the step has no
#' meaningful description
#'
#' @keywords internal
describe_boilerplate_step <- function(step, p, info) {
  switch(
    step,
    load_asc = {
      base <- paste0(
        "Raw pupil time series were imported from the EyeLink `.asc` recording"
      )
      sr <- info$sample.rate
      if (!is.null(sr) && is.numeric(sr) && length(sr) == 1 && is.finite(sr)) {
        base <- paste0(base, sprintf(" (sampled at %s Hz)", bp_num(sr)))
      }
      paste0(base, ".")
    },
    deblink = {
      ext <- p$extend
      if (is.null(ext)) {
        return(NULL)
      }
      if (length(ext) == 1) {
        sprintf(
          paste0(
            "Blink and missing-data artifacts were attenuated by extending ",
            "(i.e., NA-padding) each gap of missing samples by %s ms in both ",
            "directions."
          ),
          bp_num(ext)
        )
      } else {
        sprintf(
          paste0(
            "Blink and missing-data artifacts were attenuated by NA-padding ",
            "each gap of missing samples by %s ms backward and %s ms forward."
          ),
          bp_num(ext[1]),
          bp_num(ext[2])
        )
      }
    },
    detransient = {
      n <- if (!is.null(p$n)) p$n else 16
      sentence <- sprintf(
        paste0(
          "Physiologically implausible transient samples -- those whose ",
          "sample-to-sample change in pupil size exceeded a speed-based median ",
          "absolute deviation (MAD) threshold -- were removed (MAD multiplier ",
          "n = %s)."
        ),
        bp_num(n)
      )
      if (!is.null(p$mad_thresh)) {
        sentence <- paste0(
          sentence,
          sprintf(
            " A fixed speed threshold of %s was supplied.",
            bp_num(p$mad_thresh)
          )
        )
      }
      sentence
    },
    interpolate = paste0(
      "Remaining gaps of missing data were reconstructed using linear ",
      "interpolation."
    ),
    lpfilt = {
      wp <- if (!is.null(p$wp)) p$wp else 4
      ws <- if (!is.null(p$ws)) p$ws else 8
      rp <- if (!is.null(p$rp)) p$rp else 1
      rs <- if (!is.null(p$rs)) p$rs else 35
      sprintf(
        paste0(
          "The pupil time series was smoothed with a zero-phase (forward-and-",
          "reverse) Butterworth low-pass filter (passband edge = %s Hz, ",
          "stopband edge = %s Hz, passband ripple = %s dB, stopband ",
          "attenuation = %s dB)."
        ),
        bp_num(wp),
        bp_num(ws),
        bp_num(rp),
        bp_num(rs)
      )
    },
    downsample = {
      tf <- p$target_fs
      if (is.null(tf)) {
        return(paste0(
          "The pupil time series was downsampled following the application of ",
          "an anti-aliasing low-pass filter."
        ))
      }
      sprintf(
        paste0(
          "The pupil time series was downsampled to %s Hz after applying an ",
          "anti-aliasing low-pass filter to prevent aliasing."
        ),
        bp_num(tf)
      )
    },
    bin = {
      bps <- p$bins_per_second
      if (is.null(bps)) {
        return(NULL)
      }
      method <- if (!is.null(p$method)) p$method else "mean"
      sprintf(
        paste0(
          "The pupil time series was binned into %s bins per second (%s ms per ",
          "bin), retaining the %s of the samples within each bin."
        ),
        bp_num(bps),
        bp_num(1000 / bps),
        method
      )
    },
    detrend = paste0(
      "A linear trend (estimated by regressing pupil size on time) was removed ",
      "from the pupil time series (linear detrending)."
    ),
    # z-scoring is recorded under the "z" suffix; "zscore" kept as a defensive
    # alias in case params are constructed with the full step name
    z = ,
    zscore = paste0(
      "The pupil time series was z-scored (rescaled to a mean of 0 and a ",
      "standard deviation of 1) within each recording block."
    ),
    epoch = {
      sentence <- paste0(
        "The continuous pupil time series was segmented into event-locked ",
        "epochs"
      )
      lim <- p$limits
      if (!is.null(lim) && length(lim) == 2 && all(is.finite(lim))) {
        sentence <- sprintf(
          "%s spanning %s to %s s relative to each event of interest",
          sentence,
          bp_num(lim[1]),
          bp_num(lim[2])
        )
      }
      label <- p$label
      if (
        !is.null(label) &&
          length(label) == 1 &&
          is.character(label) &&
          nzchar(label)
      ) {
        sentence <- sprintf("%s (labeled \"%s\")", sentence, label)
      }
      sentence <- paste0(sentence, ".")
      if (isTRUE(p$baseline)) {
        bt <- if (!is.null(p$baseline_type)) p$baseline_type[1] else "sub"
        bt_word <- switch(bt, sub = "subtractive", div = "divisive", bt)
        sentence <- paste0(
          sentence,
          sprintf(
            " Each epoch was baseline-corrected using %s baseline correction.",
            bt_word
          )
        )
      }
      sentence
    },
    # custom / unknown step: describe generically so multi-step custom
    # pipelines are still represented in the boilerplate
    {
      sprintf(
        "A custom `%s` preprocessing step was applied to the pupil time series.",
        step
      )
    }
  )
}
