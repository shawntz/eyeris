#' Render R Markdown report
#'
#' Renders an R Markdown file to HTML and cleans up the temporary file.
#'
#' @param rmd_f Path to the R Markdown file to render
#'
#' @return No return value; renders HTML report and removes temporary file
#'
#' @keywords internal
render_report <- function(rmd_f) {
  rmarkdown::render(rmd_f, output_format = "html_document")
  unlink(rmd_f)
}

#' Build the canonical run-directory basename
#'
#' Returns the basename used for both the per-run figure directory and the
#' filename prefix of every figure it contains. When `task` is supplied, the
#' name is namespaced as `task-{task}_run-XX` so that different tasks sharing
#' the same run number (a valid BIDS pattern, e.g. `task-study_run-01` and
#' `task-test_run-01`) do not collide. When `task` is `NULL`, the legacy
#' `run-XX` form is returned for backwards compatibility.
#'
#' This is the single source of truth for run-directory naming: every writer
#' builds the directory and its contained filenames from it, and every reader
#' either globs with `run_dir_pattern()` (then parses the numeric run via
#' `run-(\\d+)`) or derives filenames from `basename(run_dir)`.
#'
#' @param run_num Run number (numeric or character coercible to numeric)
#' @param task Optional BIDS task name. Defaults to `NULL`
#'
#' @return A character string, e.g. `"task-study_run-01"` or `"run-01"`
#'
#' @keywords internal
make_run_dir_name <- function(run_num, task = NULL) {
  base <- sprintf("run-%02d", as.numeric(run_num))
  if (!is.null(task) && nzchar(task)) {
    base <- paste0("task-", task, "_", base)
  }
  base
}

#' Regex matching run-directory basenames
#'
#' Matches both the task-namespaced form (`task-study_run-01`) and the legacy
#' form (`run-01`). The numeric run can subsequently be extracted with
#' `sub(".*run-(\\d+)$", "\\1", x)`.
#'
#' @return A character string containing the regular expression
#'
#' @keywords internal
run_dir_pattern <- function() {
  "^(task-.+_)?run-\\d+$"
}

#' Filter run-directory basenames to a single task
#'
#' Given a vector of directory basenames (as returned by `list.dirs(...,
#' full.names = FALSE)`), keep only the valid run directories that belong to
#' `task`. When `task` is `NULL` (or empty), only legacy task-less `run-XX`
#' directories are kept. Comparison of the task component is done by exact
#' string equality (not regex), so task names containing regex metacharacters
#' or underscores are handled correctly.
#'
#' @param dirs Character vector of directory basenames
#' @param task Optional BIDS task name. Defaults to `NULL`
#'
#' @return The subset of `dirs` belonging to `task`
#'
#' @keywords internal
filter_task_run_dirs <- function(dirs, task = NULL) {
  dirs <- dirs[grepl(run_dir_pattern(), dirs)]
  if (length(dirs) == 0) {
    return(dirs)
  }
  # extract the task component; legacy run-XX dirs yield "" (no task- prefix)
  dir_task <- sub("^task-(.+)_run-\\d+$", "\\1", dirs)
  dir_task[dir_task == dirs] <- ""
  if (!is.null(task) && nzchar(task)) {
    dirs[dir_task == task]
  } else {
    dirs[dir_task == ""]
  }
}

#' Create eyeris report
#'
#' Generates a comprehensive HTML report for `eyeris` preprocessing results.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param out Output directory for the report
#' @param plots Vector of plot file paths to include in the report
#' @param eye_suffix Optional eye suffix (e.g., "eye-L", "eye-R") for binocular data
#' @param ... Additional parameters passed from bidsify
#'
#' @return Path to the generated `R Markdown` file
#'
#' @keywords internal
make_report <- function(eyeris, out, plots, eye_suffix = NULL, ...) {
  # get extra subject params from bidsify.R
  params <- list(...)
  task <- params$task

  has_multiple_runs <- length(grep("run-\\d+", plots)) > 0

  # temp file - include task and eye_suffix in filename if provided so that
  # different tasks sharing a run number do not overwrite each other (#293)
  report_filename <- paste0("sub-", params$sub)
  if (!is.null(task) && nzchar(task)) {
    report_filename <- paste0(report_filename, "_task-", task)
  }
  if (!is.null(eye_suffix)) {
    report_filename <- paste0(report_filename, "_", eye_suffix)
  }
  report_filename <- paste0(report_filename, ".Rmd")
  rmd_f <- file.path(out, report_filename)

  report_date <- format(Sys.time(), "%B %d, %Y | %H:%M:%OS3")
  package_version <- as.character(utils::packageVersion("eyeris"))
  css <- system.file(
    file.path("rmarkdown", "css", "report.css"),
    package = "eyeris"
  )

  sticker_path <- system.file("figures", "sticker.png", package = "eyeris")

  all_run_dirs <- list.dirs(
    file.path(out, "source", "figures"),
    recursive = FALSE,
    full.names = FALSE
  )
  # restrict to THIS task's run directories so a report does not enumerate
  # another task's runs that share the same source/figures/ parent (#293)
  task_run_dirs <- filter_task_run_dirs(all_run_dirs, task)
  run_ids <- sort(as.integer(sub(".*run-(\\d+)$", "\\1", task_run_dirs)))

  run_info <- paste(
    " - Runs: ",
    paste(
      sapply(run_ids, function(x) {
        x_chr <- as.character(x)
        if (x < 10) paste0("0", x_chr) else x_chr
      }),
      collapse = ", "
    ),
    "\n"
  )

  # add eye information to summary if binocular
  eye_info <- ""
  if (!is.null(eye_suffix)) {
    eye_info <- paste0(" - Eye: ", eye_suffix, "\n")
  }

  # eyeris report markdown content
  block_heatmaps_md <- "\n## Gaze Heatmaps\n\n"
  for (run_id in run_ids) {
    rd <- make_run_dir_name(run_id, task)
    heatmap_path <- file.path(
      "source",
      "figures",
      rd,
      sprintf("%s_gaze_heatmap", rd)
    )
    if (!is.null(eye_suffix)) {
      heatmap_path <- paste0(heatmap_path, "_", eye_suffix)
    }
    heatmap_path <- paste0(heatmap_path, ".png")

    if (file.exists(file.path(out, heatmap_path))) {
      block_heatmaps_md <- paste0(
        block_heatmaps_md,
        "### run-",
        sprintf("%02d", run_id),
        "\n\n",
        "![](",
        heatmap_path,
        ")\n\n"
      )
    }
  }

  # add binocular correlation plots to the report
  binocular_correlations_md <- "\n## Binocular Correlations\n\n"
  for (run_id in run_ids) {
    rd <- make_run_dir_name(run_id, task)
    correlation_path <- file.path(
      "source",
      "figures",
      rd,
      sprintf("%s_binocular_correlation.png", rd)
    )
    if (file.exists(file.path(out, correlation_path))) {
      binocular_correlations_md <- paste0(
        binocular_correlations_md,
        "### run-",
        sprintf("%02d", run_id),
        "\n\n",
        "![](",
        correlation_path,
        ")\n\n"
      )
    }
  }

  logs_dir <- file.path(out, "source", "logs")
  callstack_md <- ""

  for (run_id in run_ids) {
    metadata_dir <- file.path(out, "source", "logs")
    if (!dir.exists(metadata_dir)) {
      dir.create(metadata_dir, recursive = TRUE)
    }

    run_metadata <- list(
      run = run_id,
      source_file = eyeris$file,
      call_stack = sanitize_call_stack(eyeris$params)
    )

    meta_path <- file.path(
      metadata_dir,
      sprintf("%s_metadata.json", make_run_dir_name(run_id, task))
    )

    # Always regenerate metadata to ensure it uses the latest sanitization
    # This prevents issues with old files containing huge epoch data
    jsonlite::write_json(
      run_metadata,
      meta_path,
      pretty = TRUE,
      auto_unbox = TRUE
    )

    if (file.exists(meta_path)) {
      meta <- jsonlite::read_json(meta_path)

      callstack_md <- paste0(
        callstack_md,
        "### run-",
        sprintf("%02d", run_id),
        "\n\n",
        "**Source `.asc` file**: ",
        meta$source_file,
        "\n\n",
        "**Call stack**:\n\n",
        make_md_table_multiline(format_call_stack(meta$call_stack)),
        "\n\n"
      )
    } else {
      callstack_md <- paste0(
        callstack_md,
        "### run-",
        sprintf("%02d", run_id),
        "\n\n",
        "*No metadata found for this run*\n\n"
      )
    }
  }

  for (run_id in run_ids) {
    block <- paste0("block_", run_id)
    file <- if (!is.null(attr(eyeris$timeseries[[block]], "source_file"))) {
      attr(eyeris$timeseries[[block]], "source_file")
    } else if (!is.null(eyeris$file)) {
      eyeris$file
    } else {
      "Unknown"
    }
  }

  # generate fMRIPrep-style, copy-and-paste-ready methods boilerplate from the
  # captured pipeline params, and write it out alongside the per-run JSON
  # metadata sidecars (CC BY 4.0-licensed; see build_boilerplate_md())
  boilerplate_md <- build_boilerplate_md(
    eyeris,
    version = package_version,
    n_runs = length(run_ids)
  )

  if (!dir.exists(logs_dir)) {
    dir.create(logs_dir, recursive = TRUE)
  }

  # namespace by task so different tasks sharing a subject/session do not
  # overwrite each other's boilerplate (mirrors the report/sidecar naming, #293)
  boilerplate_filename <- "methods_boilerplate"
  if (!is.null(task) && nzchar(task)) {
    boilerplate_filename <- paste0(boilerplate_filename, "_task-", task)
  }
  if (!is.null(eye_suffix)) {
    boilerplate_filename <- paste0(boilerplate_filename, "_", eye_suffix)
  }
  boilerplate_filename <- paste0(boilerplate_filename, ".md")

  writeLines(
    c("# eyeris preprocessing methods boilerplate", "", boilerplate_md, ""),
    con = file.path(logs_dir, boilerplate_filename)
  )

  title <- "`eyeris` preprocessing report"

  content <- paste0(
    "---\n",
    "title: '",
    title,
    "'\n",
    "date: '",
    report_date,
    "'\n",
    "output:\n",
    "  html_document:\n",
    "    df_print: paged\n",
    "    css: '",
    css,
    "'\n",
    "    toc: true\n",
    "    toc_float: true\n",
    "    toc_depth: 6\n",
    "    number_sections: false\n",
    "---\n\n",
    "\n\n<img src='",
    sticker_path,
    "' class='top-right-image'>",
    "\n\n---\n\n## Summary\n",
    " - Subject ID: ",
    params$sub,
    "\n",
    " - Session: ",
    params$ses,
    "\n",
    " - Task: ",
    params$task,
    "\n",
    eye_info,
    run_info,
    " - BIDS Directory: ",
    out,
    "\n",
    " - [`eyeris` version](https://github.com/shawntz/eyeris): ",
    package_version,
    "\n",
    "\n\n<style type='text/css'>\n",
    "@import url('http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/",
    "bootstrap.min.css');\n",
    "@import url('https://cdn.jsdelivr.net/npm/lightbox2/dist/css/",
    "lightbox.min.css');\n</style>\n",
    "\n\n---\n\n## Reproducible Methods Boilerplate\n\n",
    boilerplate_md,
    "\n",
    "\n\n---\n\n## Preprocessing Summaries\n\n",
    save_progressive_summary_plots(
      eyeris = eyeris,
      out_dir = out,
      eye_suffix = eye_suffix,
      task = task,
      verbose = params$verbose
    ),
    "\n\n## Preprocessed Data Previews\n\n",
    save_detrend_plots(
      eyeris = eyeris,
      out_dir = out,
      eye_suffix = eye_suffix,
      task = task,
      verbose = params$verbose
    ),
    print_plots(plots, eye_suffix = eye_suffix, task = task, eyeris = eyeris),
    "\n",
    block_heatmaps_md,
    if (should_plot_binoc_cors(eyeris)) binocular_correlations_md else "",
    "\n\n---\n\n## EyeLink Header Metadata\n\n",
    make_md_table(eyeris$info),
    "\n",
    "\n\n---\n\n## `eyeris` call stack\n\n",
    callstack_md,
    "\n\n---\n\n## Citation\n\n",
    "```{r citation, echo=FALSE, comment=NA}\n",
    "citation('eyeris')\n",
    "```\n\n",
    "\n\n---\n\n## Session Information\n\n",
    "```{r session-info, echo=FALSE, comment=NA}\n",
    "sessionInfo()\n",
    "```\n\n\n\n\n\n"
  )

  writeLines(content, con = rmd_f)

  rmd_f
}

#' Create markdown table from data frame
#'
#' Converts a data frame into a markdown table.
#'
#' @param df The data frame to convert
#'
#' @return A character string containing the markdown table content
#'
#' @keywords internal
make_md_table <- function(df) {
  md_table <- "| Property | Value |\n|----|----|\n"
  for (prop in colnames(df)) {
    val <- df[[1, prop]]
    md_table <- paste0(md_table, "| ", prop, " | ", val, " |\n")
  }

  md_table
}

#' Create multiline markdown table from data frame
#'
#' Converts a data frame into a multiline markdown table.
#'
#' @param df The data frame to convert
#'
#' @return A character string containing the markdown table content
#'
#' @keywords internal
make_md_table_multiline <- function(df) {
  md_table <- paste0("| ", paste(colnames(df), collapse = " | "), " |\n")
  md_table <- paste0(
    md_table,
    "|",
    paste(rep("---", ncol(df)), collapse = "|"),
    "|\n"
  )
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    md_table <- paste0(
      md_table,
      "| ",
      paste(as.character(row), collapse = " | "),
      " |\n"
    )
  }
  md_table
}

sanitize_call_stack <- function(x, parent_name = NULL, in_parameters = FALSE) {
  # Filter out epoch-related large data structures before JSON serialization
  # to prevent memory issues during report rendering
  # Only filter when we're inside a "parameters" list
  if (in_parameters && !is.null(parent_name)) {
    name_lower <- tolower(parent_name)
    is_epoch_related <- grepl("epoch", name_lower) ||
      name_lower == "events" ||
      name_lower == "baseline_events"

    # Omit complex objects (lists, data.frames) with epoch-related names
    if (is_epoch_related && (is.list(x) || is.data.frame(x))) {
      return("<omitted>")
    }
  }

  if (is.call(x)) {
    # Deparse the call but limit output size to prevent memory issues
    # This handles cases where sys.calls() captures large objects
    deparsed <- tryCatch(
      {
        result <- deparse(x, width.cutoff = 500L)
        # Limit to first few lines if it's very long
        if (length(result) > 5) {
          result <- c(result[1:3], "...")
        }
        collapsed <- paste(result, collapse = " ")
        # Final size limit
        if (nchar(collapsed) > 500) {
          paste0(substr(collapsed, 1, 500), "...")
        } else {
          collapsed
        }
      },
      error = function(e) "<call: error deparsing>"
    )
    deparsed
  } else if (is.data.frame(x)) {
    # Convert data frames to a simple summary to avoid huge JSON output
    paste0("<data.frame: ", nrow(x), " rows x ", ncol(x), " cols>")
  } else if (is.list(x)) {
    # Check if we're entering a "parameters" list
    is_entering_parameters <- !is.null(parent_name) &&
      parent_name == "parameters"

    # Check if this is a call_stack field containing sys.calls() output
    # sys.calls() returns a list of calls which can contain huge objects
    is_call_stack_field <- !is.null(parent_name) &&
      parent_name %in% c("call_stack", "call")

    if (is_call_stack_field && length(x) > 0) {
      # This is likely sys.calls() output - just get the function names
      # and a simplified representation
      call_names <- sapply(x, function(call) {
        if (is.call(call)) {
          fn_name <- as.character(call[[1]])
          if (length(fn_name) > 1) {
            fn_name <- fn_name[length(fn_name)]
          }
          fn_name
        } else {
          "unknown"
        }
      })
      # Return simplified call stack
      return(paste(call_names, collapse = " > "))
    }

    # Process list elements, passing the name for filtering
    result <- lapply(names(x), function(name) {
      sanitize_call_stack(
        x[[name]],
        parent_name = name,
        in_parameters = in_parameters || is_entering_parameters
      )
    })
    names(result) <- names(x)
    result
  } else if (is.character(x) && length(x) == 1 && nchar(x) > 1000) {
    # Truncate very long strings
    paste0(substr(x, 1, 500), "... <truncated>")
  } else {
    x
  }
}

#' Compute percent data lost for a given run/block
#'
#' Calculates the proportion of samples in the raw pupil timeseries that are
#' invalid (i.e., missing/during a blink, or off-screen) and expresses it as a
#' percentage. This surfaces data loss directly in the report to reinforce
#' workflow transparency.
#'
#' Prefers the canonical `prop_invalid` metric stored in
#' `eyeris$confounds$unepoched_timeseries` (computed by
#' [eyeris::summarize_confounds()]). Falls back to computing the proportion of
#' missing samples directly from the raw timeseries when confounds are
#' unavailable.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param run_num Run identifier (numeric or character, e.g. `1` or `"01"`)
#' @param eye_suffix Optional eye suffix (e.g., "eye-L", "eye-R") used to select
#'   the correct eye from a binocular object
#'
#' @return A numeric percentage in `[0, 100]`, or `NA_real_` when it cannot be
#'   determined
#'
#' @keywords internal
compute_run_data_loss <- function(eyeris, run_num, eye_suffix = NULL) {
  if (is.null(eyeris)) {
    return(NA_real_)
  }

  # resolve the correct eye sub-object for binocular data
  obj <- eyeris
  if (is_binocular_object(eyeris)) {
    obj <- if (!is.null(eye_suffix) && eye_suffix == "eye-R") {
      eyeris$right
    } else {
      eyeris$left
    }
  }

  block <- paste0("block_", as.integer(run_num))

  is_valid_prop <- function(x) {
    !is.null(x) && length(x) == 1 && is.numeric(x) && !is.na(x)
  }

  # prefer the canonical confounds metric on the raw signal
  prop_invalid <- tryCatch(
    {
      block_confounds <- obj$confounds$unepoched_timeseries[[block]]
      if (is.null(block_confounds) || length(block_confounds) == 0) {
        NA_real_
      } else {
        raw_step <- if ("pupil_raw" %in% names(block_confounds)) {
          "pupil_raw"
        } else {
          names(block_confounds)[1]
        }
        block_confounds[[raw_step]]$prop_invalid
      }
    },
    error = function(e) NA_real_
  )

  # fall back to direct computation from the raw timeseries
  if (!is_valid_prop(prop_invalid)) {
    prop_invalid <- tryCatch(
      {
        ts <- obj$timeseries[[block]]
        if (is.null(ts)) {
          NA_real_
        } else {
          raw_col <- if ("pupil_raw" %in% names(ts)) {
            "pupil_raw"
          } else {
            grep("^pupil_", names(ts), value = TRUE)[1]
          }
          if (is.na(raw_col)) NA_real_ else mean(is.na(ts[[raw_col]]))
        }
      },
      error = function(e) NA_real_
    )
  }

  if (!is_valid_prop(prop_invalid)) {
    return(NA_real_)
  }

  prop_invalid * 100
}

#' Print plots in markdown format
#'
#' Generates markdown code to display plots in the report.
#'
#' @param plots Vector of plot file paths
#' @param eye_suffix Optional eye suffix for binocular data
#' @param task Optional BIDS task name used to scope run directories (#293)
#' @param eyeris Optional `eyeris` object used to annotate each run with the
#'   percent of data lost in its timeseries
#'
#' @return A character string containing markdown plot references
#'
#' @keywords internal
print_plots <- function(plots, eye_suffix = NULL, task = NULL, eyeris = NULL) {
  md_plots <- ""

  make_relative_path <- function(path) {
    gsub("^.*?(?=source/)", "", path, perl = TRUE)
  }

  # detect run dirs
  run_dirs <- plots |>
    dirname() |>
    unique() |>
    dirname() |>
    unique() |>
    list.dirs(full.names = TRUE, recursive = FALSE) |>
    unique()

  # restrict to THIS task's run directories so plots from another task sharing
  # the same source/figures/ parent are not mixed into this report (#293)
  run_dirs <- run_dirs[
    basename(run_dirs) %in% filter_task_run_dirs(basename(run_dirs), task)
  ]

  if (length(run_dirs) > 0) {
    for (run_dir in run_dirs) {
      run_plots <- list.files(run_dir, pattern = "*.jpg", full.names = TRUE)

      if (!is.null(eye_suffix)) {
        run_plots <- run_plots[grepl(eye_suffix, run_plots)]
      }

      if (length(run_plots) > 0) {
        run_num <- sub(".*run-(\\d+).*$", "\\1", run_dir)

        md_plots <- paste0(md_plots, "### run-", run_num, "\n\n")

        # annotate the timeseries with percent data lost for transparency
        pct_data_lost <- compute_run_data_loss(
          eyeris = eyeris,
          run_num = run_num,
          eye_suffix = eye_suffix
        )
        if (!is.na(pct_data_lost)) {
          md_plots <- paste0(
            md_plots,
            "- **Percent data lost:** ",
            sprintf("%.2f", pct_data_lost),
            "% ",
            "_(missing/blink or off-screen samples in the raw pupil ",
            "timeseries)_\n\n"
          )
        }

        # sort by fig number if possible
        plot_fig_ids <- suppressWarnings(as.numeric(sub(
          ".*_fig-(\\d+)_.*",
          "\\1",
          run_plots
        )))
        if (all(!is.na(plot_fig_ids))) {
          sorted_plot_paths <- run_plots[order(plot_fig_ids)]
        } else {
          sorted_plot_paths <- run_plots
        }

        placeholder_detected <- FALSE
        placeholder_patterns <- c(
          "no_data",
          "placeholder",
          "error",
          "No_data",
          "NoData"
        )
        if (
          length(sorted_plot_paths) == 1 ||
            all(sapply(sorted_plot_paths, function(x) {
              any(grepl(
                paste(placeholder_patterns, collapse = "|"),
                x,
                ignore.case = TRUE
              ))
            }))
        ) {
          placeholder_detected <- TRUE
        }

        if (placeholder_detected) {
          md_plots <- paste0(
            md_plots,
            "> **No data available for this run.**\n\n"
          )
        }

        for (fig_path in sorted_plot_paths) {
          relative_fig_path <- make_relative_path(fig_path)
          md_plots <- paste0(md_plots, "![](", relative_fig_path, ")\n\n")
        }

        # detrend diagnostics - check for eye_suffix version first.
        # derive the filename prefix from the (task-namespaced) directory name
        # so reads always match what save_detrend_plots() wrote (#293)
        rd <- basename(run_dir)
        detrend_plot_path <- file.path(run_dir, paste0(rd, "_detrend.png"))

        # if eye_suffix is provided, look for the suffixed version
        if (!is.null(eye_suffix)) {
          detrend_plot_path <- file.path(
            run_dir,
            paste0(rd, "_detrend_", eye_suffix, ".png")
          )
        }
        detrend_exists <- file.exists(detrend_plot_path)
        if (detrend_exists) {
          md_plots <- paste0(
            md_plots,
            "### Detrend Diagnostics\n\n",
            "![](",
            make_relative_path(detrend_plot_path),
            ")\n\n"
          )
        }
      }
    }
    md_plots
  }
}

#' Save detrend plots for each block
#'
#' Generates and saves detrend diagnostic plots for each block in the `eyeris`
#' object.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param out_dir Output directory for saving plots
#' @param preview_n Number of preview samples for plotting
#' @param plot_params Additional plotting parameters
#' @param eye_suffix Optional eye suffix for binocular data
#' @param task Optional BIDS task name used to namespace run directories (#293)
#' @param verbose Logical. Whether to print verbose output (default TRUE).
#'
#' @return No return value; saves detrend plots to the specified directory
#'
#' @keywords internal
save_detrend_plots <- function(
  eyeris,
  out_dir,
  preview_n = 3,
  plot_params = list(),
  eye_suffix = NULL,
  task = NULL,
  verbose = TRUE
) {
  blocks <- names(eyeris$timeseries)

  for (block in blocks) {
    block_number <- sub("block_", "", block)
    run_id <- sprintf("run-%02d", as.numeric(block_number))
    rd <- make_run_dir_name(block_number, task)
    run_dir <- file.path(out_dir, "source", "figures", rd)
    detrend_filename <- paste0(rd, "_detrend")
    if (!is.null(eye_suffix)) {
      detrend_filename <- paste0(detrend_filename, "_", eye_suffix)
    }
    detrend_filename <- paste0(detrend_filename, ".png")
    detrend_path <- file.path(run_dir, detrend_filename)

    if (!dir.exists(run_dir)) {
      dir.create(run_dir, recursive = TRUE)
    }

    pupil_data <- eyeris$timeseries[[block]]

    # only proceed if detrended values exist
    if (
      "detrend_fitted_values" %in%
        names(pupil_data) &&
        any(grepl("_detrend$", names(pupil_data)))
    ) {
      pupil_steps <- grep("^pupil_", names(pupil_data), value = TRUE)

      grDevices::jpeg(
        filename = detrend_path,
        width = 1850,
        height = 1500,
        res = 300
      )

      plot_detrend_overlay(
        pupil_data = pupil_data,
        pupil_steps = pupil_steps,
        preview_n = preview_n,
        plot_params = plot_params,
        suppress_prompt = TRUE
      )

      grDevices::dev.off()

      log_info("{detrend_path}", verbose = verbose)
    } else {
      log_warn("No detrend data found for {run_id}")
    }
  }
}

#' Create progressive preprocessing summary plot
#'
#' Internal function to create a comprehensive visualization showing the
#' progressive effects of preprocessing steps on pupil data. This plot displays
#' multiple preprocessing stages overlaid on the same time series, allowing
#' users to see how each step modifies the pupil signal.
#'
#' @param pupil_data A data frame containing pupil time series data with
#'   multiple preprocessing columns (e.g., `eyeris$timeseries$block_1`)
#' @param pupil_steps Character vector of column names containing pupil data
#'   at different preprocessing stages
#'   (e.g., `c("pupil_raw", "pupil_deblink", "pupil_detrend")`)
#' @param preview_n Number of columns for subplot layout. Defaults to `3`
#' @param plot_params Named list of additional parameters to forward to plotting
#'   functions. Defaults to `list()`
#' @param run_id Character string identifying the run/block (e.g., "run-01").
#'   Used for plot titles and file naming. Defaults to `"run-01"`
#' @param cex Character expansion factor for plot elements. Defaults to `2.0`
#' @param eye_suffix Optional eye suffix for binocular data
#' @param full_pupil_data Optional data frame containing the full-resolution
#'   (pre-decimation) pupil time series, with the same `pupil_*` and `time_secs`
#'   columns as `pupil_data` (e.g.,
#'   `eyeris$timeseries_pre_decimation$block_1`). When supplied, preprocessing
#'   steps that precede a `downsample()`/`bin()` step are drawn from this
#'   full-resolution data instead of the decimated `pupil_data`, so they are not
#'   shown at the decimated sampling rate. Defaults to `NULL`
#'
#' @return NULL (invisibly). Creates a plot showing progressive preprocessing
#'   effects with multiple layers overlaid on the same time series
#'
#' @details
#' This function creates a two-panel visualization:
#' \itemize{
#'   \item Top panel: Overlaid time series showing progressive preprocessing
#'     effects with different colors for each step
#'   \item Bottom panel: Legend identifying each preprocessing step
#' }
#'
#' The plot excludes z-scored data (columns ending with "_z") and only
#' includes steps with sufficient valid data points (>100). Each preprocessing
#' step is displayed with a distinct color, making it easy to see how the
#' signal changes through the pipeline.
#'
#' @keywords internal
#'
#' @seealso \code{\link{plot.eyeris}}
make_prog_summary_plot <- function(
  pupil_data,
  pupil_steps,
  preview_n = 3,
  plot_params = list(),
  run_id = "run-01",
  cex = 2.0,
  eye_suffix = NULL,
  full_pupil_data = NULL
) {
  plot_steps <- pupil_steps[!grepl("_z$", pupil_steps)]

  layer_data <- list()
  for (i in seq_along(plot_steps)) {
    # plot pre-decimation steps from the preserved full-resolution data so
    # they are not shown at the decimated sampling rate (see issue #294)
    use_full <- !is.null(full_pupil_data) && !is_decimated_col(plot_steps[i])
    src <- if (use_full) full_pupil_data else pupil_data

    step_data <- src[[plot_steps[i]]]
    step_time <- src$time_secs
    valid_indices <- is.finite(step_data)
    if (sum(valid_indices) < 100) {
      next
    }
    layer_data[[i]] <- list(
      time = step_time[valid_indices],
      signal = step_data[valid_indices],
      step_name = plot_steps[i]
    )
  }
  if (length(layer_data) < 2) {
    plot(
      NA,
      xlim = c(0, 1),
      ylim = c(0, 1),
      type = "n",
      xlab = "",
      ylab = "",
      main = paste("Insufficient data for", run_id)
    )
    text(
      0.5,
      0.5,
      "Not enough preprocessing steps\nfor progressive summary",
      cex = 1.2,
      col = "red"
    )
    return()
  }

  all_signals <- unlist(lapply(layer_data, function(x) x$signal))
  y_range <- range(all_signals, na.rm = TRUE)
  x_range <- range(unlist(lapply(layer_data, function(x) x$time)), na.rm = TRUE)
  y_padding <- diff(y_range) * 0.25 + 1e-6
  x_padding <- diff(x_range) * 0.05 + 1e-6
  y_range <- y_range + c(-y_padding, y_padding)
  x_range <- x_range + c(-x_padding, x_padding)

  colorpal <- eyeris_color_palette()
  colors <- c("black", colorpal)
  n_layers <- length(layer_data)
  colors <- colors[seq_len(n_layers)]

  layout(matrix(1:2, nrow = 2), heights = c(7, 2))
  par(mar = c(4, 5, 4, 2))
  plot(
    NA,
    xlim = x_range,
    ylim = y_range,
    type = "n",
    xlab = "Time (seconds)",
    ylab = "Pupil Size",
    main = paste(
      "Progressive Preprocessing Summary -",
      run_id,
      if (!is.null(eye_suffix)) paste0(" (", eye_suffix, ")") else ""
    ),
    cex.main = cex,
    cex.lab = cex,
    cex.axis = cex,
    yaxt = "n",
    bty = "n"
  )
  axis(2, labels = FALSE)
  for (i in seq_along(layer_data)) {
    layer <- layer_data[[i]]
    time_offset <- layer$time + i * 0.1
    scale_factor <- 1 - i * 0.02
    signal_scaled <- layer$signal * scale_factor
    lines(time_offset, signal_scaled, col = colors[i], lwd = 4)
  }

  par(mar = c(0, 0, 0, 0))
  plot.new()
  step_names <- sapply(layer_data, function(x) {
    clean_name <- gsub("pupil_", "", x$step_name)
    clean_name <- gsub("_", " > ", clean_name)
    clean_name
  })
  legend(
    "center",
    legend = step_names,
    col = colors,
    lwd = 2,
    cex = cex - 0.5,
    title = "Processing Steps",
    horiz = FALSE,
    bty = "n"
  )
  layout(1)
}

#' Save progressive summary plots for each block
#'
#' Generates and saves progressive summary plots for each block in the `eyeris`
#' object.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param out_dir Output directory for saving plots
#' @param preview_n Number of preview samples for plotting
#' @param plot_params Additional plotting parameters
#' @param eye_suffix Optional eye suffix for binocular data
#' @param task Optional BIDS task name used to namespace run directories (#293)
#' @param verbose Logical. Whether to print verbose output (default TRUE).
#'
#' @return A character string containing markdown references to the saved plots
#'
#' @keywords internal
save_progressive_summary_plots <- function(
  eyeris,
  out_dir,
  preview_n = 3,
  plot_params = list(),
  eye_suffix = NULL,
  task = NULL,
  verbose = TRUE
) {
  run_dirs <- list.dirs(
    file.path(out_dir, "source", "figures"),
    recursive = FALSE,
    full.names = FALSE
  )
  # restrict to THIS task's run directories before parsing run numbers (#293)
  run_ids <- sort(as.integer(sub(
    ".*run-(\\d+)$",
    "\\1",
    filter_task_run_dirs(run_dirs, task)
  )))

  md_content <- paste(
    "This visualization shows how the pupil time series changes across",
    "preprocessing steps. ",
    "Each layer represents a different",
    "preprocessing step, with the earliest step at the back ",
    "and the",
    "final step at the front (via a subtle horizontal offset effect).\n\n"
  )

  for (run_id in run_ids) {
    block <- paste0("block_", run_id)
    rd <- make_run_dir_name(run_id, task)
    run_id <- sprintf("run-%02d", run_id)
    run_dir <- file.path(out_dir, "source", "figures", rd)
    progressive_filename <- paste0(rd, "_desc-progressive_summary")
    if (!is.null(eye_suffix)) {
      progressive_filename <- paste0(progressive_filename, "_", eye_suffix)
    }
    progressive_filename <- paste0(progressive_filename, ".png")
    progressive_path <- file.path(run_dir, progressive_filename)

    if (!dir.exists(run_dir)) {
      dir.create(run_dir, recursive = TRUE)
    }

    # if the progressive plot already exists, just include it
    if (file.exists(progressive_path)) {
      relative_path <- gsub(
        "^.*?(?=source/)",
        "",
        progressive_path,
        perl = TRUE
      )
      md_content <- paste0(
        md_content,
        "### ",
        run_id,
        "\n\n",
        "![](",
        relative_path,
        ")\n\n"
      )
      next
    }

    pupil_data <- eyeris$timeseries[[block]]
    if (is.null(pupil_data)) {
      log_warn("No pupil data for {run_id}")
      next
    }

    pupil_steps <- grep("^pupil_", names(pupil_data), value = TRUE)

    if (length(pupil_steps) < 2) {
      md_content <- paste0(
        md_content,
        "### ",
        run_id,
        "\n\n",
        "*Not enough preprocessing steps for progressive summary*\n\n"
      )
      next
    }

    grDevices::png(
      filename = progressive_path,
      width = 7000,
      height = 6000,
      res = 300
    )

    make_prog_summary_plot(
      pupil_data = pupil_data,
      pupil_steps = pupil_steps,
      preview_n = preview_n,
      plot_params = plot_params,
      run_id = run_id,
      eye_suffix = eye_suffix,
      full_pupil_data = get_pre_decimation_block(
        eyeris,
        sub("^block_", "", block)
      )
    )

    grDevices::dev.off()

    relative_path <- gsub("^.*?(?=source/)", "", progressive_path, perl = TRUE)

    md_content <- paste0(
      md_content,
      "### ",
      run_id,
      "\n\n",
      "![](",
      relative_path,
      ")\n\n"
    )
  }

  md_content
}
