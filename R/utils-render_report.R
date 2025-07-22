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

#' Create eyeris report
#'
#' Generates a comprehensive HTML report for eyeris preprocessing results.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param out Output directory for the report
#' @param plots Vector of plot file paths to include in the report
#' @param eye_suffix Optional eye suffix (e.g., "eye-L", "eye-R") for binocular data
#' @param ... Additional parameters passed from bidsify
#'
#' @return Path to the generated R Markdown file
#'
#' @keywords internal
make_report <- function(eyeris, out, plots, eye_suffix = NULL, ...) {
  # get extra subject params from bidsify.R
  params <- list(...)

  has_multiple_runs <- length(grep("run-\\d+", plots)) > 0

  # temp file - include eye_suffix in filename if provided
  report_filename <- paste0("sub-", params$sub)
  if (!is.null(eye_suffix)) {
    report_filename <- paste0(report_filename, "_", eye_suffix)
  }
  report_filename <- paste0(report_filename, ".Rmd")
  rmd_f <- file.path(out, report_filename)

  report_date <- format(Sys.time(), "%B %d, %Y | %H:%M:%OS3")
  package_version <- as.character(
    utils::packageVersion("eyeris")
  )
  css <- system.file(
    file.path("rmarkdown", "css", "report.css"),
    package = "eyeris"
  )

  sticker_path <- system.file("figures", "sticker.png", package = "eyeris")

  run_ids <- list.dirs(
    file.path(out, "source", "figures"),
    recursive = FALSE,
    full.names = FALSE
  )
  run_ids <- sort(
    as.integer(
      gsub("run-", "", grep("^run-\\d+$", run_ids, value = TRUE))
    )
  )

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
    heatmap_path <- file.path(
      "source",
      "figures",
      sprintf("run-%02d", run_id),
      sprintf("run-%02d_gaze_heatmap", run_id)
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
    correlation_path <- file.path(
      "source",
      "figures",
      sprintf("run-%02d", run_id),
      sprintf("run-%02d_binocular_correlation.png", run_id)
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

    meta_path <- file.path(metadata_dir, sprintf("run-%02d_metadata.json", run_id))

    if (!file.exists(meta_path)) {
      jsonlite::write_json(
        run_metadata,
        meta_path,
        pretty = TRUE,
        auto_unbox = TRUE
      )
    } else {
      cli::cli_alert_warning(
        sprintf("[WARN] Metadata file already exists for %s: %s", run_id, meta_path)
      )
    }

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

  title <- "`eyeris` preprocessing summary report"

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
    "\n## Preprocessing Summaries\n\n",
    save_progressive_summary_plots(eyeris = eyeris, out_dir = out, eye_suffix = eye_suffix),
    "\n\n## Preprocessed Data Previews\n\n",
    save_detrend_plots(eyeris = eyeris, out_dir = out, eye_suffix = eye_suffix),
    print_plots(plots, eye_suffix = eye_suffix),
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
    "```\n\n\n\n\n\n"
  )

  writeLines(content, con = rmd_f)

  rmd_f
}

#' Create markdown table from dataframe
#'
#' Converts a dataframe into a markdown table.
#'
#' @param df The dataframe to convert
#'
#' @return A character string containing the markdown table content
#'
#' @keywords internal
make_md_table <- function(df) {
  md_table <- "| Property | Value |\n|----|----|\n"
  for (prop in colnames(df)) {
    val <- df[[1, prop]]
    md_table <- paste0(
      md_table,
      "| ",
      prop,
      " | ",
      val,
      " |\n"
    )
  }

  md_table
}

#' Create multiline markdown table from dataframe
#'
#' Converts a dataframe into a multiline markdown table.
#'
#' @param df The dataframe to convert
#'
#' @return A character string containing the markdown table content
#'
#' @keywords internal
make_md_table_multiline <- function(df) {
  md_table <- paste0("| ", paste(colnames(df), collapse = " | "), " |\n")
  md_table <- paste0(md_table, "|", paste(rep("---", ncol(df)), collapse = "|"), "|\n")
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

sanitize_call_stack <- function(x) {
  if (is.call(x)) {
    deparse(x)
  } else if (is.list(x)) {
    lapply(x, sanitize_call_stack)
  } else {
    x
  }
}

#' Print plots in markdown format
#'
#' Generates markdown code to display plots in the report.
#'
#' @param plots Vector of plot file paths
#' @param eye_suffix Optional eye suffix for binocular data
#'
#' @return A character string containing markdown plot references
#'
#' @keywords internal
print_plots <- function(plots, eye_suffix = NULL) {
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

  if (length(run_dirs) > 0) {
    for (run_dir in run_dirs) {
      run_plots <- list.files(run_dir, pattern = "*.jpg", full.names = TRUE)

      if (!is.null(eye_suffix)) {
        run_plots <- run_plots[grepl(eye_suffix, run_plots)]
      }

      if (length(run_plots) > 0) {
        run_num <- sub(".*run-(\\d+).*$", "\\1", run_dir)

        md_plots <- paste0(
          md_plots,
          "### run-",
          run_num,
          "\n\n"
        )

        # sort by fig number if possible
        plot_fig_ids <- suppressWarnings(
          as.numeric(sub(".*_fig-(\\d+)_.*", "\\1", run_plots))
        )
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
              any(
                grepl(
                  paste(placeholder_patterns, collapse = "|"),
                  x,
                  ignore.case = TRUE
                )
              )
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

        # detrend diagnostics - check for eye_suffix version first
        detrend_plot_path <- file.path(
          run_dir,
          paste0("run-", run_num, "_detrend.png")
        )

        # if eye_suffix is provided, look for the suffixed version
        if (!is.null(eye_suffix)) {
          detrend_plot_path <- file.path(
            run_dir,
            paste0("run-", run_num, "_detrend_", eye_suffix, ".png")
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
#' Generates and saves detrend diagnostic plots for each block in the eyeris
#' object.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param out_dir Output directory for saving plots
#' @param preview_n Number of preview samples for plotting
#' @param plot_params Additional plotting parameters
#' @param eye_suffix Optional eye suffix for binocular data
#'
#' @return No return value; saves detrend plots to the specified directory
#'
#' @keywords internal
save_detrend_plots <- function(eyeris, out_dir, preview_n = 3, plot_params = list(), eye_suffix = NULL) {
  blocks <- names(eyeris$timeseries)

  for (block in blocks) {
    block_number <- sub("block_", "", block)
    run_id <- sprintf("run-%02d", as.numeric(block_number))
    run_dir <- file.path(out_dir, "source", "figures", run_id)
    detrend_filename <- paste0(run_id, "_detrend")
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
    if ("detrend_fitted_values" %in% names(pupil_data) && any(grepl("_detrend$", names(pupil_data)))) {
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

      cli::cli_alert_info(sprintf("[INFO] %s", detrend_path))
    } else {
      cli::cli_alert_warning(sprintf("[WARN] No detrend data found for %s", run_id))
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
#' @param pupil_data A data frame containing pupil timeseries data with
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
  eye_suffix = NULL
) {
  plot_steps <- pupil_steps[!grepl("_z$", pupil_steps)]

  time_range <- range(pupil_data$time_secs, na.rm = TRUE)
  start_idx <- which.min(abs(pupil_data$time_secs - time_range[1]))
  end_idx <- which.min(abs(pupil_data$time_secs - time_range[2]))

  time_subset <- pupil_data$time_secs[start_idx:end_idx]
  layer_data <- list()
  for (i in seq_along(plot_steps)) {
    step_data <- pupil_data[[plot_steps[i]]][start_idx:end_idx]
    valid_indices <- is.finite(step_data)
    if (sum(valid_indices) < 100) {
      next
    }
    layer_data[[i]] <- list(
      time = time_subset[valid_indices],
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
    text(0.5, 0.5, "Not enough preprocessing steps\nfor progressive summary", cex = 1.2, col = "red")
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
#' Generates and saves progressive summary plots for each block in the eyeris
#' object.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param out_dir Output directory for saving plots
#' @param preview_n Number of preview samples for plotting
#' @param plot_params Additional plotting parameters
#' @param eye_suffix Optional eye suffix for binocular data
#'
#' @return A character string containing markdown references to the saved plots
#'
#' @keywords internal
save_progressive_summary_plots <- function(eyeris, out_dir, preview_n = 3, plot_params = list(), eye_suffix = NULL) {
  run_dirs <- list.dirs(
    file.path(out_dir, "source", "figures"),
    recursive = FALSE,
    full.names = FALSE
  )
  run_ids <- sort(as.integer(
    gsub("run-", "", grep("^run-\\d+$", run_dirs, value = TRUE))
  ))

  md_content <- paste(
    "This visualization shows how the pupil timeseries changes across",
    "preprocessing steps. ",
    "Each layer represents a different",
    "preprocessing step, with the earliest step at the back ",
    "and the",
    "final step at the front (via a subtle horizontal offset effect).\n\n"
  )

  for (run_id in run_ids) {
    block <- paste0("block_", run_id)
    run_id <- sprintf("run-%02d", run_id)
    run_dir <- file.path(out_dir, "source", "figures", run_id)
    progressive_filename <- paste0(run_id, "_desc-progressive_summary")
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
      cli::cli_alert_warning(
        sprintf("[WARN] No pupil data for %s", run_id)
      )
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
      eye_suffix = eye_suffix
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
