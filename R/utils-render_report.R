render_report <- function(rmd_f, html, pdf) {
  rmarkdown::render(rmd_f, output_format = "html_document")

  if (pdf) {
    tryCatch(
      {
        rmarkdown::render(rmd_f, output_format = "pdf_document")
      },
      error = function(e) {
        cli::cli_alert_danger(paste(
          "Could not render eyeris report PDF.",
          "Do you have a TeX distribution installed?",
          "If not, consider TinyTeX:\n",
          "## install.packages('tinytex')\n",
          "## tinytex::install_tinytex()"
        ))
        base_file <- tools::file_path_sans_ext(rmd_f)
        unlink(paste0(base_file, ".log"))
        unlink(paste0(base_file, ".tex"))
      }
    )
  }

  unlink(rmd_f)
}

make_report <- function(eyeris, out, plots, ...) {
  # get extra subject params from bidsify.R
  params <- list(...)

  has_multiple_runs <- length(grep("run-\\d+", plots)) > 0

  # temp file
  rmd_f <- file.path(out, paste0("sub-", params$sub, ".Rmd"))

  report_date <- format(Sys.time(), "%B %d, %Y | %H:%M:%OS3")
  package_version <- as.character(
    utils::packageVersion("eyeris")
  )
  css <- system.file(
    file.path("rmarkdown", "css", "report.css"),
    package = "eyeris"
  )

  sticker_path <- system.file("figures", "sticker.png", package = "eyeris")

  run_info <- paste(
    " - Runs: ",
    paste(seq_len(length(grep("run-\\d+", unique(dirname(plots))))),
      collapse = ", "
    ),
    "\n"
  )

  # eyeris report markdown content
  content <- paste0(
    "---\n",
    "title: '`eyeris` report'\n",
    "date: '", report_date, "'\n",
    "output:\n",
    "  html_document:\n",
    "    df_print: paged\n",
    "    css: '", css, "'\n",
    "  pdf_document: default\n",
    "---\n\n",
    "\n\n<img src='", sticker_path, "' class='top-right-image'>",
    "\n\n---\n\n## Summary\n",
    " - Subject ID: ", params$sub, "\n",
    " - Session: ", params$ses, "\n",
    " - Task: ", params$task, "\n",
    run_info,
    " - BIDS Directory: ", out, "\n",
    " - Source `.asc` file: ", eyeris$file, "\n",
    " - [`eyeris` version](https://github.com/shawntz/eyeris): ",
    package_version, "\n",
    "\n\n<style type='text/css'>\n",
    "@import url('http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/",
    "bootstrap.min.css');\n",
    "@import url('https://cdn.jsdelivr.net/npm/lightbox2/dist/css/",
    "lightbox.min.css');\n</style>\n",
    "\n## Preprocessed Data Preview\n\n",
    save_detrend_plots(eyeris = eyeris, out_dir = out),
    print_plots(plots), "\n",
    "\n\n---\n\n## EyeLink Header Metadata\n\n",
    make_md_table(eyeris$info), "\n",
    "\n\n---\n\n## eyeris call stack\n\n",
    make_md_table(format_call_stack(eyeris$params)), "\n",
    "\n\n---\n\n### Citation\n\n",
    "```{r citation, echo=FALSE, comment=NA}\n",
    "citation('eyeris')\n",
    "```\n\n\n\n\n\n"
  )

  writeLines(content, con = rmd_f)

  rmd_f
}

# parse eyelink `info` metadata into a markdown table
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

print_plots <- function(plots) {
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

      if (length(run_plots) > 0) {
        run_num <- sub(".*run-(\\d+).*", "\\1", run_dir)

        md_plots <- paste0(
          md_plots,
          "## Run ", run_num, "\n\n"
        )

        # run's detrend diagnostic path
        detrend_plot_path <- file.path(
          run_dir,
          paste0("run-", run_num, "_detrend.png")
        )
        detrend_exists <- file.exists(detrend_plot_path)

        # sort by fig number
        plot_fig_ids <- as.numeric(sub(".*_fig-(\\d+)_.*", "\\1", run_plots))
        sorted_plot_paths <- run_plots[order(plot_fig_ids)]

        num_plots <- length(sorted_plot_paths)
        before_plot_index <- num_plots - 3
        after_plot_index <- num_plots - 1

        for (i in seq_along(sorted_plot_paths)) {
          relative_fig_path <- make_relative_path(sorted_plot_paths[i])

          if (i < before_plot_index) {
            md_plots <- paste0(
              md_plots,
              if (i %% 2 == 1) {
                paste0(
                  "### Step ", ceiling(i / 2), "\n",
                  "<div style='display: flex;'>",
                  "<img src='", relative_fig_path, "' width='50%' />"
                )
              } else {
                paste0(
                  "<img src='", relative_fig_path, "' width='50%' />",
                  "</div>"
                )
              }
            )
          } else if (i == before_plot_index || i == before_plot_index + 1) {
            md_plots <- paste0(
              md_plots,
              if (i == before_plot_index) "### Before", "\n",
              "![](", relative_fig_path, ")\n\n"
            )
          } else if (i == after_plot_index || i == after_plot_index + 1) {
            md_plots <- paste0(
              md_plots,
              if (i == after_plot_index) "### After", "\n",
              "![](", relative_fig_path, ")\n\n"
            )
          }
        }

        if (detrend_exists) {
          md_plots <- paste0(
            md_plots,
            "### Detrend Diagnostic\n\n",
            "![](", make_relative_path(detrend_plot_path), ")\n\n"
          )
        }
      }
    }
    md_plots
  }
}

save_detrend_plots <- function(eyeris, out_dir, preview_n = 3,
                               plot_params = list()) {
  blocks <- names(eyeris$timeseries)

  for (block in blocks) {
    block_number <- sub("block_", "", block)
    run_id <- sprintf("run-%02d", as.numeric(block_number))
    run_dir <- file.path(out_dir, "source", "figures", run_id)
    detrend_path <- file.path(run_dir, paste0(run_id, "_detrend.png"))

    if (!dir.exists(run_dir)) {
      dir.create(run_dir, recursive = TRUE)
    }

    pupil_data <- eyeris$timeseries[[block]]

    # only proceed if detrended values exist
    if ("detrend_fitted_values" %in% names(pupil_data) &&
          any(grepl("_detrend$", names(pupil_data)))) {
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

      message(sprintf("[Saved] %s", detrend_path))
    } else {
      message(sprintf("[Skipped] No detrend data found for %s", run_id))
    }
  }
}
