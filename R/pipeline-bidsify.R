#' Save out pupil timeseries data in a BIDS-like structure
#'
#' This method provides a structured way to save out pupil data in a BIDS-like
#' structure. The method saves out epoched data as well as the raw pupil
#' timeseries, and formats the directory and filename structures based on the
#' metadata you provide.
#'
#' In the future, we intend for this function to save out the data in an
#' official BIDS format for eyetracking data (see [the proposal currently under
#' review here](https://github.com/bids-standard/bids-specification/pull/1128)).
#' At this time, however, this function instead takes a more BIDS-inspired
#' approach to organizing the output files for preprocessed pupil data.
#'
#' @param eyeris An object of class `eyeris` dervived from [eyeris::load()].
#' @param save_all Logical flag indicating whether all epochs are to be saved
#' or only a subset of them. Defaults to TRUE.
#' @param epochs_list List of epochs to be saved. Defaults to NULL.
#' @param merge_epochs Logical flag indicating whether epochs should be saved
#' as one file or as separate files. Defaults to FLASE (no merge).
#' @param bids_dir Base bids_directory.
#' @param participant_id BIDS subject ID.
#' @param session_num BIDS session ID.
#' @param task_name BIDS task ID.
#' @param run_num BIDS run ID. For single files without blocks (i.e., runs),
#' `run_num` specifies which run this file represents. However, for files
#' with multiple recording blocks embedded within the **same** `.asc` file,
#' this parameter is ignored and blocks are automatically numbered as runs
#' (block 1 = run-01, block 2 = run-02, etc.) in the order they appeared/were
#' recorded.
#' @param merge_runs Logical flag indicating whether multiple runs (either
#' from multiple recording blocks existing within the **same** `.asc` file
#' (see above), or manually specified) should be combined into a single
#' output file. When TRUE, adds a 'run' column to identify the source run.
#' Defaults to FALSE (i.e., separate files per block/run -- the standard
#' BIDS-like-behavior).
#' @param save_raw Logical flag indicating whether to save_raw pupil data in
#' addition to epoched data. Defaults to TRUE.
#' @param html_report Logical flag indicating whether to save out the `eyeris`
#' preprocessing summary report as an HTML file. Defaults to FALSE.
#' @param pdf_report Logical flag indicating whether to save out the `eyeris`
#' preprocessing summary report as a PDF file. Note, a valid TeX distribution
#' must already be installed. Defaults to FALSE.
#' @param report_seed Random seed for the plots that will appear in the report.
#' Defaults to 0. See [eyeris::plot()] for a more detailed description.
#' @param report_epoch_grouping_var_col String name of grouping column to use
#' for epoch-by-epoch diagnostic plots in an interactive rendered HTML report.
#' Column name must exist (i.e., be a custom grouping variable name set within
#' the metadata template of your `epoch()` call). Defaults to `"matched_event"`,
#' which all epoched dataframes have as a valid column name. To disable these
#' epoch-level diagnostic plots, set to `NULL`.
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`. Set to `False` to suppress messages about the current
#' processing step and run silently.
#'
#' @return Invisibly returns `NULL`. Called for its side effects.
#'
#' @examples
#' # Bleed around blink periods just long enough to remove majority of
#' #  deflections due to eyelid movements
#' \donttest{
#' system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore() |>
#'   eyeris::epoch(
#'     events = "PROBE_{type}_{trial}",
#'     limits = c(-1, 1), # grab 1 second prior to and 1 second post event
#'     label = "prePostProbe" # custom epoch label name
#'   ) |>
#'   eyeris::bidsify(
#'     bids_dir = tempdir(),
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     run_num = "01",
#'     save_raw = TRUE, # save out raw timeseries
#'     html_report = TRUE, # generate interactive report document
#'     report_seed = 0 # make randomly selected plot epochs reproducible
#'   )
#' }
#'
#' @export
bidsify <- function(eyeris, save_all = TRUE, epochs_list = NULL,
                    merge_epochs = FALSE, bids_dir = NULL,
                    participant_id = NULL, session_num = NULL,
                    task_name = NULL, run_num = NULL, merge_runs = FALSE,
                    save_raw = TRUE, html_report = FALSE,
                    pdf_report = FALSE, report_seed = 0,
                    report_epoch_grouping_var_col = "matched_event",
                    verbose = TRUE) {
  # setup
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    if (!is.null(run_num)) {
      warning(
        paste0(
          "`run_num` is ignored when data contains multiple blocks.",
          "Blocks will be automatically numbered as runs."
        )
      )
    }
    # multiple blocks case - i.e., treat blocks as the runs
    has_multiple_runs <- TRUE
    num_runs <- length(eyeris$timeseries)
  } else {
    # single df fallback case
    has_multiple_runs <- FALSE
    num_runs <- 1
  }
  sub <- participant_id
  ses <- session_num
  task <- task_name
  dir <- bids_dir

  tryCatch(
    {
      check_data(eyeris, "bidsify")
    },
    error = function(e) {
      error_handler(e, "input_data_type_error")
    }
  )

  tryCatch(
    {
      check_input(arg = participant_id)
    },
    error = function(e) {
      error_handler(e, "input_arg_missing_error")
    }
  )

  tryCatch(
    {
      check_input(arg = task_name)
    },
    error = function(e) {
      error_handler(e, "input_arg_missing_error")
    }
  )

  tryCatch(
    {
      epochs <- filter_epochs(eyeris, epochs_list)
      count_epochs(epochs)
    },
    error = function(e) {
      error_handler(e, "epoch_count_error")
    }
  )

  if (save_all) {
    epochs_to_save <- eyeris[epochs]
  } else if (!is.null(epochs_list)) {
    epochs_to_save <- eyeris[epochs_list]
  } else {
    stop("Either save_all must be TRUE or epochs_list must be specified.")
  }

  check_and_create_dir(dir, verbose = verbose)
  p <- file.path("derivatives")
  check_and_create_dir(dir, p, verbose = verbose)

  if (!is.null(sub)) {
    p <- file.path(p, paste0("sub-", sub))
    check_and_create_dir(dir, p, verbose = verbose)
  }

  if (!is.null(ses)) {
    p <- file.path(p, paste0("ses-", ses))
    check_and_create_dir(dir, p, verbose = verbose)
  }

  # normalize report_path
  report_path <- file.path(bids_dir, p)
  report_path <- normalizePath(report_path, winslash = "/", mustWork = FALSE)

  p <- file.path(p, "eye")
  check_and_create_dir(dir, p, verbose = verbose)

  block_numbers <- get_block_numbers(eyeris)

  if (!merge_epochs) {
    if (has_multiple_runs) {
      # process each epoch-type separately, one-at-a-time
      lapply(names(epochs_to_save), function(epoch_id) {
        current_label <- substr(epoch_id, 7, nchar(epoch_id))

        if (merge_runs) {
          epochs_with_runs <- do.call(
            rbind, lapply(names(eyeris$timeseries), function(i) {
              run_epochs <- epochs_to_save[[epoch_id]][[i]]
              run_epochs$run <- sprintf("%02d", get_block_numbers(i))
              run_epochs
            })
          )

          f <- make_bids_fname(
            sub = sub, ses = ses, task = task,
            epoch = current_label, desc = "preproc_pupil_allruns"
          )

          if (verbose) {
            alert(
              "info",
              "Writing combined runs epoched data to '%s'...",
              file.path(dir, p, f)
            )
          }

          write.csv(epochs_with_runs,
            file = file.path(bids_dir, p, f),
            row.names = FALSE
          )

          if (verbose) {
            alert(
              "success",
              "Combined runs epoched data written to: '%s'",
              file.path(dir, p, f)
            )
          }
        } else {
          lapply(names(eyeris$timeseries), function(i) {
            run_epochs <- epochs_to_save[[epoch_id]][[i]]

            f <- make_bids_fname(
              sub = sub, ses = ses, task = task,
              run = sprintf("%02d", get_block_numbers(i)),
              epoch = current_label, desc = "preproc_pupil"
            )

            if (verbose) {
              alert(
                "info",
                "Writing run %02d epoched data to '%s'...",
                get_block_numbers(i), file.path(dir, p, f)
              )
            }

            write.csv(run_epochs,
              file = file.path(bids_dir, p, f),
              row.names = FALSE
            )

            if (verbose) {
              alert(
                "success",
                "Run %02d epoched data written to: '%s'",
                get_block_numbers(i), file.path(dir, p, f)
              )
            }
          })
        }
      })
    } else {
      # original single-run handler method
      lapply(names(epochs_to_save), function(epoch_id) {
        current_label <- substr(epoch_id, 7, nchar(epoch_id))

        f <- make_bids_fname(
          sub = sub, ses = ses, task = task,
          run = sprintf("%02d", as.numeric(run_num)),
          epoch = current_label, desc = "preproc_pupil"
        )

        if (verbose) {
          alert(
            "info",
            "Writing epoched data to '%s'...", file.path(dir, p, f)
          )
        }

        write.csv(epochs_to_save[[epoch_id]],
          file = file.path(bids_dir, p, f),
          row.names = FALSE
        )

        if (verbose) {
          alert(
            "success", "Epoched data written to: '%s'",
            file.path(dir, p, f)
          )
        }
      })
    }
  } else {
    # merge all epochs and runs (if multiple runs exist)
    if (has_multiple_runs && merge_runs) {
      merged_epochs <- do.call(
        rbind, lapply(names(epochs_to_save), function(epoch_id) {
          epochs_with_runs <- do.call(
            rbind, lapply(names(eyeris$timeseries), function(i) {
              run_epochs <- epochs_to_save[[epoch_id]][[i]]
              run_epochs$run <- sprintf("%02d", get_block_numbers(i))
              run_epochs$epoch_type <- epoch_id
              run_epochs
            })
          )
          epochs_with_runs
        })
      )

      f <- make_bids_fname(
        sub = sub, ses = ses, task = task,
        epoch = "all", desc = "preproc_pupil_allruns"
      )
    } else {
      merged_epochs <- do.call(
        rbind, lapply(names(epochs_to_save), function(epoch_id) {
          epochs <- epochs_to_save[[epoch_id]]
          epochs$epoch_type <- epoch_id
          epochs
        })
      )

      f <- make_bids_fname(
        sub = sub, ses = ses, task = task,
        run = sprintf("%02d", as.numeric(run_num)),
        epoch = "all", desc = "preproc_pupil"
      )
    }

    if (verbose) {
      alert("info", "Writing merged epochs to '%s'...", file.path(dir, p, f))
    }

    write.csv(merged_epochs,
      file = file.path(bids_dir, p, f),
      row.names = FALSE
    )

    if (verbose) {
      alert(
        "success", "Merged epochs written to: '%s'",
        file.path(dir, p, f)
      )
    }
  }

  if (save_raw) {
    if (has_multiple_runs) {
      if (merge_runs) {
        # save all runs together
        combined_timeseries <- do.call(
          rbind, lapply(seq_len(num_runs), function(i) {
            if (has_multiple_runs) {
              run_data <- eyeris$timeseries[[i]]
            } else {
              run_data <- eyeris$timeseries
            }
            run_data$run <- sprintf("%02d", i)
            run_data
          })
        )

        f <- make_bids_fname(
          sub = sub, ses = ses, task = task,
          desc = "timeseries_pupil_allruns"
        )

        if (verbose) {
          alert(
            "info", "Writing combined raw pupil timeseries to '%s'...",
            file.path(dir, p, f)
          )
        }

        write.csv(combined_timeseries,
          file.path(dir, p, f),
          row.names = FALSE
        )

        if (verbose) {
          alert(
            "success",
            "Combined raw pupil timeseries written to: '%s'",
            file.path(dir, p, f)
          )
        }
      } else {
        # save each run separately
        lapply(seq_len(num_runs), function(i) {
          if (has_multiple_runs) {
            run_data <- eyeris$timeseries[[i]]
          } else {
            run_data <- eyeris$timeseries
          }

          f <- make_bids_fname(
            sub = sub, ses = ses, task = task,
            run = sprintf("%02d", i), # BIDS format
            desc = "timeseries_pupil"
          )

          if (verbose) {
            alert(
              "info",
              "Writing run %02d raw pupil timeseries to '%s'...",
              i, file.path(dir, p, f)
            )
          }

          write.csv(run_data, file.path(dir, p, f), row.names = FALSE)

          if (verbose) {
            alert(
              "success",
              "Run %02d raw pupil timeseries written to: '%s'",
              i, file.path(dir, p, f)
            )
          }
        })
      }
    } else {
      # single run fallback case
      f <- make_bids_fname(
        sub = sub, ses = ses, task = task,
        run = sprintf("%02d", as.numeric(run_num)),
        desc = "timeseries_pupil"
      )

      if (verbose) {
        alert(
          "info", "Writing single raw pupil timeseries to '%s'...",
          file.path(dir, p, f)
        )
      }

      write.csv(eyeris$timeseries, file.path(dir, p, f), row.names = FALSE)

      if (verbose) {
        alert(
          "success", "Single raw pupil timeseries written to: '%s'",
          file.path(dir, p, f)
        )
      }
    }
  }

  should_render_report <- html_report || pdf_report

  if (should_render_report) {
    # normalize the bids_dir path
    bids_dir <- normalizePath(path.expand(bids_dir), mustWork = FALSE)

    # create full path for figures
    figs_out <- file.path(report_path, "source", "figures")

    # create directories with normalized path
    check_and_create_dir(figs_out, verbose = verbose)

    fig_paths <- c()

    # first check if there are multiple runs
    if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
      has_multiple_runs <- TRUE
      num_runs <- length(eyeris$timeseries)
    } else {
      has_multiple_runs <- FALSE
      num_runs <- 1
    }

    for (i_run in block_numbers) {
      current_data <- if (has_multiple_runs) {
        eyeris$timeseries[[paste0("block_", i_run)]]
      } else {
        eyeris$timeseries
      }

      pupil_steps <- grep("^pupil_", colnames(current_data), value = TRUE)
      run_fig_paths <- rep(NA, length(pupil_steps) * 2)

      run_dir <- file.path(figs_out, sprintf("run-%02d", i_run))
      check_and_create_dir(run_dir, verbose = verbose)

      # make step-by-step plots
      plot_types <- c("timeseries", "histogram")

      for (i in seq_along(pupil_steps)) {
        for (p in seq_along(plot_types)) {
          fig_name <- sprintf(
            "run-%02d_fig-%d_desc-%s.jpg",
            i_run, i, plot_types[p]
          )
          run_fig_paths[(i - 1) * 2 + p] <- file.path(run_dir, fig_name)
        }
      }

      for (i in seq_along(run_fig_paths)) {
        plot_dist <- i %% 2 == 0
        # First plot
        jpeg(run_fig_paths[i],
          width = 12, height = 7, units = "in",
          res = 300, pointsize = 14
        )
        plot(eyeris,
          steps = ceiling(i / 2),
          seed = report_seed,
          block = i_run,
          plot_distributions = plot_dist
        )
        dev.off()
      }

      # make full timeseries plots
      for (p in seq_along(plot_types)) {
        plot_dist <- p %% 2 == 0

        run_fig_paths <- c(
          run_fig_paths,
          file.path(
            run_dir,
            sprintf(
              "run-%02d_fig-%d_desc-%s.jpg",
              i_run,
              ceiling(length(run_fig_paths) / 2 + 1), plot_types[p]
            )
          )
        )

        jpeg(file.path(run_fig_paths[length(run_fig_paths)]),
          width = 12, height = 7, units = "in", res = 300, pointsize = 18
        )
        plot(eyeris,
          steps = 1,
          preview_window = c(0, nrow(current_data)),
          block = i_run, plot_distributions = plot_dist
        )
        dev.off()
      }

      for (p in seq_along(plot_types)) {
        plot_dist <- p %% 2 == 0

        run_fig_paths <- c(
          run_fig_paths,
          file.path(
            run_dir,
            sprintf(
              "run-%02d_fig-%d_desc-%s.jpg",
              i_run,
              ceiling(length(run_fig_paths) / 2 + 1), plot_types[p]
            )
          )
        )

        jpeg(file.path(run_fig_paths[length(run_fig_paths)]),
          width = 12, height = 7, units = "in", res = 300, pointsize = 18
        )
        plot(eyeris,
          steps = length(pupil_steps),
          preview_window = c(0, nrow(current_data)),
          block = i_run,
          plot_distributions = plot_dist
        )
        dev.off()
      }

      fig_paths <- c(fig_paths, run_fig_paths)
    }

    # now handle epochs (if present)
    if (!is.null(report_epoch_grouping_var_col)) {
      for (i in seq_along(epochs_to_save)) {
        for (bn in names(epochs_to_save[[i]])) {
          if (nrow(epochs_to_save[[i]][[bn]]) > 0) {
            tryCatch(
              {
                check_column(
                  epochs_to_save[[i]][[bn]],
                  report_epoch_grouping_var_col
                )
              },
              error = function(e) {
                error_handler(e, "column_doesnt_exist_in_df_error")
              }
            )

            run_dir <- file.path(
              figs_out,
              sprintf(
                "run-%02d",
                get_block_numbers(bn)
              )
            )
            check_and_create_dir(run_dir, verbose = verbose)
            epochs_out <- file.path(run_dir, names(epochs_to_save)[i])
            check_and_create_dir(epochs_out, verbose = verbose)

            # nolint start
            epoch_groups <- as.vector(
              unique(epochs_to_save[[i]][[bn]]
              [report_epoch_grouping_var_col])[[1]]
            )
            # nolint end

            for (group in epoch_groups) {
              group_df <- epochs_to_save[[i]][[bn]]
              group_df <- group_df[
                group_df[[report_epoch_grouping_var_col]] == group,
              ]

              for (pstep in seq_along(pupil_steps)) {
                if (grepl("z", pupil_steps[pstep])) {
                  y_units <- "(z)"
                } else {
                  y_units <- "(a.u.)"
                }

                colors <- c("black", rainbow(length(pupil_steps) - 1))
                y_label <- paste("pupil size", y_units)

                file_out <- file.path(epochs_out, sprintf(
                  "run-%02d_%s_%d.png",
                  get_block_numbers(bn), group, pstep
                ))
                png(file_out,
                  width = 3.25,
                  height = 2.5,
                  units = "in",
                  res = 600,
                  pointsize = 6
                )
                plot(group_df$timebin, group_df[[pupil_steps[pstep]]],
                  type = "l", xlab = "time (s)", ylab = y_label,
                  col = colors[pstep],
                  main = paste0(
                    group, "\n", pupil_steps[pstep],
                    sprintf(" (Run %d)", get_block_numbers(bn))
                  )
                )
                dev.off()
              }
            }
            epochs <- list.files(epochs_out,
              full.names = FALSE,
              pattern = "\\.(jpg|jpeg|png|gif)$",
              ignore.case = TRUE
            )

            epochs <- file.path(
              "source", "figures",
              sprintf("run-%02d", get_block_numbers(bn)),
              names(epochs_to_save)[i],
              epochs
            )

            make_gallery(eyeris, epochs, report_path,
              sprintf(
                "%s%s",
                names(epochs_to_save)[i],
                sprintf("_run-%02d", get_block_numbers(bn))
              ),
              sub = sub, ses = ses, task = task,
              run = sprintf("%02d", get_block_numbers(bn))
            )
          }
        }
      }
    }

    # make final report
    report_output <- make_report(
      eyeris,
      report_path,
      fig_paths,
      sub = sub, ses = ses, task = task
    )

    render_report(report_output, html = html_report, pdf = pdf_report)
  }

  invisible(NULL)
}

make_bids_fname <- function(sub = sub, task = task, run = run,
                            desc = "", ses = NULL, epoch = NULL) {
  if (!is.null(ses)) {
    if (!is.null(epoch)) {
      f <- paste0(
        "sub-", sub,
        "_ses-", ses,
        "_task-", task,
        "_run-", run,
        "_epoch-", epoch,
        "_desc-", desc,
        ".csv"
      )
    } else {
      f <- paste0(
        "sub-", sub,
        "_ses-", ses,
        "_task-", task,
        "_run-", run,
        "_desc-", desc,
        ".csv"
      )
    }
  } else {
    if (!is.null(epoch)) {
      f <- paste0(
        "sub-", sub,
        "_task-", task,
        "_run-", run,
        "_epoch-", epoch,
        "_desc-", desc,
        ".csv"
      )
    } else {
      f <- paste0(
        "sub-", sub,
        "_task-", task,
        "_run-", run,
        "_desc-", desc, ".csv"
      )
    }
  }

  return(f)
}
