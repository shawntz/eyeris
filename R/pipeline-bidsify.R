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
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param save_all Logical flag indicating whether all epochs are to be saved
#' or only a subset of them. Defaults to `TRUE`
#' @param epochs_list List of epochs to be saved. Defaults to `NULL`
#' @param merge_epochs Logical flag indicating whether epochs should be saved
#' as one file or as separate files. Defaults to `FALSE` (no merge)
#' @param bids_dir Base bids_directory. Defaults to `NULL`
#' @param participant_id BIDS subject ID. Defaults to `NULL`
#' @param session_num BIDS session ID. Defaults to `NULL`
#' @param task_name BIDS task ID. Defaults to `NULL`
#' @param run_num BIDS run ID. Optional override for the run number when there's
#' only one block of data present in a given `.asc` file. This allows you to
#' manually specify a run number (e.g., "03") instead of using the default block
#' number in `.asc` files (1). This is especially useful if you have a single
#' `.asc` file for a single run of a task and want your BIDSified derivatives to
#' be labeled correctly. However, for files with multiple recording blocks
#' embedded within the **same** `.asc` file, this parameter is ignored and
#' blocks are automatically numbered as runs (block 1 = run-01, block 2 =
#' run-02, etc.) in the order they appeared/were recorded. Defaults to `NULL`
#' (no override)
#' @param merge_runs Logical flag indicating whether multiple runs (either
#' from multiple recording blocks existing within the **same** `.asc` file
#' (see above), or manually specified) should be combined into a single
#' output file. When `TRUE`, adds a 'run' column to identify the source run
#' Defaults to `FALSE` (i.e., separate files per block/run -- the standard
#' BIDS-like-behavior)
#' @param save_raw Logical flag indicating whether to save_raw pupil data in
#' addition to epoched data. Defaults to `TRUE`
#' @param html_report Logical flag indicating whether to save out the `eyeris`
#' preprocessing summary report as an HTML file. Defaults to `TRUE`
#' @param report_seed Random seed for the plots that will appear in the report
#' Defaults to `0`. See [eyeris::plot()] for a more detailed description
#' @param report_epoch_grouping_var_col String name of grouping column to use
#' for epoch-by-epoch diagnostic plots in an interactive rendered HTML report.
#' Column name must exist (i.e., be a custom grouping variable name set within
#' the metadata template of your `epoch()` call).
#' Defaults to `"matched_event"`, which all epoched dataframes have as a valid
#' column name. To disable these epoch-level diagnostic plots, set to `NULL`
#' @param verbose A flag to indicate whether to print detailed logging messages.
#' Defaults to `TRUE`. Set to `FALSE` to suppress messages about the current
#' processing step and run silently
#'
#' @param pdf_report **(Deprecated)** Use `html_report = TRUE` instead
#'
#' @return Invisibly returns `NULL`. Called for its side effects
#'
#' @seealso [lifecycle::deprecate_warn()]
#'
#' @examples
#' # Bleed around blink periods just long enough to remove majority of
#' #  deflections due to eyelid movements
#' \donttest{
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # example with unepoched data
#' demo_data |>
#'   eyeris::glassbox() |>
#'   eyeris::bidsify(
#'     bids_dir = tempdir(), # <- MAKE SURE TO UPDATE TO YOUR DESIRED LOCAL PATH
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     run_num = "01",
#'     save_raw = TRUE, # save out raw timeseries
#'     html_report = TRUE, # generate interactive report document
#'     report_seed = 0 # make randomly selected plot epochs reproducible
#'   )
#'
#' # example with epoched data
#' demo_data |>
#'   eyeris::glassbox() |>
#'   eyeris::epoch(
#'     events = "PROBE_{startstop}_{trial}",
#'     limits = c(-1, 1), # grab 1 second prior to and 1 second post event
#'     label = "prePostProbe" # custom epoch label name
#'   ) |>
#'   eyeris::bidsify(
#'     bids_dir = tempdir(), # <- MAKE SURE TO UPDATE TO YOUR DESIRED LOCAL PATH
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     run_num = "01"
#'   )
#'
#' # example with run_num for single block data
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   eyeris::glassbox() |>
#'   eyeris::epoch(
#'     events = "PROBE_{startstop}_{trial}",
#'     limits = c(-1, 1),
#'     label = "prePostProbe"
#'   ) |>
#'   eyeris::bidsify(
#'     bids_dir = tempdir(),
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     run_num = "03" # override default run-01 (block_1) to use run-03 instead
#'   )
#' }
#'
#' @export
bidsify <- function(
  eyeris,
  save_all = TRUE,
  epochs_list = NULL,
  merge_epochs = FALSE,
  bids_dir = NULL,
  participant_id = NULL,
  session_num = NULL,
  task_name = NULL,
  run_num = NULL,
  merge_runs = FALSE,
  save_raw = TRUE,
  html_report = TRUE,
  report_seed = 0,
  report_epoch_grouping_var_col = "matched_event",
  verbose = TRUE,
  pdf_report = deprecated()
) {
  # deprecation warning for pdf_report ---------------------------------
  if (is_present(pdf_report)) {
    deprecate_warn(
      "1.3.0",
      "bidsify(pdf_report)",
      "bidsify(html_report)"
    )
    html_report <- pdf_report
  }

  # setup --------------------------------------------------------------
  if (is_binocular_object(eyeris)) {
    left_eyeris <- eyeris$left
    right_eyeris <- eyeris$right

    run_bidsify(
      left_eyeris,
      save_all = save_all,
      epochs_list = epochs_list,
      merge_epochs = merge_epochs,
      bids_dir = bids_dir,
      participant_id = participant_id,
      session_num = session_num,
      task_name = task_name,
      run_num = run_num,
      merge_runs = merge_runs,
      save_raw = save_raw,
      html_report = html_report,
      report_seed = report_seed,
      report_epoch_grouping_var_col = report_epoch_grouping_var_col,
      eye_suffix = "eye-L",
      verbose = verbose,
      raw_binocular_object = eyeris$raw_binocular_object
    )

    run_bidsify(
      right_eyeris,
      save_all = save_all,
      epochs_list = epochs_list,
      merge_epochs = merge_epochs,
      bids_dir = bids_dir,
      participant_id = participant_id,
      session_num = session_num,
      task_name = task_name,
      run_num = run_num,
      merge_runs = merge_runs,
      save_raw = save_raw,
      html_report = html_report,
      report_seed = report_seed,
      report_epoch_grouping_var_col = report_epoch_grouping_var_col,
      eye_suffix = "eye-R",
      verbose = verbose,
      raw_binocular_object = eyeris$raw_binocular_object
    )
  } else {
    run_bidsify(
      eyeris,
      save_all = save_all,
      epochs_list = epochs_list,
      merge_epochs = merge_epochs,
      bids_dir = bids_dir,
      participant_id = participant_id,
      session_num = session_num,
      task_name = task_name,
      run_num = run_num,
      merge_runs = merge_runs,
      save_raw = save_raw,
      html_report = html_report,
      report_seed = report_seed,
      report_epoch_grouping_var_col = report_epoch_grouping_var_col,
      verbose = verbose,
      raw_binocular_object = eyeris$raw_binocular_object
    )
  }
}

#' Internal function to run bidsify on a single eye
#' @param eyeris An eyeris object
#' @param save_all Whether to save all data
#' @param epochs_list A list of epochs to include
#' @param merge_epochs Whether to merge epochs
#' @param bids_dir The directory to save the bids data
#' @param participant_id The participant id
#' @param session_num The session number
#' @param task_name The task name
#' @param run_num The run number
#' @param merge_runs Whether to merge runs
#' @param save_raw Whether to save raw data
#' @param html_report Whether to generate an html report
#' @param report_seed The seed for the report
#' @param report_epoch_grouping_var_col The column to use for grouping epochs in the report
#' @param eye_suffix The suffix to add to the eye data
#' @param verbose Whether to print verbose output
#' @param raw_binocular_object The raw binocular object
#'
#' @return A eyeris object
#'
#' @keywords internal
run_bidsify <- function(
  eyeris,
  save_all = TRUE,
  epochs_list = NULL,
  merge_epochs = FALSE,
  bids_dir = NULL,
  participant_id = NULL,
  session_num = NULL,
  task_name = NULL,
  run_num = NULL,
  merge_runs = FALSE,
  save_raw = TRUE,
  html_report = TRUE,
  report_seed = 0,
  report_epoch_grouping_var_col = "matched_event",
  eye_suffix = NULL,
  verbose = TRUE,
  raw_binocular_object = NULL
) {
  actual_block_count <- length(eyeris$timeseries)

  if (actual_block_count > 1) {
    # case: multiple blocks: show warning if run_num was provided
    if (!is.null(run_num)) {
      cli::cli_alert_warning(
        paste0(
          "[WARN] `run_num` is ignored when data contains multiple blocks.",
          "Blocks will be automatically numbered as runs (block 1 = run-01,",
          "block 2 = run-02, etc.) in the order they appeared/were recorded."
        )
      )
    }
    has_multiple_runs <- TRUE
    num_runs <- actual_block_count
  } else {
    # case: single block in list format, here allow run_num override
    has_multiple_runs <- FALSE
    num_runs <- 1

    if (verbose) {
      cli::cli_alert_info("[INFO] Only 1 block detected...")
    }

    original_block_name <- names(eyeris$timeseries)[1]
    run_num_stripped <- as.character(as.integer(run_num))
    new_block_name <- paste0("block_", run_num_stripped)

    if (!is.null(run_num)) {
      cli::cli_alert_info(
        sprintf("[INFO] Using run_num = %s for single block data", run_num)
      )

      names(eyeris$timeseries)[1] <- new_block_name

      if ("block" %in% colnames(eyeris$timeseries[[1]])) {
        eyeris$timeseries[[1]]$block <- as.numeric(run_num)
      }

      names(eyeris$events)[1] <- new_block_name
      if ("block" %in% colnames(eyeris$events[[1]])) {
        eyeris$events[[1]]$block <- as.numeric(run_num)
      }

      names(eyeris$blinks)[1] <- new_block_name
      if ("block" %in% colnames(eyeris$blinks[[1]])) {
        eyeris$blinks[[1]]$block <- as.numeric(run_num)
      }

      names(eyeris$latest)[1] <- new_block_name
    }

    epoch_names <- names(eyeris)[grep("^epoch_", names(eyeris))]
    for (epoch_name in epoch_names) {
      current_block_names <- names(eyeris[[epoch_name]])
      is_epoch_list <- is.list(eyeris[[epoch_name]])
      epoch_info <- eyeris[[epoch_name]]$info

      if (is_epoch_list && original_block_name %in% current_block_names) {
        current_block_names[current_block_names == original_block_name] <- new_block_name

        if (!is.null(epoch_info)) {
          names(epoch_info)[names(epoch_info) == original_block_name] <- new_block_name
          eyeris[[epoch_name]]$info <- epoch_info # <-- ensure changes are saved back
        }

        if (
          is.data.frame(eyeris[[epoch_name]][[new_block_name]]) &&
            "block" %in% colnames(eyeris[[epoch_name]][[new_block_name]])
        ) {
          eyeris[[epoch_name]][[new_block_name]]$block <- as.numeric(run_num)
        }
      }
    }

    if (!is.null(eyeris$confounds)) {
      if (
        !is.null(eyeris$confounds$unepoched_timeseries) &&
          original_block_name %in% names(eyeris$confounds$unepoched_timeseries)
      ) {
        names(eyeris$confounds$unepoched_timeseries)[
          names(eyeris$confounds$unepoched_timeseries) == original_block_name
        ] <- new_block_name
      }

      if (!is.null(eyeris$confounds$epoched_timeseries)) {
        for (epoch_name in names(eyeris$confounds$epoched_timeseries)) {
          if (original_block_name %in% names(eyeris$confounds$epoched_timeseries[[epoch_name]])) {
            names(eyeris$confounds$epoched_timeseries[[epoch_name]])[
              names(eyeris$confounds$epoched_timeseries[[epoch_name]]) == original_block_name
            ] <- new_block_name
          }
        }
      }

      if (!is.null(eyeris$confounds$epoched_epoch_wide)) {
        for (epoch_name in names(eyeris$confounds$epoched_epoch_wide)) {
          if (original_block_name %in% names(eyeris$confounds$epoched_epoch_wide[[epoch_name]])) {
            names(eyeris$confounds$epoched_epoch_wide[[epoch_name]])[
              names(eyeris$confounds$epoched_epoch_wide[[epoch_name]]) == original_block_name
            ] <- new_block_name
          }
        }
      }
    }

    baseline_names <- names(eyeris)[grep("^baseline_", names(eyeris))]
    for (baseline_name in baseline_names) {
      if (is.list(eyeris[[baseline_name]]) && original_block_name %in% names(eyeris[[baseline_name]])) {
        names(eyeris[[baseline_name]])[names(eyeris[[baseline_name]]) == original_block_name] <- new_block_name

        if (!is.null(eyeris[[baseline_name]]$info) && original_block_name %in% names(eyeris[[baseline_name]]$info)) {
          names(eyeris[[baseline_name]]$info)[
            names(eyeris[[baseline_name]]$info) == original_block_name
          ] <- new_block_name
        }

        if (
          is.data.frame(eyeris[[baseline_name]][[new_block_name]]) &&
            "block" %in% colnames(eyeris[[baseline_name]][[new_block_name]])
        ) {
          eyeris[[baseline_name]][[new_block_name]]$block <- as.numeric(run_num)
        }
      }
    }
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

  epochs <- filter_epochs(eyeris, epochs_list)
  n_epochs <- length(epochs)
  any_epochs <- n_epochs > 0

  if (verbose) {
    cli::cli_alert_info(
      sprintf("[INFO] Filtered epochs: %s", paste(epochs, collapse = ", "))
    )
  }

  if (save_all && any_epochs) {
    epochs_to_save <- eyeris[epochs]
  } else if (!is.null(epochs_list)) {
    epochs_to_save <- eyeris[epochs_list]
  } else {
    epochs_to_save <- NULL
  }

  if (verbose && any_epochs) {
    cli::cli_alert_info("[INFO] Epochs to save structure:")
    cli::cli_alert_info(
      sprintf("[INFO] Names: %s", paste(names(epochs_to_save), collapse = ", "))
    )
    for (epoch_name in names(epochs_to_save)) {
      epoch_data <- epochs_to_save[[epoch_name]]
      if (is.list(epoch_data)) {
        cli::cli_alert_info(
          sprintf("[INFO] %s:", epoch_name)
        )
        for (block_name in names(epoch_data)) {
          block_data <- epoch_data[[block_name]]
          if (is.data.frame(block_data)) {
            cli::cli_alert_info(
              sprintf(
                "[INFO] %s: data.frame with %d rows",
                block_name,
                nrow(block_data)
              )
            )
          } else {
            cli::cli_alert_info(
              sprintf(
                "[INFO] %s: list with %d elements",
                block_name,
                length(block_data)
              )
            )
          }
        }
      }
    }
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

  # set run_num for blinks/events files
  run_num_for_blinks_events <- if (has_multiple_runs) "all" else run_num

  # for binocular data, create left/right subdirectories within the eye directory
  if (!is.null(eye_suffix)) {
    if (eye_suffix == "eye-L") {
      p <- file.path(p, eye_suffix)
      check_and_create_dir(dir, p, verbose = verbose)
    } else if (eye_suffix == "eye-R") {
      p <- file.path(p, eye_suffix)
      check_and_create_dir(dir, p, verbose = verbose)
    }
  } else {
    p <- file.path(p, "eye")
    check_and_create_dir(dir, p, verbose = verbose)
  }

  if (!is.null(eyeris$blinks)) {
    bids_fname <- make_bids_fname(
      sub_id = sub,
      ses_id = ses,
      task_name = task,
      run_num = run_num_for_blinks_events,
      desc = "blinks",
      eye_suffix = eye_suffix
    )

    if (verbose) {
      cli::cli_alert_info("[INFO] Writing blinks data to '%s'...", file.path(dir, p, bids_fname))
    }

    if (is_binocular_object(eyeris)) {
      if (eye_suffix == "eye-L") {
        blinks_df <- purrr::imap_dfr(eyeris$left$blinks, ~ dplyr::mutate(.x))
        write.csv(blinks_df, file.path(dir, p, bids_fname), row.names = FALSE)
      } else if (eye_suffix == "eye-R") {
        blinks_df <- purrr::imap_dfr(eyeris$right$blinks, ~ dplyr::mutate(.x))
        write.csv(blinks_df, file.path(dir, p, bids_fname), row.names = FALSE)
      }
    } else {
      blinks_df <- purrr::imap_dfr(eyeris$blinks, ~ dplyr::mutate(.x))
      write.csv(blinks_df, file.path(dir, p, bids_fname), row.names = FALSE)
    }

    if (verbose) {
      cli::cli_alert_success("[OKAY] Blinks data written to: '%s'", file.path(dir, p, bids_fname))
    }
  }

  if (!is.null(eyeris$events)) {
    bids_fname <- make_bids_fname(
      sub_id = sub,
      ses_id = ses,
      task_name = task,
      run_num = run_num_for_blinks_events,
      desc = "events",
      eye_suffix = eye_suffix
    )

    if (verbose) {
      cli::cli_alert_info("[INFO] Writing events data to '%s'...", file.path(dir, p, bids_fname))
    }

    if (is_binocular_object(eyeris)) {
      if (eye_suffix == "eye-L") {
        events_df <- purrr::imap_dfr(eyeris$left$events, ~ dplyr::mutate(.x))
        write.csv(events_df, file.path(dir, p, bids_fname), row.names = FALSE)
      } else if (eye_suffix == "eye-R") {
        events_df <- purrr::imap_dfr(eyeris$right$events, ~ dplyr::mutate(.x))
        write.csv(events_df, file.path(dir, p, bids_fname), row.names = FALSE)
      }
    } else {
      events_df <- purrr::imap_dfr(eyeris$events, ~ dplyr::mutate(.x))
      write.csv(events_df, file.path(dir, p, bids_fname), row.names = FALSE)
    }

    if (verbose) {
      cli::cli_alert_success("[OKAY] Events data written to: '%s'", file.path(dir, p, bids_fname))
    }
  }

  block_numbers <- get_block_numbers(eyeris)
  block_numbers <- block_numbers[block_numbers > 0]

  # if single block and run_num is provided, override block_numbers for output
  if (!has_multiple_runs && !is.null(run_num)) {
    block_numbers <- as.numeric(run_num)
  }

  if (!merge_epochs && any_epochs) {
    if (has_multiple_runs) {
      for (epoch_id in names(epochs_to_save)) {
        current_label <- substr(epoch_id, 7, nchar(epoch_id))

        if (verbose) {
          cli::cli_alert_info("[INFO] Processing epoch: %s (label: %s)", epoch_id, current_label)
        }

        if (merge_runs) {
          epochs_with_runs <- do.call(
            rbind,
            lapply(names(eyeris$timeseries), function(i) {
              run_epochs <- epochs_to_save[[epoch_id]][[i]]
              run_epochs$run <- sprintf("%02d", get_block_numbers(i))
              run_epochs
            })
          )

          if (verbose) {
            cli::cli_alert_info("[INFO] Merging runs for epoch: %s", epoch_id)
          }

          f <- make_bids_fname(
            sub_id = sub,
            ses_id = ses,
            task_name = task,
            run_num = run_num,
            desc = paste0("preproc_pupil_all", current_label),
            eye_suffix = eye_suffix
          )

          if (verbose) {
            cli::cli_alert_info(
              "[INFO] Writing combined runs epoched data for epoch '%s' to '%s'...",
              current_label,
              file.path(dir, p, f)
            )
          }

          write.csv(epochs_with_runs, file.path(dir, p, f), row.names = FALSE)

          if (verbose) {
            cli::cli_alert_success(
              "[OKAY] Combined runs epoched data for epoch '%s' written to: '%s'",
              current_label,
              file.path(dir, p, f)
            )
          }
        } else {
          for (i in names(eyeris$timeseries)) {
            run_epochs <- epochs_to_save[[epoch_id]][[i]]
            run_epochs$run <- sprintf("%02d", get_block_numbers(i))

            if (verbose) {
              cli::cli_alert_info("[INFO] Processing run %s for epoch %s", i, epoch_id)
            }

            if (is.null(run_epochs)) {
              if (verbose) {
                cli::cli_alert_warning("[WARN] Skipping run %s for epoch %s - no data", i, epoch_id)
              }
              next
            }

            if (!is.data.frame(run_epochs) || nrow(run_epochs) == 0) {
              if (verbose) {
                cli::cli_alert_warning("[WARN] Skipping run %s for epoch %s - empty or invalid data", i, epoch_id)
              }
              next
            }

            if (verbose) {
              cli::cli_alert_info("[INFO] Run %s for epoch %s has %d rows", i, epoch_id, nrow(run_epochs))
            }

            evs <- if (
              !is.null(find_baseline_structure(eyeris, current_label)) &&
                !is.null(
                  eyeris[[find_baseline_structure(
                    eyeris,
                    current_label
                  )]]$block_1$info$epoch_events
                )
            ) {
              epoch_events <- eyeris[[find_baseline_structure(
                eyeris,
                current_label
              )]]$block_1$info$epoch_events
              if (is.character(epoch_events)) {
                if (length(epoch_events) == 1) {
                  epoch_events
                } else {
                  paste(epoch_events, collapse = ", ")
                }
              } else {
                paste(epoch_events, collapse = ", ")
              }
            } else {
              epoch_data <- eyeris[[epoch_id]]
              if (is.list(epoch_data) && !is.null(epoch_data$info)) {
                for (block_name in names(epoch_data$info)) {
                  if (!is.null(epoch_data$info[[block_name]]$epoch_events)) {
                    epoch_events <- epoch_data$info[[block_name]]$epoch_events
                    if (is.character(epoch_events)) {
                      if (length(epoch_events) == 1) {
                        result <- epoch_events
                      } else {
                        result <- paste(epoch_events, collapse = ", ")
                      }
                    } else {
                      result <- paste(epoch_events, collapse = ", ")
                    }

                    escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                    cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
                  }
                }
              }
              NULL
            }

            c_bline <- !is.null(
              find_baseline_structure(eyeris, current_label)
            ) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  current_label
                )]]$block_1$info$baseline_events
              )
            bline_evs <- if (c_bline) {
              baseline_events <- eyeris[[find_baseline_structure(
                eyeris,
                current_label
              )]]$block_1$info$baseline_events
              if (is.character(baseline_events)) {
                if (length(baseline_events) == 1) {
                  baseline_events
                } else {
                  paste(baseline_events, collapse = ", ")
                }
              } else {
                paste(baseline_events, collapse = ", ")
              }
            } else {
              NULL
            }
            bline_type <- if (c_bline) {
              baseline_type <- eyeris[[find_baseline_structure(
                eyeris,
                current_label
              )]]$block_1$info$baseline_type
              if (is.character(baseline_type)) {
                if (length(baseline_type) == 1) {
                  baseline_type
                } else {
                  paste(baseline_type, collapse = ", ")
                }
              } else {
                paste(baseline_type, collapse = ", ")
              }
            } else {
              NULL
            }

            f <- make_bids_fname(
              sub_id = sub,
              ses_id = ses,
              task_name = task,
              run_num = run_num,
              desc = paste0("preproc_pupil_", current_label),
              eye_suffix = eye_suffix
            )

            if (verbose) {
              alert(
                "info",
                "[INFO] Writing run %02d epoched data for epoch '%s' to '%s'...",
                get_block_numbers(i),
                current_label,
                file.path(dir, p, f)
              )
            }

            write.csv(run_epochs, file.path(dir, p, f), row.names = FALSE)

            if (verbose) {
              alert(
                "success",
                "[OKAY] Run %02d epoched data for epoch '%s' written to: '%s'",
                get_block_numbers(i),
                current_label,
                file.path(dir, p, f)
              )
            }
          }
        }
      }
    } else {
      for (epoch_id in names(epochs_to_save)) {
        current_label <- substr(epoch_id, 7, nchar(epoch_id))

        if (verbose) {
          alert(
            "info",
            "[INFO] Processing single-run epoch: %s (label: %s)",
            epoch_id,
            current_label
          )
        }

        epoch_entry <- epochs_to_save[[epoch_id]]
        block_names <- setdiff(names(epoch_entry), "info")
        any_written <- FALSE
        for (block_name in block_names) {
          block_data <- epoch_entry[[block_name]]
          if (is.null(block_data) || !is.data.frame(block_data) || nrow(block_data) == 0) {
            if (verbose) {
              alert(
                "warning",
                "[WARN] Skipping block %s for epoch %s - empty or invalid data",
                block_name,
                epoch_id
              )
            }
            next
          }

          if (verbose) {
            alert(
              "info",
              "[INFO] Block %s for epoch %s has %d rows",
              block_name,
              epoch_id,
              nrow(block_data)
            )
          }

          evs <- if (
            !is.null(find_baseline_structure(eyeris, current_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  current_label
                )]]$block_1$info$epoch_events
              )
          ) {
            epoch_events <- eyeris[[find_baseline_structure(
              eyeris,
              current_label
            )]]$block_1$info$epoch_events
            if (is.character(epoch_events)) {
              if (length(epoch_events) == 1) {
                epoch_events
              } else {
                paste(epoch_events, collapse = ", ")
              }
            } else {
              paste(epoch_events, collapse = ", ")
            }
          } else {
            epoch_data <- eyeris[[epoch_id]]
            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              for (bn in names(epoch_data$info)) {
                if (!is.null(epoch_data$info[[bn]]$epoch_events)) {
                  epoch_events <- epoch_data$info[[bn]]$epoch_events
                  if (is.character(epoch_events)) {
                    if (length(epoch_events) == 1) {
                      result <- epoch_events
                    } else {
                      result <- paste(epoch_events, collapse = ", ")
                    }
                  } else {
                    result <- paste(epoch_events, collapse = ", ")
                  }

                  escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                  cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
                }
              }
            }
            NULL
          }
          c_bline <- !is.null(find_baseline_structure(eyeris, current_label)) &&
            !is.null(
              eyeris[[find_baseline_structure(
                eyeris,
                current_label
              )]]$block_1$info$baseline_events
            )
          bline_evs <- if (c_bline) {
            baseline_events <- eyeris[[find_baseline_structure(
              eyeris,
              current_label
            )]]$block_1$info$baseline_events
            if (is.character(baseline_events)) {
              if (length(baseline_events) == 1) {
                baseline_events
              } else {
                paste(baseline_events, collapse = ", ")
              }
            } else {
              paste(baseline_events, collapse = ", ")
            }
          } else {
            NULL
          }
          bline_type <- if (c_bline) {
            baseline_type <- eyeris[[find_baseline_structure(
              eyeris,
              current_label
            )]]$block_1$info$baseline_type
            if (is.character(baseline_type)) {
              if (length(baseline_type) == 1) {
                baseline_type
              } else {
                paste(baseline_type, collapse = ", ")
              }
            } else {
              paste(baseline_type, collapse = ", ")
            }
          } else {
            NULL
          }

          f <- make_bids_fname(
            sub_id = sub,
            ses_id = ses,
            task_name = task,
            run_num = sprintf("%02d", as.numeric(run_num)),
            desc = "preproc_pupil",
            epoch_name = current_label,
            epoch_events = evs,
            baseline_events = bline_evs,
            baseline_type = bline_type,
            eye_suffix = eye_suffix
          )

          if (verbose) {
            alert(
              "info",
              "[INFO] Writing epoched data for epoch '%s' (block %s) to '%s'...",
              current_label,
              block_name,
              file.path(dir, p, f)
            )
          }

          write.csv(block_data, file = file.path(bids_dir, p, f), row.names = FALSE)

          if (verbose) {
            alert(
              "success",
              "[OKAY] Epoched data for epoch '%s' (block %s) written to: '%s'",
              current_label,
              block_name,
              file.path(dir, p, f)
            )
          }
          any_written <- TRUE
        }
        if (!any_written && verbose) {
          alert("warning", "[WARN] No valid blocks found for epoch %s", epoch_id)
        }
      }
    }
  } else if (any_epochs) {
    # merge all epochs and runs (if multiple runs exist)
    if (has_multiple_runs && merge_runs) {
      merged_epochs <- do.call(
        rbind,
        lapply(names(epochs_to_save), function(epoch_id) {
          epochs_with_runs <- do.call(
            rbind,
            lapply(names(eyeris$timeseries), function(i) {
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
        sub_id = sub,
        ses_id = ses,
        task_name = task,
        run_num = run_num,
        desc = "preproc_pupil_all",
        epoch_name = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          first_label
        } else {
          NULL
        },
        epoch_events = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          if (
            !is.null(find_baseline_structure(eyeris, first_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  first_label
                )]]$block_1$info$epoch_events
              )
          ) {
            epoch_events <- eyeris[[find_baseline_structure(
              eyeris,
              first_label
            )]]$block_1$info$epoch_events
            if (is.character(epoch_events)) {
              if (length(epoch_events) == 1) {
                epoch_events
              } else {
                paste(epoch_events, collapse = ", ")
              }
            } else {
              paste(epoch_events, collapse = ", ")
            }
          } else {
            epoch_data <- eyeris[[first_epoch_id]]
            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              for (block_name in names(epoch_data$info)) {
                if (!is.null(epoch_data$info[[block_name]]$epoch_events)) {
                  epoch_events <- epoch_data$info[[block_name]]$epoch_events
                  if (is.character(epoch_events)) {
                    if (length(epoch_events) == 1) {
                      result <- epoch_events
                    } else {
                      result <- paste(epoch_events, collapse = ", ")
                    }
                  } else {
                    result <- paste(epoch_events, collapse = ", ")
                  }

                  escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                  cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
                }
              }
            }
            NULL
          }
        } else {
          NULL
        },
        baseline_events = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          if (
            !is.null(find_baseline_structure(eyeris, first_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  first_label
                )]]$block_1$info$baseline_events
              )
          ) {
            baseline_events <- eyeris[[find_baseline_structure(
              eyeris,
              first_label
            )]]$block_1$info$baseline_events
            if (is.character(baseline_events)) {
              if (length(baseline_events) == 1) {
                result <- baseline_events
              } else {
                result <- paste(baseline_events, collapse = ", ")
              }
            } else {
              result <- paste(baseline_events, collapse = ", ")
            }
            cli::cli_alert_info("[INFO] Found baseline events: ", result)
            return(result)
          }
        } else {
          NULL
        },
        baseline_type = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          if (
            !is.null(find_baseline_structure(eyeris, first_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  first_label
                )]]$block_1$info$baseline_type
              )
          ) {
            baseline_type <- eyeris[[find_baseline_structure(
              eyeris,
              first_label
            )]]$block_1$info$baseline_type
            if (is.character(baseline_type)) {
              if (length(baseline_type) == 1) {
                result <- baseline_type
              } else {
                result <- paste(baseline_type, collapse = ", ")
              }
            } else {
              result <- paste(baseline_type, collapse = ", ")
            }
            cli::cli_alert_info("[INFO] Found baseline type: ", result)
            return(result)
          } else {
            NULL
          }
        } else {
          NULL
        },
        eye_suffix = NULL
      )
    } else {
      merged_epochs <- do.call(
        rbind,
        lapply(names(epochs_to_save), function(epoch_id) {
          epochs <- epochs_to_save[[epoch_id]]
          epochs$epoch_type <- epoch_id
          epochs
        })
      )

      f <- make_bids_fname(
        sub_id = sub,
        ses_id = ses,
        task_name = task,
        run_num = sprintf("%02d", as.numeric(run_num)),
        desc = "preproc_pupil",
        epoch_name = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          first_label
        } else {
          NULL
        },
        epoch_events = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          if (
            !is.null(find_baseline_structure(eyeris, first_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  first_label
                )]]$block_1$info$epoch_events
              )
          ) {
            epoch_events <- eyeris[[find_baseline_structure(
              eyeris,
              first_label
            )]]$block_1$info$epoch_events
            if (is.character(epoch_events)) {
              if (length(epoch_events) == 1) {
                epoch_events
              } else {
                paste(epoch_events, collapse = ", ")
              }
            } else {
              paste(epoch_events, collapse = ", ")
            }
          } else {
            epoch_data <- eyeris[[first_epoch_id]]
            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              for (block_name in names(epoch_data$info)) {
                if (!is.null(epoch_data$info[[block_name]]$epoch_events)) {
                  epoch_events <- epoch_data$info[[block_name]]$epoch_events
                  if (is.character(epoch_events)) {
                    if (length(epoch_events) == 1) {
                      result <- epoch_events
                    } else {
                      result <- paste(epoch_events, collapse = ", ")
                    }
                  } else {
                    result <- paste(epoch_events, collapse = ", ")
                  }

                  escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                  cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
                }
              }
            }
            NULL
          }
        } else {
          NULL
        },
        baseline_events = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          if (
            !is.null(find_baseline_structure(eyeris, first_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  first_label
                )]]$block_1$info$baseline_events
              )
          ) {
            baseline_events <- eyeris[[find_baseline_structure(
              eyeris,
              first_label
            )]]$block_1$info$baseline_events
            if (is.character(baseline_events)) {
              if (length(baseline_events) == 1) {
                result <- baseline_events
              } else {
                result <- paste(baseline_events, collapse = ", ")
              }
            } else {
              result <- paste(baseline_events, collapse = ", ")
            }
            cli::cli_alert_info("[INFO] Found baseline events: ", result)
            return(result)
          }
        } else {
          NULL
        },
        baseline_type = if (length(epochs_to_save) > 0) {
          first_epoch_id <- names(epochs_to_save)[1]
          first_label <- substr(first_epoch_id, 7, nchar(first_epoch_id))
          if (
            !is.null(find_baseline_structure(eyeris, first_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  first_label
                )]]$block_1$info$baseline_type
              )
          ) {
            baseline_type <- eyeris[[find_baseline_structure(
              eyeris,
              first_label
            )]]$block_1$info$baseline_type
            if (is.character(baseline_type)) {
              if (length(baseline_type) == 1) {
                result <- baseline_type
              } else {
                result <- paste(baseline_type, collapse = ", ")
              }
            } else {
              result <- paste(baseline_type, collapse = ", ")
            }
            cli::cli_alert_info("[INFO] Found baseline type: ", result)
            return(result)
          } else {
            NULL
          }
        } else {
          NULL
        },
        eye_suffix = NULL
      )
    }

    if (verbose) {
      alert("info", "[INFO] Writing merged epochs to '%s'...", file.path(dir, p, f))
    }

    write.csv(merged_epochs, file = file.path(bids_dir, p, f), row.names = FALSE)

    if (verbose) {
      alert(
        "success",
        "[OKAY] Merged epochs written to: '%s'",
        file.path(dir, p, f)
      )
    }
  }

  if (save_raw && is.null(eye_suffix)) {
    if (has_multiple_runs) {
      if (merge_runs) {
        # save all runs together
        combined_timeseries <- do.call(
          rbind,
          lapply(seq_len(num_runs), function(i) {
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
          sub_id = sub,
          ses_id = ses,
          task_name = task,
          run_num = run_num,
          desc = "timeseries_all",
          eye_suffix = eye_suffix
        )

        if (verbose) {
          alert(
            "info",
            "[INFO] Writing combined raw pupil timeseries to '%s'...",
            file.path(dir, p, f)
          )
        }

        write.csv(combined_timeseries, file.path(dir, p, f), row.names = FALSE)

        if (verbose) {
          alert(
            "success",
            "[OKAY] Combined raw pupil timeseries written to: '%s'",
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
            sub_id = sub,
            ses_id = ses,
            task_name = task,
            run_num = sprintf("%02d", i),
            desc = "timeseries",
            eye_suffix = eye_suffix
          )

          if (verbose) {
            alert(
              "info",
              "[INFO] Writing run %02d raw pupil timeseries to '%s'...",
              i,
              file.path(dir, p, f)
            )
          }

          write.csv(run_data, file.path(dir, p, f), row.names = FALSE)

          if (verbose) {
            alert(
              "success",
              "[OKAY] Run %02d raw pupil timeseries written to: '%s'",
              i,
              file.path(dir, p, f)
            )
          }
        })
      }
    }
  }

  # for binocular data, save timeseries in the appropriate left/right subdirectory
  if (save_raw && !is.null(eye_suffix)) {
    if (has_multiple_runs) {
      if (merge_runs) {
        # save all runs together
        combined_timeseries <- do.call(
          rbind,
          lapply(seq_len(num_runs), function(i) {
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
          sub_id = sub,
          ses_id = ses,
          task_name = task,
          run_num = run_num,
          desc = "timeseries_all",
          eye_suffix = eye_suffix
        )

        if (verbose) {
          alert(
            "info",
            "[INFO] Writing combined raw pupil timeseries to '%s'...",
            file.path(dir, p, f)
          )
        }

        write.csv(combined_timeseries, file.path(dir, p, f), row.names = FALSE)

        if (verbose) {
          alert(
            "success",
            "[OKAY] Combined raw pupil timeseries written to: '%s'",
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
            sub_id = sub,
            ses_id = ses,
            task_name = task,
            run_num = sprintf("%02d", i),
            desc = "timeseries",
            eye_suffix = eye_suffix
          )

          if (verbose) {
            alert(
              "info",
              "[INFO] Writing run %02d raw pupil timeseries to '%s'...",
              i,
              file.path(dir, p, f)
            )
          }

          write.csv(run_data, file.path(dir, p, f), row.names = FALSE)

          if (verbose) {
            alert(
              "success",
              "[OKAY] Run %02d raw pupil timeseries written to: '%s'",
              i,
              file.path(dir, p, f)
            )
          }
        })
      }
    }
  }

  # first export confounds for unepoched timeseries
  if (!is.null(eyeris$confounds$unepoched_timeseries)) {
    if (length(block_numbers) == 1) {
      # case: single block
      export_confounds_to_csv(
        confounds_list = eyeris$confounds$unepoched_timeseries,
        output_dir = file.path(dir, p),
        filename_prefix = function(i) {
          paste0(
            "sub-",
            sub,
            if (!is.null(ses)) paste0("_ses-", ses) else "",
            "_task-",
            task,
            if (!merge_runs) sprintf("_run-%02d", as.numeric(block_numbers)) else "",
            "_desc-confounds",
            if (!is.null(eye_suffix)) paste0("_", eye_suffix) else ""
          )
        },
        verbose = verbose,
        run_num = if (!merge_runs) as.numeric(block_numbers) else run_num
      )
    } else {
      # case: multiple blocks - export each block's confounds separately
      for (block in block_numbers) {
        block_name <- paste0("block_", block)
        if (block_name %in% names(eyeris$confounds$unepoched_timeseries)) {
          single_block_confounds <- list()
          single_block_confounds[[block_name]] <-
            eyeris$confounds$unepoched_timeseries[[block_name]]

          export_confounds_to_csv(
            confounds_list = single_block_confounds,
            output_dir = file.path(dir, p),
            filename_prefix = function(i) {
              paste0(
                "sub-",
                sub,
                if (!is.null(ses)) paste0("_ses-", ses) else "",
                "_task-",
                task,
                if (!merge_runs) sprintf("_run-%02d", as.numeric(block)) else "",
                "_desc-confounds",
                if (!is.null(eye_suffix)) paste0("_", eye_suffix) else ""
              )
            },
            verbose = verbose,
            run_num = if (!merge_runs) as.numeric(block) else run_num
          )
        }
      }
    }
  }

  if (
    !is.null(eyeris$confounds$epoched_timeseries) ||
      !is.null(eyeris$confounds$epoched_epoch_wide) && any_epochs
  ) {
    # create summary files for each block
    for (block in block_numbers) {
      epoch_names <- names(eyeris)[grep("^epoch_", names(eyeris))]

      epoch_summary <- data.frame(
        epoch_type = names(eyeris)[grep("^epoch_", names(eyeris))],
        epoch_events = sapply(names(eyeris)[grep("^epoch_", names(eyeris))], function(epoch_name) {
          epoch_label <- sub("^epoch_", "", epoch_name)
          baseline_structure <- find_baseline_structure(eyeris, epoch_label)
          cli::cli_alert_info(
            paste0(
              "[INFO] Processing epoch: ",
              epoch_name,
              " -> label: ",
              epoch_label,
              " -> baseline: ",
              baseline_structure
            )
          )

          if (
            !is.null(baseline_structure) &&
              !is.null(
                eyeris[[baseline_structure]][[
                  paste0("block_", block)
                ]]$info$epoch_events
              )
          ) {
            epoch_events <- eyeris[[baseline_structure]][[
              paste0("block_", block)
            ]]$info$epoch_events
            if (is.character(epoch_events)) {
              if (length(epoch_events) == 1) {
                result <- epoch_events
              } else {
                result <- paste(epoch_events, collapse = ", ")
              }
            } else {
              result <- paste(epoch_events, collapse = ", ")
            }
            escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
            cli::cli_alert_info(paste0("[INFO] Found epoch events in baseline structure: ", escaped_result))
            return(result)
          } else {
            epoch_data <- eyeris[[epoch_name]]

            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              block_name <- paste0("block_", block)

              if (block_name %in% names(epoch_data$info) && !is.null(epoch_data$info[[block_name]]$epoch_events)) {
                epoch_events <- epoch_data$info[[block_name]]$epoch_events
                if (is.character(epoch_events)) {
                  if (length(epoch_events) == 1) {
                    result <- epoch_events
                  } else {
                    result <- paste(epoch_events, collapse = ", ")
                  }
                } else {
                  result <- paste(epoch_events, collapse = ", ")
                }
                escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
                return(result)
              }
            }
            cli::cli_alert_info(paste0("[INFO] No epoch events found for: ", epoch_name))
            NA_character_
          }
        }),
        epoch_limits = sapply(names(eyeris)[grep("^epoch_", names(eyeris))], function(epoch_name) {
          epoch_label <- sub("^epoch_", "", epoch_name)
          baseline_structure <- find_baseline_structure(eyeris, epoch_label)

          if (
            !is.null(baseline_structure) &&
              !is.null(
                eyeris[[baseline_structure]][[
                  paste0("block_", block)
                ]]$info$epoch_limits
              )
          ) {
            paste(
              eyeris[[baseline_structure]][[
                paste0("block_", block)
              ]]$info$epoch_limits,
              collapse = ", "
            )
          } else {
            epoch_data <- eyeris[[epoch_name]]
            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              block_name <- paste0("block_", block)
              if (block_name %in% names(epoch_data$info) && !is.null(epoch_data$info[[block_name]]$epoch_limits)) {
                paste(epoch_data$info[[block_name]]$epoch_limits, collapse = ", ")
              } else {
                NA_character_
              }
            } else {
              NA_character_
            }
          }
        }),
        n_epochs = sapply(names(eyeris)[grep("^epoch_", names(eyeris))], function(epoch_name) {
          epoch_label <- sub("^epoch_", "", epoch_name)
          baseline_structure <- find_baseline_structure(eyeris, epoch_label)

          if (
            !is.null(baseline_structure) &&
              !is.null(
                eyeris[[baseline_structure]][[
                  paste0("block_", block)
                ]]$info$n_epochs
              )
          ) {
            eyeris[[baseline_structure]][[
              paste0("block_", block)
            ]]$info$n_epochs
          } else {
            epoch_data <- eyeris[[epoch_name]]
            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              block_name <- paste0("block_", block)
              if (block_name %in% names(epoch_data$info) && !is.null(epoch_data$info[[block_name]]$n_epochs)) {
                return(epoch_data$info[[block_name]]$n_epochs)
              }
            }
            # fallback: count epochs from the data itself for this block
            epoch_data <- eyeris[[epoch_name]]
            if (is.list(epoch_data)) {
              block_name <- paste0("block_", block)
              if (
                block_name %in%
                  names(epoch_data) &&
                  is.data.frame(epoch_data[[block_name]]) &&
                  "matched_event" %in% colnames(epoch_data[[block_name]])
              ) {
                length(unique(epoch_data[[block_name]]$matched_event))
              } else {
                0
              }
            } else if (
              is.data.frame(epoch_data) &&
                "matched_event" %in% colnames(epoch_data)
            ) {
              length(unique(epoch_data$matched_event))
            } else {
              NA_integer_
            }
          }
        }),
        baseline_events = sapply(names(eyeris)[grep("^epoch_", names(eyeris))], function(epoch_name) {
          epoch_label <- sub("^epoch_", "", epoch_name)
          baseline_structure <- find_baseline_structure(eyeris, epoch_label)

          if (
            !is.null(baseline_structure) &&
              !is.null(
                eyeris[[baseline_structure]][[
                  paste0("block_", block)
                ]]$info$baseline_events
              )
          ) {
            baseline_events <- eyeris[[baseline_structure]][[
              paste0("block_", block)
            ]]$info$baseline_events
            if (is.character(baseline_events)) {
              if (length(baseline_events) == 1) {
                result <- baseline_events
              } else {
                result <- paste(baseline_events, collapse = ", ")
              }
            } else {
              result <- paste(baseline_events, collapse = ", ")
            }
            cli::cli_alert_info(paste0("[INFO] Found baseline events: ", result))
            return(result)
          } else {
            cli::cli_alert_warning(paste0("[WARN] No baseline events found for: ", epoch_name))
            NA_character_
          }
        }),
        baseline_period = sapply(names(eyeris)[grep("^epoch_", names(eyeris))], function(epoch_name) {
          epoch_label <- sub("^epoch_", "", epoch_name)
          baseline_structure <- find_baseline_structure(eyeris, epoch_label)

          if (
            !is.null(baseline_structure) &&
              !is.null(
                eyeris[[baseline_structure]][[
                  paste0("block_", block)
                ]]$info$baseline_period
              )
          ) {
            paste(
              eyeris[[baseline_structure]][[
                paste0("block_", block)
              ]]$info$baseline_period,
              collapse = ", "
            )
          } else {
            NA_character_
          }
        }),
        n_baseline_epochs = sapply(
          names(eyeris)[
            grep("^epoch_", names(eyeris))
          ],
          function(epoch_name) {
            epoch_label <- sub("^epoch_", "", epoch_name)
            baseline_structure <- find_baseline_structure(eyeris, epoch_label)

            if (
              !is.null(baseline_structure) &&
                !is.null(
                  eyeris[[baseline_structure]][[
                    paste0("block_", block)
                  ]]$info$n_baseline_epochs
                )
            ) {
              eyeris[[baseline_structure]][[
                paste0("block_", block)
              ]]$info$n_baseline_epochs
            } else {
              # if no baseline structure, there are no baseline epochs
              NA_integer_
            }
          }
        )
      )

      summary_filename <- make_bids_fname(
        sub_id = sub,
        ses_id = ses,
        task_name = task,
        run_num = if (!merge_runs) sprintf("%02d", as.numeric(block)) else run_num,
        desc = "epoch_summary",
        eye_suffix = eye_suffix
      )
      summary_filepath <- file.path(dir, p, summary_filename)

      if (verbose) {
        alert("info", "[INFO] Writing epoch summary for block %d to '%s'...", block, summary_filepath)
      }

      write.csv(epoch_summary, summary_filepath, row.names = FALSE)

      if (verbose) {
        alert("success", "[OKAY] Epoch summary for block %d written to: '%s'", block, summary_filepath)
      }
    }

    # export epoch-wide confounds
    if (!is.null(eyeris$confounds$epoched_epoch_wide) && any_epochs) {
      for (epoch_name in names(eyeris$confounds$epoched_epoch_wide)) {
        epoch_label <- sub("^epoch_", "", epoch_name)

        epoch_folder <- file.path(dir, p, paste0("epoch_", epoch_label))
        if (!dir.exists(epoch_folder)) {
          dir.create(epoch_folder, recursive = TRUE)
        }

        epoch_events_info <- if (
          !is.null(find_baseline_structure(
            eyeris,
            epoch_label
          )) &&
            !is.null(
              eyeris[[find_baseline_structure(
                eyeris,
                epoch_label
              )]]$block_1$info$epoch_events
            )
        ) {
          epoch_events <- eyeris[[find_baseline_structure(
            eyeris,
            epoch_label
          )]]$block_1$info$epoch_events
          if (is.character(epoch_events)) {
            if (length(epoch_events) == 1) {
              epoch_events
            } else {
              paste(epoch_events, collapse = ", ")
            }
          } else {
            paste(epoch_events, collapse = ", ")
          }
        } else {
          epoch_data <- eyeris[[epoch_name]]
          if (is.list(epoch_data) && !is.null(epoch_data$info)) {
            for (block_name in names(epoch_data$info)) {
              if (!is.null(epoch_data$info[[block_name]]$epoch_events)) {
                epoch_events <- epoch_data$info[[block_name]]$epoch_events
                if (is.character(epoch_events)) {
                  if (length(epoch_events) == 1) {
                    result <- epoch_events
                  } else {
                    result <- paste(epoch_events, collapse = ", ")
                  }
                } else {
                  result <- paste(epoch_events, collapse = ", ")
                }
                escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
              }
            }
          }
          NULL
        }
        baseline_events_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  epoch_label
                )]]$block_1$info$baseline_events
              )
          ) {
            baseline_events <- eyeris[[
              find_baseline_structure(
                eyeris,
                epoch_label
              )
            ]]$block_1$info$baseline_events
            if (is.character(baseline_events)) {
              if (length(baseline_events) == 1) {
                baseline_events
              } else {
                paste(baseline_events, collapse = ", ")
              }
            } else {
              paste(baseline_events, collapse = ", ")
            }
          } else {
            NULL
          }
        baseline_type_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  epoch_label
                )]]$block_1$info$baseline_type
              )
          ) {
            baseline_type <- eyeris[[
              find_baseline_structure(
                eyeris,
                epoch_label
              )
            ]]$block_1$info$baseline_type
            if (is.character(baseline_type)) {
              if (length(baseline_type) == 1) {
                baseline_type
              } else {
                paste(baseline_type, collapse = ", ")
              }
            } else {
              paste(baseline_type, collapse = ", ")
            }
          } else {
            NULL
          }

        for (block_name in names(
          eyeris$confounds$epoched_epoch_wide[[epoch_name]]
        )) {
          block_confounds <-
            eyeris$confounds$epoched_epoch_wide[[epoch_name]][[block_name]]

          if (nrow(block_confounds) == 0) {
            next
          }

          matched_events <- unique(block_confounds$matched_event)

          for (event in matched_events) {
            event_confounds <- block_confounds[
              block_confounds$matched_event == event,
            ]

            if (nrow(event_confounds) == 0) {
              next
            }

            event_unique <- if ("text_unique" %in% colnames(event_confounds)) {
              unique(event_confounds$text_unique)[1]
            } else {
              event
            }

            epoch_filename <- make_bids_fname(
              sub_id = sub,
              ses_id = ses,
              task_name = task,
              run_num = sprintf("%02d", get_block_numbers(block_name)),
              epoch_name = epoch_label,
              desc = paste0("confounds_summary_", event_unique),
              epoch_events = epoch_events_info,
              baseline_events = baseline_events_info,
              baseline_type = baseline_type_info,
              eye_suffix = eye_suffix
            )
            epoch_filepath <- file.path(epoch_folder, epoch_filename)

            if (verbose) {
              alert(
                "info",
                paste0("[INFO] Writing epoch-wide confounds for event '%s'", "(unique: '%s') to '%s'..."),
                event,
                event_unique,
                epoch_filepath
              )
            }

            write.csv(event_confounds, epoch_filepath, row.names = FALSE)

            if (verbose) {
              alert(
                "success",
                paste0("[OKAY] Epoch-wide confounds for event '%s'", "(unique: '%s') written to: '%s'"),
                event,
                event_unique,
                epoch_filepath
              )
            }
          }
        }
      }
    }

    if (!is.null(eyeris$confounds$epoched_timeseries)) {
      for (epoch_name in names(eyeris$confounds$epoched_timeseries)) {
        epoch_label <- sub("^epoch_", "", epoch_name)

        epoch_folder <- file.path(dir, p, paste0("epoch_", epoch_label))
        if (!dir.exists(epoch_folder)) {
          dir.create(epoch_folder, recursive = TRUE)
        }

        epoch_events_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  epoch_label
                )]]$block_1$info$epoch_events
              )
          ) {
            epoch_events <- eyeris[[find_baseline_structure(
              eyeris,
              epoch_label
            )]]$block_1$info$epoch_events
            if (is.character(epoch_events)) {
              if (length(epoch_events) == 1) {
                epoch_events
              } else {
                paste(epoch_events, collapse = ", ")
              }
            } else {
              paste(epoch_events, collapse = ", ")
            }
          } else {
            epoch_data <- eyeris[[epoch_name]]
            if (is.list(epoch_data) && !is.null(epoch_data$info)) {
              for (block_name in names(epoch_data$info)) {
                if (!is.null(epoch_data$info[[block_name]]$epoch_events)) {
                  epoch_events <- epoch_data$info[[block_name]]$epoch_events
                  if (is.character(epoch_events)) {
                    if (length(epoch_events) == 1) {
                      result <- epoch_events
                    } else {
                      result <- paste(epoch_events, collapse = ", ")
                    }
                  } else {
                    result <- paste(epoch_events, collapse = ", ")
                  }
                  escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
                  cli::cli_alert_info(paste0("[INFO] Found epoch events in epoch structure: ", escaped_result))
                }
              }
            }
            NULL
          }
        baseline_events_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  epoch_label
                )]]$block_1$info$baseline_events
              )
          ) {
            baseline_events <- eyeris[[find_baseline_structure(
              eyeris,
              epoch_label
            )]]$block_1$info$baseline_events
            if (is.character(baseline_events)) {
              if (length(baseline_events) == 1) {
                result <- baseline_events
              } else {
                result <- paste(baseline_events, collapse = ", ")
              }
            } else {
              result <- paste(baseline_events, collapse = ", ")
            }
          } else {
            NULL
          }
        baseline_type_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)) &&
              !is.null(
                eyeris[[find_baseline_structure(
                  eyeris,
                  epoch_label
                )]]$block_1$info$baseline_type
              )
          ) {
            baseline_type <- eyeris[[find_baseline_structure(
              eyeris,
              epoch_label
            )]]$block_1$info$baseline_type
            if (is.character(baseline_type)) {
              if (length(baseline_type) == 1) {
                result <- baseline_type
              } else {
                result <- paste(baseline_type, collapse = ", ")
              }
            } else {
              result <- paste(baseline_type, collapse = ", ")
            }
          } else {
            NULL
          }

        for (block_name in names(
          eyeris$confounds$epoched_timeseries[[epoch_name]]
        )) {
          block_confounds <- eyeris$confounds$epoched_timeseries[[
            epoch_name
          ]][[block_name]]

          if (nrow(block_confounds) == 0) {
            next
          }

          matched_events <- unique(block_confounds$matched_event)

          for (event in matched_events) {
            event_confounds <-
              block_confounds[block_confounds$matched_event == event, ]

            if (nrow(event_confounds) == 0) {
              next
            }

            event_unique <- if ("text_unique" %in% colnames(event_confounds)) {
              unique(event_confounds$text_unique)[1]
            } else {
              event
            }

            event_filename <- make_bids_fname(
              sub_id = sub,
              ses_id = ses,
              task_name = task,
              run_num = sprintf("%02d", get_block_numbers(block_name)),
              epoch_name = epoch_label,
              desc = paste0("confounds_events_", event_unique),
              epoch_events = epoch_events_info,
              baseline_events = baseline_events_info,
              baseline_type = baseline_type_info,
              eye_suffix = eye_suffix
            )
            event_filepath <- file.path(epoch_folder, event_filename)

            if (verbose) {
              alert(
                "info",
                paste0("[INFO] Writing step-specific confounds for event '%s'", "(unique: '%s') to '%s'..."),
                event,
                event_unique,
                event_filepath
              )
            }

            write.csv(event_confounds, event_filepath, row.names = FALSE)

            if (verbose) {
              alert(
                "success",
                paste0("[OKAY] Step-specific confounds for event '%s'", "(unique: '%s') written to: '%s'"),
                event,
                event_unique,
                event_filepath
              )
            }
          }
        }
      }
    }
  }

  should_render_report <- html_report

  if (should_render_report) {
    # normalize the bids_dir path
    bids_dir <- normalizePath(path.expand(bids_dir), mustWork = FALSE)

    # create full path for figures
    figs_out <- file.path(report_path, "source", "figures")

    # create directories with normalized path
    check_and_create_dir(figs_out, verbose = verbose)

    fig_paths <- c()

    # first check if there are multiple runs
    # if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    if (actual_block_count > 1) {
      has_multiple_runs <- TRUE
      num_runs <- length(eyeris$timeseries)
    } else {
      has_multiple_runs <- FALSE
      num_runs <- 1
    }

    for (i_run in block_numbers) {
      # current_data <- if (has_multiple_runs) {
      current_data <- eyeris$timeseries[[paste0("block_", i_run)]]
      # } else {
      # eyeris$timeseries
      # }

      pupil_steps <- grep("^pupil_", colnames(current_data), value = TRUE)
      run_fig_paths <- rep(NA, length(pupil_steps) * 2)

      # use run_num override for single block
      run_dir_num <- if (!has_multiple_runs && !is.null(run_num)) as.numeric(run_num) else i_run
      run_dir <- file.path(figs_out, sprintf("run-%02d", run_dir_num))
      check_and_create_dir(run_dir, verbose = verbose)

      # make step-by-step plots
      plot_types <- c("timeseries", "histogram")

      for (i in seq_along(pupil_steps)) {
        for (p in seq_along(plot_types)) {
          fig_name <- sprintf(
            "run-%02d_fig-%d_desc-%s",
            run_dir_num,
            i,
            plot_types[p]
          )
          if (!is.null(eye_suffix)) {
            fig_name <- paste0(fig_name, "_", eye_suffix)
          }
          fig_name <- paste0(fig_name, ".jpg")
          run_fig_paths[(i - 1) * 2 + p] <- file.path(run_dir, fig_name)
        }
      }

      # plot random epoch panel
      for (i in seq_along(run_fig_paths)) {
        plot_dist <- i %% 2 == 0
        jpeg(run_fig_paths[i], width = 12, height = 7, units = "in", res = 300, pointsize = 14)
        tryCatch(
          {
            plot(
              eyeris,
              steps = ceiling(i / 2),
              seed = report_seed,
              block = i_run,
              plot_distributions = plot_dist,
              add_progressive_summary = FALSE
            )
          },
          error = function(e) {
            # create empty plot with error message
            plot(
              NA,
              xlim = c(0, 1),
              ylim = c(0, 1),
              type = "n",
              xlab = "",
              ylab = "",
              main = paste("No data to plot for block", i_run)
            )
            text(0.5, 0.5, paste("Error plotting block", i_run, ":\n", e$message), cex = 0.8, col = "red")
          }
        )
        dev.off()
      }

      # make full timeseries plots for all intermediate steps
      for (i_step in seq_along(pupil_steps)) {
        for (p in seq_along(plot_types)[1]) {
          plot_dist <- p %% 2 == 0

          fig_filename <- file.path(
            run_dir,
            sprintf(
              "run-%02d_fig-full-%d_desc-%s",
              run_dir_num,
              i_step,
              plot_types[p]
            )
          )
          if (!is.null(eye_suffix)) {
            fig_filename <- paste0(fig_filename, "_", eye_suffix)
          }
          fig_filename <- paste0(fig_filename, ".jpg")

          run_fig_paths <- c(run_fig_paths, fig_filename)

          jpeg(fig_filename, width = 12, height = 7, units = "in", res = 300, pointsize = 18)

          max_time <- max(current_data$time_secs, na.rm = TRUE)
          if (!is.finite(max_time)) {
            max_time <- 1 # default if no time data
          }

          tryCatch(
            {
              plot(
                eyeris,
                steps = i_step,
                preview_window = c(0, max_time),
                block = i_run,
                plot_distributions = plot_dist
              )
            },
            error = function(e) {
              plot(
                NA,
                xlim = c(0, 1),
                ylim = c(0, 1),
                type = "n",
                xlab = "",
                ylab = "",
                main = paste("No data to plot for block", i_run)
              )
              text(0.5, 0.5, paste("Error plotting block", i_run, ":\n", e$message), cex = 0.8, col = "red")
            }
          )
          dev.off()
        }
      }

      fig_paths <- c(fig_paths, run_fig_paths)
    }

    for (i_run in block_numbers) {
      current_data <- eyeris

      # use run_num override for single block
      run_dir_num <- if (!has_multiple_runs && !is.null(run_num)) as.numeric(run_num) else i_run

      if (
        all(c("eye_x", "eye_y") %in% colnames(current_data$timeseries[[paste0("block_", i_run)]])) &&
          all(c("screen.x", "screen.y") %in% colnames(eyeris$info))
      ) {
        run_dir <- file.path(figs_out, sprintf("run-%02d", run_dir_num))
        check_and_create_dir(run_dir, verbose = verbose)

        heatmap_filename <- file.path(
          run_dir,
          sprintf("run-%02d_gaze_heatmap", i_run)
        )

        if (!is.null(eye_suffix)) {
          heatmap_filename <- paste0(heatmap_filename, "_", eye_suffix)
        }

        heatmap_filename <- paste0(heatmap_filename, ".png")

        png(heatmap_filename, width = 8, height = 6, units = "in", res = 300, pointsize = 12)

        tryCatch(
          {
            plot_gaze_heatmap(
              eyeris = current_data,
              block = i_run,
              screen_width = eyeris$info$screen.x,
              screen_height = eyeris$info$screen.y,
              n_bins = 50,
              col_palette = "viridis",
              main = sprintf("Gaze Heatmap (run-%02d)", i_run),
              eye_suffix = eye_suffix
            )
          },
          error = function(e) {
            plot(
              NA,
              xlim = c(0, 1),
              ylim = c(0, 1),
              type = "n",
              xlab = "",
              ylab = "",
              main = sprintf("Error creating gaze heatmap for run-%02d", i_run)
            )
            text(0.5, 0.5, paste("Error:", e$message), cex = 0.8, col = "red")
          }
        )

        dev.off()

        if (verbose) {
          alert("info", "Created gaze heatmap for run-%02d", i_run)
          alert("info", "[INFO] Created gaze heatmap for run-%02d", i_run)
        }
      }

      # generate binocular correlation plots if binocular data is detected
      # check for both regular binocular data and binocular objects
      # has_binocular <- isTRUE(current_data$binocular) || !is.null(current_data$raw_binocular_object)
      has_binocular <- !is.null(raw_binocular_object)

      if (has_binocular) {
        run_dir <- file.path(figs_out, sprintf("run-%02d", i_run))
        check_and_create_dir(run_dir, verbose = verbose)

        correlation_filename <- file.path(
          run_dir,
          sprintf("run-%02d_binocular_correlation", i_run)
        )

        correlation_filename <- paste0(correlation_filename, ".png")

        png(correlation_filename, width = 12, height = 4, units = "in", res = 300, pointsize = 12)

        tryCatch(
          {
            plot_binocular_correlation(
              eyeris = raw_binocular_object,
              block = i_run,
              variables = c("pupil", "x", "y"),
              main = "",
              col_palette = "viridis",
              verbose = verbose
            )
          },
          error = function(e) {
            plot(
              NA,
              xlim = c(0, 1),
              ylim = c(0, 1),
              type = "n",
              xlab = "",
              ylab = "",
              main = sprintf("Error creating binocular correlation for run-%02d", i_run)
            )
            text(0.5, 0.5, paste("Error:", e$message), cex = 0.8, col = "red")
          }
        )

        dev.off()

        if (verbose) {
          alert("info", "[INFO] Created binocular correlation plot for run-%02d", i_run)
        }
      }
    }

    # now handle epochs (if present)
    if (!is.null(report_epoch_grouping_var_col)) {
      for (i in seq_along(epochs_to_save)) {
        epoch_data <- epochs_to_save[[i]]

        if (is.null(epoch_data) || !is.list(epoch_data)) {
          if (verbose) {
            alert("warning", "[WARN] Skipping epoch %d for report generation - no valid data", i)
          }
          next
        }

        for (bn in names(epoch_data)) {
          if (
            is.null(epoch_data[[bn]]) ||
              !is.data.frame(epoch_data[[bn]]) ||
              nrow(epoch_data[[bn]]) == 0
          ) {
            if (verbose) {
              alert("warning", "[WARN] Skipping block %s for epoch %d - no valid data", bn, i)
            }
            next
          }

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

          # use run_num override for single block
          run_dir_num <- if (!has_multiple_runs && !is.null(run_num)) as.numeric(run_num) else get_block_numbers(bn)

          run_dir <- file.path(
            figs_out,
            sprintf(
              "run-%02d",
              run_dir_num
            )
          )
          check_and_create_dir(run_dir, verbose = verbose)
          epochs_out <- file.path(run_dir, names(epochs_to_save)[i])
          check_and_create_dir(epochs_out, verbose = verbose)

          epoch_groups <- as.vector(
            unique(epochs_to_save[[i]][[bn]][report_epoch_grouping_var_col])[[1]]
          )

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

              colorpal <- eyeris_color_palette()
              colors <- c("black", colorpal)

              y_label <- paste("pupil size", y_units)

              file_out <- file.path(
                epochs_out,
                sprintf(
                  "run-%02d_%s_%d",
                  run_dir_num,
                  group,
                  pstep
                )
              )

              if (!is.null(eye_suffix)) {
                file_out <- paste0(file_out, "_", eye_suffix)
              }

              file_out <- paste0(file_out, ".png")

              png(file_out, width = 3.25, height = 2.5, units = "in", res = 600, pointsize = 6)
              y_values <- group_df[[pupil_steps[pstep]]]
              if (any(is.finite(y_values))) {
                plot(
                  group_df$timebin,
                  y_values,
                  type = "l",
                  xlab = "time (s)",
                  ylab = y_label,
                  col = colors[pstep],
                  main = paste0(
                    group,
                    "\n",
                    pupil_steps[pstep],
                    sprintf(
                      " (Run %d)",
                      run_dir_num
                    )
                  )
                )
              } else {
                plot(
                  NA,
                  xlim = range(group_df$timebin, na.rm = TRUE),
                  ylim = c(0, 1),
                  type = "n",
                  xlab = "time (s)",
                  ylab = y_label,
                  main = paste0(
                    group,
                    "\n",
                    pupil_steps[pstep],
                    "\nNO DATA"
                  )
                )
                cli::cli_alert_warning(
                  paste(
                    "[WARN] eyeris: no finite pupillometry data to plot for
                        current epoch...",
                    "plotting empty epoch plot."
                  )
                )
                text(0.5, 0.5, "No valid data", cex = 0.8, col = "red")
              }
              dev.off()
            }
          }

          for (group in epoch_groups) {
            group_df <- epochs_to_save[[i]][[bn]]
            group_df <- group_df[
              group_df[[report_epoch_grouping_var_col]] == group,
            ]

            if (
              all(c("eye_x", "eye_y") %in% colnames(group_df)) &&
                all(c("screen.x", "screen.y") %in% colnames(eyeris$info))
            ) {
              heatmap_filename <- file.path(
                epochs_out,
                sprintf(
                  "run-%02d_%s_gaze_heatmap",
                  run_dir_num,
                  group
                )
              )

              if (!is.null(eye_suffix)) {
                heatmap_filename <- paste0(heatmap_filename, "_", eye_suffix)
              }

              heatmap_filename <- paste0(heatmap_filename, ".png")

              png(heatmap_filename, width = 6, height = 4, units = "in", res = 300, pointsize = 10)

              tryCatch(
                {
                  plot_gaze_heatmap(
                    eyeris = group_df,
                    block = run_dir_num,
                    screen_width = eyeris$info$screen.x,
                    screen_height = eyeris$info$screen.y,
                    n_bins = 30,
                    col_palette = "viridis",
                    main = sprintf("%s\nGaze Heatmap (run-%02d)", group, run_dir_num),
                    eye_suffix = eye_suffix
                  )
                },
                error = function(e) {
                  plot(
                    NA,
                    xlim = c(0, 1),
                    ylim = c(0, 1),
                    type = "n",
                    xlab = "",
                    ylab = "",
                    main = paste("Error creating gaze heatmap for epoch", group)
                  )
                  text(0.5, 0.5, paste("Error:", e$message), cex = 0.8, col = "red")
                }
              )

              dev.off()

              if (verbose) {
                alert("info", "[INFO] Created gaze heatmap for epoch %s (run-%02d)", group, run_dir_num)
              }
            }
          }

          if (any_epochs) {
            epochs <- list.files(epochs_out, full.names = FALSE, pattern = "\\.(jpg|jpeg|png|gif)$", ignore.case = TRUE)

            epochs <- file.path(
              "source",
              "figures",
              sprintf("run-%02d", run_dir_num),
              names(epochs_to_save)[i],
              epochs
            )

            if (!is.null(eye_suffix)) {
              epochs <- epochs[grepl(eye_suffix, epochs)]
            }

            make_gallery(
              eyeris,
              epochs,
              report_path,
              sprintf(
                "%s%s",
                names(epochs_to_save)[i],
                sprintf("_run-%02d", run_dir_num)
              ),
              sub = sub,
              ses = ses,
              task = task,
              run = sprintf("%02d", run_dir_num),
              eye_suffix = eye_suffix
            )
          }
        }
      }
    }
    # generate report
    report_output <- make_report(
      eyeris,
      report_path,
      fig_paths,
      eye_suffix = eye_suffix,
      sub = sub,
      ses = ses,
      task = task
    )

    render_report(report_output)
  }
}

#' Make a BIDS-compatible filename
#'
#' Helper function to generate a BIDS-compatible filename based on the provided
#' parameters.
#'
#' @param sub_id The subject ID
#' @param task_name The task name
#' @param run_num The run number
#' @param desc The description
#' @param ses_id The session ID
#' @param epoch_name The epoch name
#' @param epoch_events The epoch events
#' @param baseline_events The baseline events
#' @param baseline_type The baseline type
#' @param eye_suffix The eye suffix
#'
#' @return A BIDS-compatible filename
#'
#' @keywords internal
make_bids_fname <- function(
  sub_id,
  task_name,
  run_num,
  desc = "",
  ses_id = NULL,
  epoch_name = NULL,
  epoch_events = NULL,
  baseline_events = NULL,
  baseline_type = NULL,
  eye_suffix = NULL
) {
  desc_parts <- c(desc)

  if (!is.null(epoch_events)) {
    epoch_event_name <- if (
      is.character(epoch_events) &&
        length(epoch_events) == 1
    ) {
      gsub("[*{}]", "", epoch_events)
    } else {
      "multi_events"
    }
    desc_parts <- c(desc_parts, paste0("epoch-", sanitize_event_tag(epoch_event_name, "")))
  } else if (!is.null(epoch_name)) {
    # fallback: use epoch_name with "epoch_" prefix removed
    epoch_name_clean <- sub("^epoch_", "", epoch_name)
    desc_parts <- c(desc_parts, paste0("epoch-", sanitize_event_tag(epoch_name_clean, "")))
  }

  if (!is.null(baseline_events)) {
    baseline_event_name <-
      if (is.character(baseline_events) && length(baseline_events) == 1) {
        gsub("[*{}]", "", baseline_events)
      } else {
        "multi_baseline"
      }

    bline_string <- "bline"
    if (!is.null(baseline_type)) {
      bline_string <- paste0(bline_string, "-", baseline_type)
    }
    bline_string <- paste0(bline_string, "-", sanitize_event_tag(baseline_event_name, ""))
    desc_parts <- c(desc_parts, bline_string)
  }

  final_desc <- paste(desc_parts, collapse = "_")

  # add eye suffix if provided
  if (!is.null(eye_suffix)) {
    final_desc <- paste0(final_desc, "_", eye_suffix)
  }

  f <- paste0(
    "sub-",
    sub_id,
    if (!is.null(ses_id)) paste0("_ses-", ses_id) else "",
    "_task-",
    task_name,
    if (!is.null(run_num)) paste0("_run-", run_num) else "",
    "_desc-",
    final_desc,
    ".csv"
  )

  return(gsub("__", "_", f)) # replace double underscores
}

#' Find baseline structure name for a given epoch
#'
#' Helper function to find the correct baseline structure name that matches
#' the complex baseline naming scheme used by eyeris.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param epoch_label The epoch label (without "epoch_" prefix)
#'
#' @return The baseline structure name or `NULL` if not found
#'
#' @keywords internal
find_baseline_structure <- function(eyeris, epoch_label) {
  baseline_names <- names(eyeris)[grep("^baseline_", names(eyeris))]

  if (length(baseline_names) > 0) {
    cli::cli_alert_info("[INFO] Available baseline structures: ", paste(baseline_names, collapse = ", "))
    cli::cli_alert_info("[INFO] Looking for epoch label: ", epoch_label)
  }

  for (baseline_name in baseline_names) {
    if (grepl(paste0("_epoch_", epoch_label, "$"), baseline_name)) {
      cli::cli_alert_info("[INFO] Found matching baseline structure: ", baseline_name)
      return(baseline_name)
    }
  }

  simple_name <- paste0("baseline_", epoch_label)
  if (simple_name %in% names(eyeris)) {
    cli::cli_alert_info("[INFO] Found simple baseline structure: ", simple_name)
    return(simple_name)
  }

  cli::cli_alert_warning("[WARN] No baseline structure found for epoch label: ", epoch_label)
  NULL
}
