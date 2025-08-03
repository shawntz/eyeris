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
#' @param csv_enabled Logical flag indicating whether to write CSV output files.
#' Defaults to `TRUE`. Set to `FALSE` to disable CSV file generation, useful
#' for large-scale cloud compute environments when using database storage only
#' @param db_enabled Logical flag indicating whether to write data to a DuckDB
#' database. Defaults to `FALSE`. When `TRUE`, creates or connects to a database
#' for centralized data storage and querying
#' @param db_path Database filename or path. Defaults to `"eyeris-proj.eyerisdb"`.
#' If just a filename, the database will be created in the `derivatives/`
#' directory. If a full path is provided, it will be used as specified
#' @param merge_epochs **(Deprecated)** This parameter is deprecated and will be
#' ignored. All epochs are now saved as separate files following BIDS conventions.
#' This parameter will be removed in a future version
#' @param merge_runs **(Deprecated)** This parameter is deprecated and will be
#' ignored. All runs are now saved as separate files following BIDS conventions.
#' This parameter will be removed in a future version
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
#'
#' # example with database storage enabled
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
#'     db_enabled = TRUE,  # enable eyerisdb database storage
#'     db_path = "my-project"  # custom project database name
#'   )
#'
#' # example for large-scale cloud compute (database only, no CSV files)
#' demo_data |>
#'   eyeris::glassbox() |>
#'   eyeris::bidsify(
#'     bids_dir = tempdir(),
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     csv_enabled = FALSE,  # disable CSV files
#'     db_enabled = TRUE     # database storage only
#'   )
#' }
#'
#' @export
bidsify <- function(
  eyeris,
  save_all = TRUE,
  epochs_list = NULL,
  bids_dir = NULL,
  participant_id = NULL,
  session_num = NULL,
  task_name = NULL,
  run_num = NULL,
  save_raw = TRUE,
  html_report = TRUE,
  report_seed = 0,
  report_epoch_grouping_var_col = "matched_event",
  verbose = TRUE,
  csv_enabled = TRUE,
  db_enabled = FALSE,
  db_path = "my-project",
  merge_epochs = deprecated(),
  merge_runs = deprecated(),
  pdf_report = deprecated()
) {
  # deprecation warnings -----------------------------------------------
  if (is_present(pdf_report)) {
    deprecate_warn(
      "1.3.0",
      "bidsify(pdf_report)",
      "bidsify(html_report)"
    )
    html_report <- pdf_report
  }

  if (is_present(merge_runs)) {
    deprecate_warn(
      "2.2.0",
      "bidsify(merge_runs)",
      details = "All runs are now saved as separate files following BIDS conventions."
    )
  }

  if (is_present(merge_epochs)) {
    deprecate_warn(
      "2.2.0",
      "bidsify(merge_epochs)",
      details = "All epochs are now saved as separate files following BIDS conventions."
    )
  }

  # setup --------------------------------------------------------------
  if (is_binocular_object(eyeris)) {
    left_eyeris <- eyeris$left
    right_eyeris <- eyeris$right

    run_bidsify(
      left_eyeris,
      save_all = save_all,
      epochs_list = epochs_list,
      bids_dir = bids_dir,
      participant_id = participant_id,
      session_num = session_num,
      task_name = task_name,
      run_num = run_num,
      save_raw = save_raw,
      html_report = html_report,
      report_seed = report_seed,
      report_epoch_grouping_var_col = report_epoch_grouping_var_col,
      eye_suffix = "eye-L",
      verbose = verbose,
      csv_enabled = csv_enabled,
      db_enabled = db_enabled,
      db_path = db_path,
      raw_binocular_object = eyeris$raw_binocular_object,
      skip_db_cleanup = FALSE
    )

    run_bidsify(
      right_eyeris,
      save_all = save_all,
      epochs_list = epochs_list,
      bids_dir = bids_dir,
      participant_id = participant_id,
      session_num = session_num,
      task_name = task_name,
      run_num = run_num,
      save_raw = save_raw,
      html_report = html_report,
      report_seed = report_seed,
      report_epoch_grouping_var_col = report_epoch_grouping_var_col,
      eye_suffix = "eye-R",
      verbose = verbose,
      csv_enabled = csv_enabled,
      db_enabled = db_enabled,
      db_path = db_path,
      raw_binocular_object = eyeris$raw_binocular_object,
      skip_db_cleanup = TRUE
    )
  } else {
    run_bidsify(
      eyeris,
      save_all = save_all,
      epochs_list = epochs_list,
      bids_dir = bids_dir,
      participant_id = participant_id,
      session_num = session_num,
      task_name = task_name,
      run_num = run_num,
      save_raw = save_raw,
      html_report = html_report,
      report_seed = report_seed,
      report_epoch_grouping_var_col = report_epoch_grouping_var_col,
      verbose = verbose,
      csv_enabled = csv_enabled,
      db_enabled = db_enabled,
      db_path = db_path,
      raw_binocular_object = eyeris$raw_binocular_object
    )
  }
}

#' Internal function to run bidsify on a single eye
#' @param eyeris An eyeris object
#' @param save_all Whether to save all data
#' @param epochs_list A list of epochs to include
#' @param bids_dir The directory to save the bids data
#' @param participant_id The participant id
#' @param session_num The session number
#' @param task_name The task name
#' @param run_num The run number
#' @param save_raw Whether to save raw data
#' @param html_report Whether to generate an html report
#' @param report_seed The seed for the report
#' @param report_epoch_grouping_var_col The column to use for grouping epochs in the report
#' @param eye_suffix The suffix to add to the eye data
#' @param verbose Whether to print verbose output
#' @param csv_enabled Whether to save csv files
#' @param db_enabled Whether to save data to the database
#' @param db_path The path to the database
#' @param raw_binocular_object The raw binocular object
#' @param skip_db_cleanup Whether to skip database cleanup, used internally to avoid unintended overwriting when calling complementary binocular bidsify processing commands
#'
#' @return A eyeris object
#'
#' @keywords internal
run_bidsify <- function(
  eyeris,
  save_all = TRUE,
  epochs_list = NULL,
  bids_dir = NULL,
  participant_id = NULL,
  session_num = NULL,
  task_name = NULL,
  run_num = NULL,
  save_raw = TRUE,
  html_report = TRUE,
  report_seed = 0,
  report_epoch_grouping_var_col = "matched_event",
  eye_suffix = NULL,
  verbose = TRUE,
  csv_enabled = TRUE,
  db_enabled = FALSE,
  db_path = "my-project",
  raw_binocular_object = NULL,
  skip_db_cleanup = FALSE
) {
  start_time <- Sys.time()

  if (is.null(eye_suffix)) {
    eye_log_string <- "(monocular)"
  } else {
    eye_log_string <- paste0("(binocular: ", eye_suffix, ")")
  }

  if (verbose) {
    cli::cli_alert_info(
      glue::glue(
        "[INFO] Starting BIDSify for sub-{participant_id} {eye_log_string} at {format(start_time, '%Y-%m-%d %H:%M:%S')}"
      )
    )
  }

  actual_block_count <- length(eyeris$timeseries)

  if (actual_block_count > 1) {
    # case: multiple blocks: show warning if run_num was provided
    if (!is.null(run_num)) {
      cli::cli_alert_warning(
        paste0(
          "[WARN] `run_num` is ignored when data contains multiple blocks.",
          "Blocks will be automatically numbered as runs (block 1 = run-01,",
          "block 2 = run-02, etc.) in the order they appeared/were recorded."
        ),
        wrap = TRUE
      )
    }
    has_multiple_runs <- TRUE
    num_runs <- actual_block_count
  } else {
    # case: single block in list format, here allow run_num override
    has_multiple_runs <- FALSE
    num_runs <- 1

    if (verbose) {
      cli::cli_alert_info("[INFO] Only 1 block detected...", wrap = TRUE)
    }

    original_block_name <- names(eyeris$timeseries)[1]
    original_block_number <- substr(original_block_name, 7, nchar(original_block_name))
    if (is.null(run_num)) {
      run_num_stripped <- as.character(as.integer(original_block_number))
      run_num <- as.character(as.integer(original_block_number))
    } else {
      run_num_stripped <- as.character(as.integer(run_num))
    }
    new_block_name <- paste0("block_", run_num_stripped)

    if (!is.null(run_num)) {
      cli::cli_alert_info(
        sprintf("[INFO] Using run_num = %s for single block data", run_num),
        wrap = TRUE
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
    if (length(epoch_names) > 0) {
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

  if (verbose && any_epochs) {
    cli::cli_alert_info(
      sprintf("[INFO] Filtered epochs: %s", paste(epochs, collapse = ", ")),
      wrap = TRUE
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
    cli::cli_alert_info(
      sprintf("[INFO] Epoch names to save: %s", paste(names(epochs_to_save), collapse = ", ")),
      wrap = TRUE
    )
  }

  # database connection setup with csv fallback
  db_con <- NULL
  if (db_enabled) {
    db_con <- connect_eyeris_database(
      bids_dir = dir,
      db_path = db_path,
      verbose = verbose
    )

    if (is.null(db_con)) {
      cli::cli_alert_warning("[WARN] Database connection failed", wrap = TRUE)
      db_enabled <- FALSE
    }
  }

  # fallback: enable csv if DB fails and csv was disabled
  if (!csv_enabled && !db_enabled) {
    cli::cli_alert_warning(
      "[WARN] Database failed and CSV disabled - enabling CSV as fallback to prevent data loss",
      wrap = TRUE
    )
    csv_enabled <- TRUE
  }

  # comprehensive subject cleanup: remove all existing data for this subject
  if (!is.null(db_con) && !skip_db_cleanup) {
    tryCatch(
      {
        # get all tables in the database
        all_tables <- DBI::dbListTables(db_con)
        target_tables <- all_tables[grepl(paste0("_", sub, "_"), all_tables)]

        if (length(target_tables) > 0) {
          if (verbose) {
            cli::cli_alert_info(
              glue::glue("[INFO] Cleaning up existing data for sub-{sub} in database..."),
              wrap = TRUE
            )
          }

          tables_cleaned <- 0
          for (table_name in target_tables) {
            drop_query <- paste0("DROP TABLE IF EXISTS \"", table_name, "\"")

            tryCatch(
              {
                DBI::dbExecute(db_con, drop_query)
                tables_cleaned <- tables_cleaned + 1
                if (verbose) {
                  cli::cli_alert_info(
                    glue::glue("[INFO] Dropped table: {table_name} for sub-{sub}"),
                    wrap = TRUE
                  )
                }
              },
              error = function(e) {
                if (verbose) {
                  cli::cli_alert_warning(
                    glue::glue("[WARN] Could not drop table '{table_name}': {e$message}"),
                    wrap = TRUE
                  )
                }
              }
            )
          }

          if (verbose) {
            cli::cli_alert_success(
              glue::glue("[OKAY] Cleaned existing data from {tables_cleaned} tables for sub-{sub}"),
              wrap = TRUE
            )
          }
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(
            glue::glue("[WARN] Error during database cleanup: {e$message}"),
            wrap = TRUE
          )
        }
      }
    )
  }

  for (epoch_name in names(epochs_to_save)) {
    epoch_data <- epochs_to_save[[epoch_name]]
    if (is.list(epoch_data)) {
      cli::cli_alert_info(
        sprintf("[INFO]     %s:", epoch_name),
        wrap = TRUE
      )
      for (block_name in names(epoch_data)) {
        block_data <- epoch_data[[block_name]]
        if (is.data.frame(block_data)) {
          cli::cli_alert_info(
            sprintf(
              "[INFO]         %s: data.frame with %d rows",
              block_name,
              nrow(block_data)
            ),
            wrap = TRUE
          )
        } else {
          cli::cli_alert_info(
            sprintf(
              "[INFO] %s: list with %d elements",
              block_name,
              length(block_data)
            ),
            wrap = TRUE
          )
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
  if (has_multiple_runs) {
    run_num_for_blinks_events <- "N"
  } else {
    # for single run, use provided run_num or extract from block name
    if (!is.null(run_num)) {
      run_num_for_blinks_events <- sprintf("%02d", as.numeric(run_num))
    } else {
      # extract run number from first block name (e.g., "block_1" -> "01")
      first_block_name <- names(eyeris$timeseries)[1]
      block_num <- get_block_numbers(first_block_name)
      run_num_for_blinks_events <- sprintf("%02d", block_num)
    }
  }

  # for binocular data, create left/right subdirectories within the eye directory
  # only create directories if CSV files are being written
  if (csv_enabled) {
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
      cli::cli_alert_info(glue::glue("[INFO] Writing blinks data to {file.path(dir, p, bids_fname)}..."), wrap = TRUE)
    }

    if (is_binocular_object(eyeris)) {
      if (eye_suffix == "eye-L") {
        blinks_df <- purrr::imap_dfr(eyeris$left$blinks, ~ dplyr::mutate(.x))
      } else if (eye_suffix == "eye-R") {
        blinks_df <- purrr::imap_dfr(eyeris$right$blinks, ~ dplyr::mutate(.x))
      }
    } else {
      blinks_df <- purrr::imap_dfr(eyeris$blinks, ~ dplyr::mutate(.x))
    }

    write_csv_and_db(
      data = blinks_df,
      csv_path = file.path(dir, p, bids_fname),
      csv_enabled = csv_enabled,
      db_con = db_con,
      data_type = "blinks",
      sub = sub,
      ses = ses,
      task = task,
      run = run_num_for_blinks_events,
      eye_suffix = eye_suffix,
      verbose = verbose
    )
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
      cli::cli_alert_info(glue::glue("[INFO] Writing events data to {file.path(dir, p, bids_fname)}..."), wrap = TRUE)
    }

    if (is_binocular_object(eyeris)) {
      if (eye_suffix == "eye-L") {
        events_df <- purrr::imap_dfr(eyeris$left$events, ~ dplyr::mutate(.x))
      } else if (eye_suffix == "eye-R") {
        events_df <- purrr::imap_dfr(eyeris$right$events, ~ dplyr::mutate(.x))
      }
    } else {
      events_df <- purrr::imap_dfr(eyeris$events, ~ dplyr::mutate(.x))
    }

    write_csv_and_db(
      data = events_df,
      csv_path = file.path(dir, p, bids_fname),
      csv_enabled = csv_enabled,
      db_con = db_con,
      data_type = "events",
      sub = sub,
      ses = ses,
      task = task,
      run = run_num_for_blinks_events,
      eye_suffix = eye_suffix,
      verbose = verbose
    )
  }

  block_numbers <- get_block_numbers(eyeris)
  block_numbers <- block_numbers[block_numbers > 0]

  # if single block and run_num is provided, override block_numbers for output
  if (!has_multiple_runs && !is.null(run_num)) {
    block_numbers <- as.numeric(run_num)
  }

  if (any_epochs) {
    if (has_multiple_runs) {
      for (epoch_id in names(epochs_to_save)) {
        current_label <- substr(epoch_id, 7, nchar(epoch_id))

        if (verbose) {
          cli::cli_alert_info(glue::glue("[INFO] Processing epoch: {epoch_id} (label: {current_label})"), wrap = TRUE)
        }

        for (i in names(eyeris$timeseries)) {
          run_epochs <- epochs_to_save[[epoch_id]][[i]]
          run_epochs$run <- sprintf("%02d", get_block_numbers(i))

          if (verbose) {
            cli::cli_alert_info(glue::glue("[INFO] Processing run {i} for epoch {epoch_id}"), wrap = TRUE)
          }

          if (is.null(run_epochs)) {
            if (verbose) {
              cli::cli_alert_warning(
                glue::glue("[WARN] Skipping run {i} for epoch {epoch_id} - no data"),
                wrap = TRUE
              )
            }
            next
          }

          if (!is.data.frame(run_epochs) || nrow(run_epochs) == 0) {
            if (verbose) {
              cli::cli_alert_warning(
                glue::glue("[WARN] Skipping run {i} for epoch {epoch_id} - empty or invalid data"),
                wrap = TRUE
              )
            }
            next
          }

          if (verbose) {
            cli::cli_alert_info(
              glue::glue("[INFO] Run {i} for epoch {epoch_id} has {nrow(run_epochs)} rows"),
              wrap = TRUE
            )
          }

          evs <- get_epoch_events(eyeris, epoch_id)
          c_bline <- has_baseline(eyeris, current_label)
          bline_evs <- get_baseline_events(eyeris, epoch_id)
          bline_type <- get_baseline_type(eyeris, epoch_id)

          f <- make_bids_fname(
            sub_id = sub,
            ses_id = ses,
            task_name = task,
            run_num = run_num,
            desc = paste0("preproc_pupil_", current_label),
            eye_suffix = eye_suffix
          )

          write_csv_and_db(
            data = run_epochs,
            csv_path = file.path(dir, p, f),
            csv_enabled = csv_enabled,
            db_con = db_con,
            data_type = "epochs",
            sub = sub,
            ses = ses,
            task = task,
            run = sprintf("%02d", get_block_numbers(i)),
            eye_suffix = eye_suffix,
            epoch_label = current_label,
            verbose = verbose
          )
        }
      }
    } else {
      for (epoch_id in names(epochs_to_save)) {
        current_label <- substr(epoch_id, 7, nchar(epoch_id))

        if (verbose) {
          cli::cli_alert_info(
            sprintf("[INFO] Processing single-run epoch: %s (label: %s)", epoch_id, current_label),
            wrap = TRUE
          )
        }

        epoch_entry <- epochs_to_save[[epoch_id]]
        block_names <- setdiff(names(epoch_entry), "info")
        any_written <- FALSE
        for (block_name in block_names) {
          block_data <- epoch_entry[[block_name]]
          if (is.null(block_data) || !is.data.frame(block_data) || nrow(block_data) == 0) {
            if (verbose) {
              cli::cli_alert_warning(
                "[WARN] Skipping block %s for epoch %s - empty or invalid data",
                block_name,
                epoch_id,
                wrap = TRUE
              )
            }
            next
          }

          if (verbose) {
            cli::cli_alert_info(
              sprintf("[INFO] Block %s for epoch %s has %d rows", block_name, epoch_id, nrow(block_data)),
              wrap = TRUE
            )
          }

          evs <- get_epoch_events(eyeris, epoch_id)
          c_bline <- has_baseline(eyeris, current_label)
          bline_evs <- get_baseline_events(eyeris, epoch_id, block_name)
          bline_type <- get_baseline_type(eyeris, epoch_id, block_name)

          # extract run number from block name
          run_num_for_epoch <- get_block_numbers(block_name)

          # handle case where run number extraction fails
          if (is.null(run_num_for_epoch) || is.na(run_num_for_epoch)) {
            # extract from block name directly (e.g., "block_1" -> 1)
            if (grepl("^block_", block_name)) {
              run_num_for_epoch <- as.numeric(gsub("block_", "", block_name))
            } else {
              run_num_for_epoch <- 1 # default fallback
            }
          }

          f <- make_bids_fname(
            sub_id = sub,
            ses_id = ses,
            task_name = task,
            run_num = sprintf("%02d", run_num_for_epoch),
            desc = "preproc_pupil",
            epoch_name = current_label,
            epoch_events = evs,
            baseline_events = bline_evs,
            baseline_type = bline_type,
            eye_suffix = eye_suffix
          )

          write_csv_and_db(
            data = block_data,
            csv_path = file.path(bids_dir, p, f),
            csv_enabled = csv_enabled,
            db_con = db_con,
            data_type = "epochs",
            sub = sub,
            ses = ses,
            task = task,
            run = sprintf("%02d", run_num_for_epoch),
            eye_suffix = eye_suffix,
            epoch_label = current_label,
            verbose = verbose
          )
          any_written <- TRUE
        }
        if (!any_written && verbose) {
          cli::cli_alert_warning(glue::glue("[WARN] No valid blocks found for epoch {epoch_id}"), wrap = TRUE)
        }
      }
    }
  }

  if (save_raw && is.null(eye_suffix)) {
    if (has_multiple_runs) {
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

        write_csv_and_db(
          data = run_data,
          csv_path = file.path(dir, p, f),
          csv_enabled = csv_enabled,
          db_con = db_con,
          data_type = "timeseries",
          sub = sub,
          ses = ses,
          task = task,
          run = sprintf("%02d", i),
          eye_suffix = eye_suffix,
          verbose = verbose
        )
      })
    } else {
      # single run (monocular) case: write the raw timeseries
      if (is.list(eyeris$timeseries) && length(eyeris$timeseries) > 0) {
        run_data <- eyeris$timeseries[[1]]
      } else {
        cli::cli_abort("[EXIT] eyeris$timeseries is either not a list or is empty. Cannot access the first element.")
      }
      # use run_num if provided, otherwise default to 1
      run_num_to_use <- if (!is.null(run_num)) {
        run_num_numeric <- suppressWarnings(as.numeric(run_num))
        if (!is.na(run_num_numeric)) {
          sprintf("%02d", run_num_numeric)
        } else {
          cli::cli_alert_warning("[WARN] Invalid run_num provided. Defaulting to '01'.", wrap = TRUE)
          "01"
        }
      } else {
        "01"
      }
      f <- make_bids_fname(
        sub_id = sub,
        ses_id = ses,
        task_name = task,
        run_num = run_num_to_use,
        desc = "timeseries",
        eye_suffix = eye_suffix
      )

      write_csv_and_db(
        data = run_data,
        csv_path = file.path(dir, p, f),
        csv_enabled = csv_enabled,
        db_con = db_con,
        data_type = "timeseries",
        sub = sub,
        ses = ses,
        task = task,
        run = run_num_to_use,
        eye_suffix = eye_suffix,
        verbose = verbose
      )
    }
  }

  # for binocular data, save timeseries in the appropriate left/right subdirectory
  if (save_raw && !is.null(eye_suffix)) {
    if (has_multiple_runs) {
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

        write_csv_and_db(
          data = run_data,
          csv_path = file.path(dir, p, f),
          csv_enabled = csv_enabled,
          db_con = db_con,
          data_type = "timeseries",
          sub = sub,
          ses = ses,
          task = task,
          run = sprintf("%02d", i),
          eye_suffix = eye_suffix,
          verbose = verbose
        )
      })
    } else {
      # single run (binocular) case: write the raw timeseries
      if (is.list(eyeris$timeseries) && length(eyeris$timeseries) > 0) {
        run_data <- eyeris$timeseries[[1]]
      } else {
        run_data <- eyeris$timeseries
      }

      f <- make_bids_fname(
        sub_id = sub,
        ses_id = ses,
        task_name = task,
        run_num = sprintf("%02d", run_data$block[1]),
        desc = "timeseries",
        eye_suffix = eye_suffix
      )

      write_csv_and_db(
        data = run_data,
        csv_path = file.path(dir, p, f),
        csv_enabled = csv_enabled,
        db_con = db_con,
        data_type = "timeseries",
        sub = sub,
        ses = ses,
        task = task,
        run = sprintf("%02d", run_data$block[1]),
        eye_suffix = eye_suffix,
        verbose = verbose
      )
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
            sprintf("_run-%02d", as.numeric(block_numbers)),
            "_desc-confounds",
            if (!is.null(eye_suffix)) paste0("_", eye_suffix) else ""
          )
        },
        verbose = verbose,
        run_num = sprintf("%02d", as.numeric(block_numbers)),
        csv_enabled = csv_enabled,
        db_con = db_con,
        sub = sub,
        ses = ses,
        task = task,
        eye_suffix = eye_suffix
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
                sprintf("_run-%02d", as.numeric(block)),
                "_desc-confounds",
                if (!is.null(eye_suffix)) paste0("_", eye_suffix) else ""
              )
            },
            verbose = verbose,
            run_num = sprintf("%02d", as.numeric(block)),
            csv_enabled = csv_enabled,
            db_con = db_con,
            sub = sub,
            ses = ses,
            task = task,
            eye_suffix = eye_suffix
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
      # collect epoch info using the simplified unlist() approach
      epoch_summaries <- list()

      for (epoch_name in names(eyeris)[grep("^epoch_", names(eyeris))]) {
        block_name <- paste0("block_", block)

        epoch_info <- get_epoch_info(eyeris, epoch_name, block_name)
        if (!is.null(epoch_info)) {
          unlisted_info <- unlist(epoch_info)
          names(unlisted_info) <- sub("^[^.]+\\.", "", names(unlisted_info))

          epoch_row <- data.frame(
            epoch_type = epoch_name,
            t(unlisted_info),
            stringsAsFactors = FALSE
          )

          epoch_summaries[[epoch_name]] <- epoch_row

          if (verbose) {
            cli::cli_alert_info(
              paste0("[INFO] Created epoch summary for ", epoch_name, " with ", length(unlisted_info), " fields"),
              wrap = TRUE
            )
          }
        } else {
          # No info found, create minimal row
          epoch_summaries[[epoch_name]] <- data.frame(
            epoch_type = epoch_name,
            stringsAsFactors = FALSE
          )

          if (verbose) {
            cli::cli_alert_warning(
              paste0("[WARN] No epoch info found for ", epoch_name),
              wrap = TRUE
            )
          }
        }
      }

      # Combine all epoch summaries into one data frame
      if (length(epoch_summaries) > 0) {
        epoch_summary <- do.call(rbind, epoch_summaries)
        rownames(epoch_summary) <- NULL
      } else {
        epoch_summary <- data.frame(epoch_type = character(0))
      }

      summary_filename <- make_bids_fname(
        sub_id = sub,
        ses_id = ses,
        task_name = task,
        run_num = sprintf("%02d", as.numeric(block)),
        desc = "epoch_summary",
        eye_suffix = eye_suffix
      )
      summary_filepath <- file.path(dir, p, summary_filename)

      write_csv_and_db(
        data = epoch_summary,
        csv_path = summary_filepath,
        csv_enabled = csv_enabled,
        db_con = db_con,
        data_type = "epoch_summary",
        sub = sub,
        ses = ses,
        task = task,
        run = sprintf("%02d", as.numeric(block)),
        eye_suffix = eye_suffix,
        verbose = verbose
      )
    }

    # export epoch-wide confounds
    if (!is.null(eyeris$confounds$epoched_epoch_wide) && any_epochs) {
      for (epoch_name in names(eyeris$confounds$epoched_epoch_wide)) {
        epoch_label <- sub("^epoch_", "", epoch_name)

        epoch_folder <- file.path(dir, p, paste0("epoch_", epoch_label))
        if (csv_enabled && !dir.exists(epoch_folder)) {
          dir.create(epoch_folder, recursive = TRUE)
        }

        epoch_events_info <- get_epoch_events(eyeris, epoch_name)
        baseline_events_info <- get_baseline_events(eyeris, epoch_name)
        baseline_type_info <- get_baseline_type(eyeris, epoch_name)

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

            write_csv_and_db(
              data = event_confounds,
              csv_path = epoch_filepath,
              csv_enabled = csv_enabled,
              db_con = db_con,
              data_type = "confounds_summary",
              sub = sub,
              ses = ses,
              task = task,
              run = sprintf("%02d", get_block_numbers(block_name)),
              eye_suffix = eye_suffix,
              epoch_label = epoch_label,
              verbose = verbose
            )
          }
        }
      }
    }

    if (!is.null(eyeris$confounds$epoched_timeseries)) {
      for (epoch_name in names(eyeris$confounds$epoched_timeseries)) {
        epoch_label <- sub("^epoch_", "", epoch_name)

        epoch_folder <- file.path(dir, p, paste0("epoch_", epoch_label))
        if (csv_enabled && !dir.exists(epoch_folder)) {
          dir.create(epoch_folder, recursive = TRUE)
        }

        epoch_events_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)$baseline_name) &&
              !is.null(
                eyeris[[
                  find_baseline_structure(
                    eyeris,
                    epoch_label
                  )$baseline_name
                ]]$block_1$info$epoch_events
              )
          ) {
            epoch_events <- eyeris[[
              find_baseline_structure(
                eyeris,
                epoch_label
              )$baseline_name
            ]]$block_1$info$epoch_events
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
                  cli::cli_alert_info(
                    paste0("[INFO] Found epoch events in epoch structure: ", escaped_result),
                    wrap = TRUE
                  )
                }
              }
            }
            NULL
          }
        baseline_events_info <-
          if (
            !is.null(find_baseline_structure(eyeris, epoch_label)$baseline_name) &&
              !is.null(
                eyeris[[
                  find_baseline_structure(
                    eyeris,
                    epoch_label
                  )$baseline_name
                ]]$block_1$info$baseline_events
              )
          ) {
            baseline_events <- eyeris[[
              find_baseline_structure(
                eyeris,
                epoch_label
              )$baseline_name
            ]]$block_1$info$baseline_events
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
            !is.null(find_baseline_structure(eyeris, epoch_label)$baseline_name) &&
              !is.null(
                eyeris[[
                  find_baseline_structure(
                    eyeris,
                    epoch_label
                  )$baseline_name
                ]]$block_1$info$baseline_type
              )
          ) {
            baseline_type <- eyeris[[
              find_baseline_structure(
                eyeris,
                epoch_label
              )$baseline_name
            ]]$block_1$info$baseline_type
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

            write_csv_and_db(
              data = event_confounds,
              csv_path = event_filepath,
              csv_enabled = csv_enabled,
              db_con = db_con,
              data_type = "confounds_events",
              sub = sub,
              ses = ses,
              task = task,
              run = sprintf("%02d", get_block_numbers(block_name)),
              eye_suffix = eye_suffix,
              epoch_label = epoch_label,
              verbose = verbose
            )
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
    if (actual_block_count > 1) {
      has_multiple_runs <- TRUE
      num_runs <- length(eyeris$timeseries)
    } else {
      has_multiple_runs <- FALSE
      num_runs <- 1
    }

    for (i_run in block_numbers) {
      current_data <- eyeris$timeseries[[paste0("block_", i_run)]]

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
          cli::cli_alert_success(
            glue::glue("[OKAY] Created gaze heatmap for run-{sprintf('%02d', i_run)}"),
            wrap = TRUE
          )
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
          cli::cli_alert_info(
            glue::glue("[INFO] Created binocular correlation plot for run-{sprintf('%02d', i_run)}"),
            wrap = TRUE
          )
        }
      }
    }

    # now handle epochs (if present)
    if (!is.null(report_epoch_grouping_var_col)) {
      for (i in seq_along(epochs_to_save)) {
        epoch_data <- epochs_to_save[[i]]

        if (is.null(epoch_data) || !is.list(epoch_data)) {
          if (verbose) {
            cli::cli_alert_warning(
              glue::glue("[WARN] Skipping epoch {i} for report generation - no valid data"),
              wrap = TRUE
            )
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
              cli::cli_alert_warning(
                glue::glue("[WARN] Skipping block {bn} for epoch {i} - no valid data"),
                wrap = TRUE
              )
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
                  ),
                  wrap = TRUE
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
                cli::cli_alert_success(
                  glue::glue("[OKAY] Created gaze heatmap for epoch {group} (run-{sprintf('%02d', run_dir_num)})"),
                  wrap = TRUE
                )
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

  # disconnect from DB
  if (!is.null(db_con)) {
    status <- disconnect_eyeris_database(db_con, verbose = verbose)
  }

  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "secs"), 2)
  if (verbose) {
    cli::cli_alert_info(
      glue::glue(
        "[INFO] Finished BIDSify for sub-{sub} at {format(end_time, '%Y-%m-%d %H:%M:%S')} (Duration: {duration} seconds)"
      )
    )
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

  if (!is.null(epoch_name)) {
    # prioritize epoch_name (clean label) over epoch_events (raw pattern)
    epoch_name_clean <- sub("^epoch_", "", epoch_name)
    desc_parts <- c(desc_parts, paste0("epoch-", sanitize_event_tag(epoch_name_clean, "")))
  } else if (!is.null(epoch_events)) {
    # fallback: use epoch_events pattern if no epoch_name provided
    epoch_event_name <- if (
      is.character(epoch_events) &&
        length(epoch_events) == 1
    ) {
      gsub("[*{}]", "", epoch_events)
    } else {
      "multi_events"
    }
    desc_parts <- c(desc_parts, paste0("epoch-", sanitize_event_tag(epoch_event_name, "")))
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
    cli::cli_alert_info(
      paste0(
        "[INFO] Available baseline structures: ",
        paste(baseline_names, collapse = ", ")
      ),
      wrap = TRUE
    )
    cli::cli_alert_info(
      paste0("[INFO] Looking for epoch label: ", epoch_label),
      wrap = TRUE
    )
  }

  for (baseline_name in baseline_names) {
    if (grepl(paste0("_epoch_", epoch_label, "$"), baseline_name)) {
      cli::cli_alert_info(
        paste0("[INFO] Found matching baseline structure: ", baseline_name),
        wrap = TRUE
      )
      return(
        list(
          baseline_name = baseline_name,
          baseline_blocks = names(eyeris[[baseline_name]])
        )
      )
    }
  }

  cli::cli_alert_warning(
    paste0(
      "[WARN] No baseline structure found for epoch label: ",
      epoch_label
    ),
    wrap = TRUE
  )
  NULL
}

get_epoch_info <- function(eyeris, epoch_id, block_name = "block_1") {
  epoch_label <- substr(epoch_id, 7, nchar(epoch_id))
  baseline_structure_list <- find_baseline_structure(eyeris, epoch_label)

  if (is.null(block_name)) {
    block_name <- baseline_structure_list$baseline_blocks[1]
  }

  if (
    !is.null(baseline_structure_list) && !is.null(eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info)
  ) {
    return(
      list(
        calc_baseline = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$calc_baseline,
        apply_baseline = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$apply_baseline,
        baseline_type = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$baseline_type,
        baseline_events = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$baseline_events,
        baseline_period = paste0(
          "(",
          eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$baseline_period[1],
          ", ",
          eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$baseline_period[2],
          ")"
        ),
        epoch_events = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$epoch_events,
        epoch_limits = paste0(
          "(",
          eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$epoch_limits[1],
          ", ",
          eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$epoch_limits[2],
          ")"
        ),
        n_epochs = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$n_epochs,
        n_baseline_epochs = eyeris[[baseline_structure_list$baseline_name]][[block_name]]$info$n_baseline_epochs
      )
    )
  }

  if (!is.null(eyeris[[epoch_id]]) && !is.null(eyeris[[epoch_id]]$info)) {
    return(
      list(
        calc_baseline = eyeris[[epoch_id]]$info[[block_name]]$calc_baseline,
        apply_baseline = eyeris[[epoch_id]]$info[[block_name]]$apply_baseline,
        baseline_type = NA_character_,
        baseline_events = NA_character_,
        baseline_period = NA_character_,
        epoch_events = eyeris[[epoch_id]]$info[[block_name]]$epoch_events,
        epoch_limits = paste0(
          "(",
          eyeris[[epoch_id]]$info[[block_name]]$epoch_limits[1],
          ", ",
          eyeris[[epoch_id]]$info[[block_name]]$epoch_limits[2],
          ")"
        ),
        n_epochs = eyeris[[epoch_id]]$info[[block_name]]$n_epochs,
        n_baseline_epochs = NA_character_
      )
    )
  }

  return(NULL)
}

format_event_string <- function(events) {
  if (is.null(events)) {
    return(NULL)
  }

  if (is.character(events)) {
    if (length(events) == 1) {
      return(events)
    } else {
      return(paste(events, collapse = ", "))
    }
  } else {
    return(paste(events, collapse = ", "))
  }
}

get_epoch_events <- function(eyeris, epoch_id, block_name = "block_1") {
  info <- get_epoch_info(eyeris, epoch_id, block_name)
  if (!is.null(info) && !is.null(info$epoch_events)) {
    result <- format_event_string(info$epoch_events)
    if (!is.null(result)) {
      escaped_result <- gsub("\\{", "{{", gsub("\\}", "}}", result))
      cli::cli_alert_info(
        paste0("[INFO] Found epoch events in structure: ", escaped_result),
        wrap = TRUE
      )
      return(result)
    }
  }
  return(NULL)
}

get_baseline_events <- function(eyeris, epoch_id, block_name = "block_1") {
  info <- get_epoch_info(eyeris, epoch_id, block_name)
  if (!is.null(info) && !is.na(info$baseline_events)) {
    result <- format_event_string(info$baseline_events)
    if (!is.na(result)) {
      cli::cli_alert_info("[INFO] Found baseline events: ", result, wrap = TRUE)
      return(result)
    }
  }
  return(NULL)
}

get_baseline_type <- function(eyeris, epoch_id, block_name = "block_1") {
  info <- get_epoch_info(eyeris, epoch_id, block_name)
  if (!is.null(info) && !is.na(info$baseline_type)) {
    result <- format_event_string(info$baseline_type)
    if (!is.na(result)) {
      cli::cli_alert_info("[INFO] Found baseline type: ", result, wrap = TRUE)
      return(result)
    }
  }
  return(NULL)
}

has_baseline <- function(eyeris, epoch_label) {
  epoch_id <- paste0("epoch_", epoch_label)
  !is.na(find_baseline_structure(eyeris, epoch_label)$baseline_name)
}
