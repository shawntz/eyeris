#' Create or connect to eyeris project database
#'
#' Creates a new `DuckDB` database for the `eyeris` project or connects to an existing one.
#' The database will be created in the BIDS derivatives directory. When parallel processing
#' is enabled, creates temporary databases to avoid concurrency issues.
#'
#' @param bids_dir Path to the BIDS directory containing derivatives
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#' @param verbose Whether to print verbose output
#' @param parallel Whether to enable parallel processing with temporary databases
#'
#' @return DBI database connection object or temp database info list (when parallel=TRUE)
#'
#' @keywords internal
connect_eyeris_database <- function(
  bids_dir,
  db_path = "my-project",
  verbose = FALSE,
  parallel = FALSE
) {
  # use temporary database for parallel processing
  if (parallel) {
    return(create_temp_eyeris_database(
      bids_dir = bids_dir,
      base_db_path = db_path,
      verbose = verbose
    ))
  }
  derivatives_dir <- file.path(bids_dir, "derivatives")
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    log_info(
      "Created derivatives directory: {derivatives_dir}",
      verbose = verbose
    )
  }

  # auto-append .eyerisdb extension if not present
  if (!grepl("\\.eyerisdb$", db_path)) {
    db_path <- paste0(db_path, ".eyerisdb")
  }

  if (dirname(db_path) == ".") {
    full_db_path <- file.path(derivatives_dir, db_path)
  } else {
    full_db_path <- db_path
  }

  tryCatch(
    {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = full_db_path)

      if (file.exists(full_db_path)) {
        log_success(
          "Connected to existing eyeris project database: {full_db_path}",
          verbose = verbose
        )
      } else {
        log_success(
          "Created new eyeris database: {full_db_path}",
          verbose = verbose
        )
      }

      return(con)
    },
    error = function(e) {
      log_warn("Failed to connect to database: {e$message}", verbose = TRUE)
      return(NULL)
    }
  )
}

#' Disconnect from eyeris database
#'
#' Safely disconnects from the `eyeris` project database.
#'
#' @param con Database connection object
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating success
#'
#' @keywords internal
disconnect_eyeris_database <- function(con, verbose = FALSE) {
  if (is.null(con)) {
    return(TRUE)
  }

  tryCatch(
    {
      DBI::dbDisconnect(con)
      log_info("Disconnected from eyeris database", verbose = verbose)
      return(TRUE)
    },
    error = function(e) {
      log_warn(
        "Error disconnecting from database: {e$message}",
        verbose = verbose
      )
      return(FALSE)
    }
  )
}

#' Create table name for eyeris data
#'
#' Generates a standardized table name for `eyeris` data based on the data type
#' and subject information.
#'
#' @param data_type Type of data ("timeseries", "epochs", "epoch_timeseries", "epoch_summary", "events", "blinks")
#' @param sub Subject ID
#' @param ses Session ID
#' @param task Task name
#' @param run Run number
#' @param eye_suffix Optional eye suffix for binocular data
#'
#' @return Character string with table name
#'
#' @keywords internal
create_table_name <- function(
  data_type,
  sub,
  ses,
  task,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL
) {
  # base table name
  table_name <- paste0(data_type, "_", sub, "_", ses, "_", task)

  # add run if provided
  if (!is.null(run)) {
    # ensure run number is formatted as 2-digit string
    formatted_run <- if (is.numeric(run)) {
      sprintf("%02d", run)
    } else if (is.character(run)) {
      # if already a string, check if it needs zero-padding
      if (nchar(run) == 1 && grepl("^[0-9]$", run)) {
        sprintf("%02d", as.numeric(run))
      } else {
        run
      }
    } else {
      run
    }
    table_name <- paste0(table_name, "_run", formatted_run)
  }

  # add epoch label if provided (for epoched data)
  if (!is.null(epoch_label)) {
    # sanitize epoch label for database table naming
    sanitized_label <- gsub("[^a-zA-Z0-9]", "", epoch_label)
    sanitized_label <- tolower(sanitized_label)
    table_name <- paste0(table_name, "_", sanitized_label)
  }

  # add eye suffix if provided
  if (!is.null(eye_suffix)) {
    clean_suffix <- gsub("[-_]", "", eye_suffix)
    table_name <- paste0(table_name, "_", clean_suffix)
  }

  table_name <- gsub("[^a-zA-Z0-9_]", "_", table_name)

  return(table_name)
}

#' Write eyeris data to database
#'
#' Writes `eyeris` data to the project database as an alternative to CSV files.
#' Creates or updates tables as needed.
#'
#' @param data Data frame to write
#' @param con Database connection
#' @param data_type Type of data ("timeseries", "epochs", "epoch_timeseries", "epoch_summary", "events", "blinks")
#' @param sub Subject ID
#' @param ses Session ID
#' @param task Task name
#' @param run Run number
#' @param eye_suffix Optional eye suffix for binocular data
#' @param epoch_label Optional epoch label for epoched data (used in table naming)
#' @param append Whether to append to existing table (default TRUE)
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating success
#'
#' @keywords internal
write_eyeris_data_to_db <- function(
  data,
  con,
  data_type,
  sub,
  ses,
  task,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL,
  append = TRUE,
  verbose = FALSE
) {
  if (is.null(con)) {
    log_warn("No database connection provided", verbose = verbose)
    return(FALSE)
  }

  if (is.null(data) || nrow(data) == 0) {
    log_warn("No data to write to database", verbose = verbose)
    return(FALSE)
  }

  table_name <- create_table_name(
    data_type,
    sub,
    ses,
    task,
    run,
    eye_suffix,
    epoch_label
  )

  tryCatch(
    {
      metadata_cols <- data.frame(
        subject_id = sub,
        session_id = ses,
        task_name = task,
        data_type = data_type,
        stringsAsFactors = FALSE
      )

      if (!is.null(run)) {
        metadata_cols$run_number <- run
      }

      if (!is.null(eye_suffix)) {
        metadata_cols$eye_suffix <- eye_suffix
      }

      if (!is.null(epoch_label)) {
        metadata_cols$epoch_label <- epoch_label
      }

      metadata_cols$created_timestamp <- Sys.time()

      data <- cbind(metadata_cols, data)

      # when dropping existing entries, create fresh table
      # actual_append <- if (drop_existing_subject) FALSE else append
      actual_append <- append

      log_info(
        "Writing {nrow(data)} rows to table '{table_name}' (append={actual_append})",
        verbose = verbose
      )

      DBI::dbWriteTable(
        conn = con,
        name = table_name,
        value = data,
        append = actual_append,
        overwrite = !actual_append
      )

      action <- if (append) "Added" else "Created"
      log_success(
        "{action} table '{table_name}' with {nrow(data)} rows",
        verbose = verbose
      )

      return(TRUE)
    },
    error = function(e) {
      log_warn(
        "Failed to write data to table '{table_name}': {e$message}",
        verbose = verbose
      )
      return(FALSE)
    }
  )
}

#' List available tables in eyeris database
#'
#' Lists all tables in the `eyeris` project database with optional filtering.
#'
#' @param con Database connection
#' @param data_type Optional filter by data type
#' @param subject Optional filter by subject ID
#'
#' @return Character vector of table names
#'
#' @export
eyeris_db_list_tables <- function(con, data_type = NULL, subject = NULL) {
  if (is.null(con)) {
    log_warn("No database connection provided", verbose = TRUE)
    return(character(0))
  }

  tryCatch(
    {
      tables <- DBI::dbListTables(con)

      # filter by data type if provided
      if (!is.null(data_type)) {
        pattern <- paste0("^", data_type, "_")
        tables <- tables[grepl(pattern, tables)]
      }

      # filter by subject if provided
      if (!is.null(subject)) {
        pattern <- paste0("_", subject, "_")
        tables <- tables[grepl(pattern, tables)]
      }

      return(tables)
    },
    error = function(e) {
      log_warn("Failed to list tables: {e$message}", verbose = TRUE)
      return(character(0))
    }
  )
}

#' Read eyeris data from database
#'
#' Reads `eyeris` data from the project database with dplyr-style interface.
#'
#' @param con Database connection
#' @param data_type Type of data to read ("timeseries", "epochs", "epoch_timeseries", "epoch_summary", "events", "blinks")
#' @param subject Optional subject ID filter
#' @param session Optional session ID filter
#' @param task Optional task name filter
#' @param run Optional run number filter
#' @param eye_suffix Optional eye suffix filter
#' @param epoch_label Optional epoch label filter (for epoched data)
#' @param table_name Exact table name (overrides other parameters)
#'
#' @return Data frame with requested data
#'
#' @export
eyeris_db_read <- function(
  con,
  data_type = NULL,
  subject = NULL,
  session = NULL,
  task = NULL,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL,
  table_name = NULL
) {
  if (is.null(con)) {
    log_error("No database connection provided")
  }

  tryCatch(
    {
      if (!is.null(table_name)) {
        return(DBI::dbReadTable(con, table_name))
      }

      # build query based on filters
      query <- "SELECT * FROM ("

      tables <- DBI::dbListTables(con)

      if (length(tables) == 0) {
        log_warn("No tables found in database", verbose = TRUE)
        return(data.frame())
      }

      # filter tables based on criteria
      if (!is.null(data_type)) {
        pattern <- paste0("^", data_type, "_")
        tables <- tables[grepl(pattern, tables)]
      }

      # filter by epoch label if provided
      if (!is.null(epoch_label)) {
        sanitized_label <- gsub("[^a-zA-Z0-9]", "", epoch_label)
        sanitized_label <- tolower(sanitized_label)
        pattern <- paste0("_", sanitized_label, "(_|$)")
        tables <- tables[grepl(pattern, tables)]
      }

      if (length(tables) == 0) {
        return(data.frame())
      }

      # unite all matching tables (validate tables exist first)
      valid_tables <- c()
      for (table in tables) {
        # verify table actually exists before adding to query
        if (table %in% DBI::dbListTables(con)) {
          valid_tables <- c(valid_tables, table)
        }
      }

      if (length(valid_tables) == 0) {
        return(data.frame())
      }

      union_queries <- c()
      for (table in valid_tables) {
        union_queries <- c(
          union_queries,
          paste0("SELECT * FROM \"", table, "\"")
        )
      }

      query <- paste(union_queries, collapse = " UNION ALL ")
      query <- paste0("SELECT * FROM (", query, ") as combined_data WHERE 1=1")

      if (!is.null(subject)) {
        query <- paste(query, "AND subject_id =", paste0("'", subject, "'"))
      }

      if (!is.null(session)) {
        query <- paste(query, "AND session_id =", paste0("'", session, "'"))
      }

      if (!is.null(task)) {
        query <- paste(query, "AND task_name =", paste0("'", task, "'"))
      }

      if (!is.null(run)) {
        query <- paste(query, "AND run_number =", paste0("'", run, "'"))
      }

      if (!is.null(eye_suffix)) {
        query <- paste(query, "AND eye_suffix =", paste0("'", eye_suffix, "'"))
      }

      # dev note: epoch_label filtering is handled by table selection above
      # and no additional WHERE clause needed since tables are already filtered by epoch

      # execute query
      log_info("Executing query: {query}", verbose = TRUE)
      result <- DBI::dbGetQuery(con, query)

      return(result)
    },
    error = function(e) {
      log_warn("Failed to read from database: {e$message}", verbose = TRUE)
      return(data.frame())
    }
  )
}

#' Connect to eyeris project database (user-facing)
#'
#' User-friendly function to connect to an existing `eyeris` project database.
#' This function provides easy access for users to query their `eyeris` data.
#'
#' @param bids_dir Path to the BIDS directory containing the database
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#'   If just a filename, will look in `derivatives/` directory.
#'   If includes path, will use as provided.
#'
#' @return Database connection object for use with other eyeris database functions
#'
#' @examples
#' \donttest{
#' # step 1: create a database using bidsify with db_enabled = TRUE
#' # (This example assumes you have already run bidsify to create a database)
#'
#' # temp dir for testing
#' temp_dir <- tempdir()
#'
#' # step 2: connect to eyeris DB (will fail gracefully if no DB exists)
#' tryCatch({
#'   con <- eyeris_db_connect(temp_dir)
#'
#'   tables <- eyeris_db_list_tables(con)
#'
#'   # read timeseries data for a specific subject
#'   data <- eyeris_db_read(con, data_type = "timeseries", subject = "001")
#'
#'   # close connection when done
#'   eyeris_db_disconnect(con)
#' }, error = function(e) {
#'   message("No eyeris DB found - create one first with bidsify(db_enabled = TRUE)")
#' })
#' }
#'
#' @export
eyeris_db_connect <- function(bids_dir, db_path = "my-project") {
  # auto-append .eyerisdb extension if not present
  if (!grepl("\\.eyerisdb$", db_path)) {
    db_path <- paste0(db_path, ".eyerisdb")
  }

  if (dirname(db_path) == ".") {
    full_db_path <- file.path(bids_dir, "derivatives", db_path)
  } else {
    full_db_path <- db_path
  }

  if (!file.exists(full_db_path)) {
    log_error(
      "No eyeris database found at: {full_db_path}\n       Run bidsify() with db_enabled = TRUE to create a database first."
    )
  }

  tryCatch(
    {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = full_db_path)
      log_success(
        "Connected to eyeris database: {full_db_path}",
        verbose = TRUE
      )
      return(con)
    },
    error = function(e) {
      log_error("Failed to connect to database: {e$message}")
    }
  )
}

#' Disconnect from eyeris database (user-facing)
#'
#' User-friendly function to disconnect from the `eyeris` project database.
#'
#' @param con Database connection object
#'
#' @return Logical indicating success
#'
#' @export
eyeris_db_disconnect <- function(con) {
  status <- disconnect_eyeris_database(con, verbose = TRUE)
}

#' Write data to CSV and/or database (helper function)
#'
#' This helper function writes data to CSV files and/or database based on the
#' configuration. Useful for large-scale cloud compute where CSV files may be
#' unnecessary when using database storage.
#'
#' @param data Data frame to write
#' @param csv_path Full path where CSV file should be written (ignored if csv_enabled = FALSE)
#' @param csv_enabled Whether to write CSV files (defaults to TRUE for backward compatibility)
#' @param db_con Database connection (NULL if not enabled)
#' @param data_type Type of data ("timeseries", "epochs", "epoch_timeseries", "epoch_summary", "events", "blinks", "confounds")
#' @param sub Subject ID
#' @param ses Session ID
#' @param task Task name
#' @param run Run number (optional)
#' @param eye_suffix Eye suffix for binocular data (optional)
#' @param epoch_label Epoch label for epoched data (optional, used in table naming)
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating success
#'
#' @keywords internal
write_csv_and_db <- function(
  data,
  csv_path,
  csv_enabled = TRUE,
  db_con = NULL,
  data_type = NULL,
  sub = NULL,
  ses = NULL,
  task = NULL,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL,
  verbose = FALSE
) {
  csv_success <- TRUE
  db_success <- TRUE
  outputs <- c()

  if (csv_enabled) {
    tryCatch(
      {
        write.csv(data, csv_path, row.names = FALSE)
        csv_success <- TRUE
        outputs <- c(outputs, "CSV")
      },
      error = function(e) {
        log_warn("Failed to write CSV file: {csv_path}", verbose = verbose)
        csv_success <- FALSE
      }
    )
  }

  if (
    !is.null(db_con) &&
      !is.null(data_type) &&
      !is.null(sub) &&
      !is.null(ses) &&
      !is.null(task)
  ) {
    db_success <- write_eyeris_data_to_db(
      data = data,
      con = db_con,
      data_type = data_type,
      sub = sub,
      ses = ses,
      task = task,
      run = run,
      eye_suffix = eye_suffix,
      epoch_label = epoch_label,
      append = TRUE,
      verbose = FALSE # suppress individual DB logging to avoid duplication
    )

    if (db_success) {
      outputs <- c(outputs, "database")
    }
  }

  # centralized success logging
  if (verbose && length(outputs) > 0 && csv_success && db_success) {
    output_str <- paste(outputs, collapse = " and ")
    data_type_str <- if (is.null(data_type)) "data" else data_type
    log_success(
      "Wrote {data_type_str} data ({nrow(data)} rows) to {output_str}",
      verbose = verbose
    )
  }

  return(csv_success && db_success)
}

#' Extract and aggregate eyeris data across subjects from database
#'
#' A comprehensive wrapper function that simplifies extracting `eyeris` data from
#' the database. Provides easy one-liner access to aggregate data across multiple
#' subjects for each data type, without requiring SQL knowledge.
#'
#' @param bids_dir Path to the BIDS directory containing the database
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#' @param subjects Vector of subject IDs to include. If NULL (default), includes all subjects
#' @param data_types Vector of data types to extract. If NULL (default), extracts all available types.
#'   Valid types: "blinks", "events", "timeseries", "epochs", "epoch_summary",
#'   "run_confounds", "confounds_events", "confounds_summary"
#' @param sessions Vector of session IDs to include. If NULL (default), includes all sessions
#' @param tasks Vector of task names to include. If NULL (default), includes all tasks
#' @param epoch_labels Vector of epoch labels to include. If NULL (default), includes all epochs.
#'   Only applies to epoch-related data types
#' @param eye_suffixes Vector of eye suffixes to include. If NULL (default), includes all eyes.
#'   Typically c("eye-L", "eye-R") for binocular data
#' @param verbose Logical. Whether to print progress messages (default TRUE)
#'
#' @return A named list of data frames, one per data type
#'
#' @examples
#' \donttest{
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#' eyeris::glassbox() |>
#' eyeris::epoch(
#'   events = "PROBE_{startstop}_{trial}",
#'   limits = c(-1, 1),
#'   label = "prePostProbe"
#' ) |>
#' eyeris::bidsify(
#'   bids_dir = tempdir(),
#'   participant_id = "001",
#'   session_num = "01",
#'   task_name = "assocret",
#'   run_num = "03", # override default run-01 (block_1) to use run-03 instead
#'   db_enabled = TRUE # enable database storage
#' )
#'
#' # extract all data for all subjects (returns list of data frames)
#' all_data <- eyeris_db_collect(tempdir())
#'
#' # view available data types
#' names(all_data)
#'
#' # access specific data type
#' blinks_data <- all_data$blinks
#' epochs_data <- all_data$epochs
#'
#' # extract specific subjects and data types
#' subset_data <- eyeris_db_collect(
#'   bids_dir = tempdir(),
#'   subjects = c("001"),
#'   data_types = c("blinks", "epochs", "timeseries")
#' )
#'
#' # extract epoch data for specific epoch label
#' epoch_data <- eyeris_db_collect(
#'   bids_dir = tempdir(),
#'   data_types = "epochs",
#'   epoch_labels = "prepostprobe"
#' )
#'
#' }
#'
#' @export
eyeris_db_collect <- function(
  bids_dir,
  db_path = "my-project",
  subjects = NULL,
  data_types = NULL,
  sessions = NULL,
  tasks = NULL,
  epoch_labels = NULL,
  eye_suffixes = NULL,
  verbose = TRUE
) {
  # connect to database
  log_info("Connecting to eyeris database...", verbose = verbose)

  con <- tryCatch(
    {
      eyeris_db_connect(bids_dir, db_path)
    },
    error = function(e) {
      log_error("Failed to connect to database: {e$message}")
    }
  )

  # ensure disconnection on exit
  on.exit(eyeris_db_disconnect(con))

  # get available tables
  all_tables <- eyeris_db_list_tables(con)

  if (length(all_tables) == 0) {
    log_warn("No tables found in database", verbose = TRUE)
    return(list())
  }

  log_info("Found {length(all_tables)} tables in database", verbose = verbose)

  # define all possible data types
  all_data_types <- c(
    "blinks",
    "events",
    "timeseries",
    "epochs",
    "epoch_summary",
    "run_confounds",
    "confounds_events",
    "confounds_summary"
  )

  # use all data types if none specified
  if (is.null(data_types)) {
    data_types <- all_data_types
  } else {
    # validate specified data types
    invalid_types <- setdiff(data_types, all_data_types)
    if (length(invalid_types) > 0) {
      log_warn(
        "Invalid data types ignored: {paste(invalid_types, collapse = ', ')}",
        verbose = TRUE
      )
      data_types <- intersect(data_types, all_data_types)
    }
  }

  log_info(
    "Extracting data types: {paste(data_types, collapse = ', ')}",
    verbose = verbose
  )

  # extract data for each type
  result_list <- list()

  for (data_type in data_types) {
    log_info("Processing {data_type}...", verbose = verbose)

    tryCatch(
      {
        # handle epoch-specific data types
        if (
          data_type %in% c("epochs", "confounds_events", "confounds_summary")
        ) {
          if (is.null(epoch_labels)) {
            # get all available epoch labels for this data type
            type_tables <- all_tables[grepl(
              paste0("^", data_type, "_"),
              all_tables
            )]
            if (length(type_tables) > 0) {
              # extract unique epoch labels from table names
              # handles both eye-L/eye-R and eyeL/eyeR formats
              epoch_pattern <- paste0(
                data_type,
                "_[^_]+_[^_]+_[^_]+_[^_]+_(.+?)(?:_eye[LR]|_eye-[LR])?$"
              )
              extracted_labels <- unique(gsub(
                epoch_pattern,
                "\\1",
                type_tables
              ))
              # remove failed matches (when pattern doesn't match, return original string)
              extracted_labels <- extracted_labels[
                extracted_labels != type_tables &
                  !is.na(extracted_labels) &
                  extracted_labels != ""
              ]
              epoch_labels_to_use <- if (length(extracted_labels) > 0) {
                extracted_labels
              } else {
                NULL
              }
            } else {
              epoch_labels_to_use <- NULL
            }
          } else {
            epoch_labels_to_use <- epoch_labels
          }

          # extract data for each epoch label
          epoch_data_list <- list()
          if (!is.null(epoch_labels_to_use)) {
            for (epoch_label in epoch_labels_to_use) {
              epoch_data <- eyeris_db_read(
                con = con,
                data_type = data_type,
                subject = subjects,
                session = sessions,
                task = tasks,
                epoch_label = epoch_label,
                eye_suffix = eye_suffixes
              )
              if (!is.null(epoch_data) && nrow(epoch_data) > 0) {
                epoch_data_list[[epoch_label]] <- epoch_data
              }
            }
          }

          # combine all epoch data
          if (length(epoch_data_list) > 0) {
            combined_data <- do.call(rbind, epoch_data_list)
            result_list[[data_type]] <- combined_data
          }
          # check if there are any tables for this data type at all
          type_tables <- all_tables[grepl(
            paste0("^", data_type, "_"),
            all_tables
          )]
          if (length(type_tables) == 0) {
            log_warn(
              "No tables found for data type: {data_type}",
              verbose = verbose
            )
          }
        } else {
          # handle non-epoch data types
          data <- eyeris_db_read(
            con = con,
            data_type = data_type,
            subject = subjects,
            session = sessions,
            task = tasks,
            eye_suffix = eye_suffixes
          )

          if (!is.null(data) && nrow(data) > 0) {
            result_list[[data_type]] <- data
          }
          # check if there are any tables for this data type at all
          type_tables <- all_tables[grepl(
            paste0("^", data_type, "_"),
            all_tables
          )]
          if (length(type_tables) == 0) {
            log_warn(
              "No tables found for data type: {data_type}",
              verbose = verbose
            )
          }
        }
      },
      error = function(e) {
        log_warn(
          "Failed to extract {data_type}: {e$message}",
          verbose = verbose
        )
      }
    )
  }

  # filter out empty results
  result_list <- result_list[lengths(result_list) > 0]

  log_success(
    "Successfully extracted {length(result_list)} data types",
    verbose = verbose
  )
  for (dtype in names(result_list)) {
    n_rows <- nrow(result_list[[dtype]])
    n_subjects <- length(unique(result_list[[dtype]]$subject_id))
    log_info(
      "  {dtype}: {n_rows} rows across {n_subjects} subjects",
      verbose = verbose
    )
  }

  return(result_list)
}

#' Get summary statistics for eyeris database
#'
#' Provides a quick overview of the contents of an `eyeris` database,
#' including available subjects, sessions, tasks, and data types.
#'
#' @param bids_dir Path to the BIDS directory containing the database
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#' @param verbose Logical. Whether to print detailed output (default TRUE)
#'
#' @return A named list containing summary information about the database contents
#'
#' @examples
#' \donttest{
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
#'     bids_dir = file.path(tempdir(), "my-cool-memory-project"),
#'     participant_id = "001",
#'     session_num = "01",
#'     task_name = "assocret",
#'     run_num = "03", # override default run-01 (block_1) to use run-03 instead
#'     db_enabled = TRUE,
#'     db_path = "my-cool-memory-study",
#'   )
#'
#' # get database summary
#' summary <- eyeris_db_summary(
#'              file.path(
#'                tempdir(),
#'                "my-cool-memory-project"
#'              ),
#'              db_path = "my-cool-memory-study"
#'            )
#'
#' # view available subjects
#' summary$subjects
#'
#' # view available data types
#' summary$data_types
#'
#' # view table counts
#' summary$table_counts
#' }
#'
#' @export
eyeris_db_summary <- function(
  bids_dir,
  db_path = "my-project",
  verbose = TRUE
) {
  # connect to database
  log_info("Connecting to eyeris database...", verbose = verbose)

  con <- tryCatch(
    {
      eyeris_db_connect(bids_dir, db_path)
    },
    error = function(e) {
      log_error("Failed to connect to database: {e$message}")
    }
  )

  # ensure disconnection on exit
  on.exit(eyeris_db_disconnect(con))

  # get all tables
  all_tables <- eyeris_db_list_tables(con)

  if (length(all_tables) == 0) {
    log_warn("No tables found in database", verbose = TRUE)
    return(list(
      subjects = character(0),
      sessions = character(0),
      tasks = character(0),
      data_types = character(0),
      eye_suffixes = character(0),
      table_counts = integer(0),
      total_tables = 0
    ))
  }

  # parse table names to extract metadata
  # table name format: datatype_subject_session_task_run[_epochlabel][_eyesuffix]

  # extract data types properly - handle compound types like "confounds_events"
  # filter out temp tables first
  all_tables <- all_tables[!grepl("^temp_", all_tables)]

  data_types <- character(0)
  known_compound_types <- c(
    "confounds_events",
    "confounds_summary",
    "epoch_summary",
    "epoch_timeseries"
  )

  for (table in all_tables) {
    # check for compound types first
    matched_type <- NULL
    for (compound_type in known_compound_types) {
      if (grepl(paste0("^", compound_type, "_\\d+"), table)) {
        matched_type <- compound_type
        break
      }
    }

    # if no compound type matched, use first part before number
    if (is.null(matched_type)) {
      # match pattern: datatype_NUMBER or datatype_more_parts_NUMBER
      matched_type <- gsub("^(.+?)_\\d+.*", "\\1", table)
    }

    if (!is.null(matched_type) && matched_type != table) {
      data_types <- c(data_types, matched_type)
    }
  }
  data_types <- unique(data_types)

  # extract subjects, sessions, tasks from TIMESERIES tables only for consistency
  timeseries_tables <- all_tables[grepl("^timeseries_", all_tables)]
  subjects <- character(0)
  sessions <- character(0)
  tasks <- character(0)
  eye_suffixes <- character(0)
  table_counts <- integer(0)

  for (table in timeseries_tables) {
    # parse: timeseries_SUBJECT_SESSION_TASK_runXX
    parts <- strsplit(table, "_")[[1]]

    if (length(parts) >= 5) {
      # timeseries_subject_session_task_runXX
      subject_num <- parts[2]
      session_part <- parts[3]
      task_part <- parts[4]

      # standardize subject format
      SUBJECT_NUM_PATTERN <- "^\\d+$"

      if (grepl(SUBJECT_NUM_PATTERN, subject_num)) {
        subject_id <- paste0("sub-", sprintf("%02d", as.numeric(subject_num)))
        subjects <- c(subjects, subject_id)
      }

      # standardize session format
      session_id <- paste0("ses-", session_part)
      sessions <- c(sessions, session_id)

      # task name
      tasks <- c(tasks, task_part)
    }
  }

  # extract epoch labels from epochs tables
  epoch_tables <- all_tables[grepl("^epochs_", all_tables)]
  epoch_labels <- character(0)

  for (table in epoch_tables) {
    # parse: epochs_SUBJECT_SESSION_TASK_runXX_EPOCHLABEL
    parts <- strsplit(table, "_")[[1]]

    if (length(parts) >= 6) {
      # epochs_subject_session_task_runXX_epochlabel
      epoch_label <- parts[6]
      # check for additional parts (might have eye suffix)
      if (length(parts) > 6) {
        # check if last part is eye suffix
        last_part <- parts[length(parts)]
        if (!grepl("^eye[LR]$|^eye-[LR]$", last_part)) {
          # include additional parts in epoch label
          epoch_label <- paste(parts[6:length(parts)], collapse = "_")
        }
      }
      epoch_labels <- c(epoch_labels, epoch_label)
    }
  }

  # get row counts for each table
  for (table in all_tables) {
    tryCatch(
      {
        count <- DBI::dbGetQuery(
          con,
          paste0("SELECT COUNT(*) as n FROM \"", table, "\"")
        )$n
        table_counts[table] <- count
      },
      error = function(e) {
        table_counts[table] <- NA
      }
    )
  }

  # remove duplicates and NAs
  subjects <- sort(unique(subjects[!is.na(subjects)]))
  sessions <- sort(unique(sessions[!is.na(sessions)]))
  tasks <- sort(unique(tasks[!is.na(tasks)]))
  eye_suffixes <- sort(unique(eye_suffixes[!is.na(eye_suffixes)]))
  epoch_labels <- sort(unique(epoch_labels[!is.na(epoch_labels)]))

  result <- list(
    subjects = subjects,
    sessions = sessions,
    tasks = tasks,
    data_types = sort(data_types),
    eye_suffixes = eye_suffixes,
    epoch_labels = epoch_labels,
    table_counts = table_counts,
    total_tables = length(all_tables)
  )

  log_success("Database summary:", verbose = verbose)
  log_info("  Total tables: {result$total_tables}", verbose = verbose)
  log_info(
    "  Subjects: {length(result$subjects)} ({paste(head(result$subjects, 5), collapse = ', ')}{if(length(result$subjects) > 5) '...' else ''})",
    verbose = verbose
  )
  log_info(
    "  Sessions: {length(result$sessions)} ({paste(result$sessions, collapse = ', ')})",
    verbose = verbose
  )
  log_info(
    "  Tasks: {length(result$tasks)} ({paste(result$tasks, collapse = ', ')})",
    verbose = verbose
  )
  log_info(
    "  Data types: {length(result$data_types)} ({paste(result$data_types, collapse = ', ')})",
    verbose = verbose
  )
  if (length(result$eye_suffixes) > 0) {
    log_info(
      "  Eye suffixes: {length(result$eye_suffixes)} ({paste(result$eye_suffixes, collapse = ', ')})",
      verbose = verbose
    )
  }
  if (length(result$epoch_labels) > 0) {
    log_info(
      "  Epoch labels: {length(result$epoch_labels)} ({paste(result$epoch_labels, collapse = ', ')})",
      verbose = verbose
    )
  }

  total_rows <- sum(table_counts, na.rm = TRUE)
  log_info("  Total rows: {total_rows}", verbose = verbose)

  return(result)
}

#' Create temporary database for parallel processing
#'
#' Creates a unique temporary database for use in parallel jobs to avoid
#' concurrency issues. The temporary database is named using process ID
#' and timestamp to ensure uniqueness.
#'
#' @param bids_dir Path to the BIDS directory containing derivatives
#' @param base_db_path Base database name (e.g., "my-project")
#' @param verbose Whether to print verbose output
#'
#' @return List containing database connection and temp database path
#'
#' @keywords internal
create_temp_eyeris_database <- function(
  bids_dir,
  base_db_path = "my-project",
  verbose = FALSE
) {
  derivatives_dir <- file.path(bids_dir, "derivatives")
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
  }

  # create unique temp database name using process ID and timestamp
  # Use a portable timestamp: YYYYMMDD_HHMMSS_mmm (mmm = milliseconds)
  now <- Sys.time()
  timestamp <- format(now, "%Y%m%d_%H%M%S")
  millis <- sprintf("%03d", as.integer((as.numeric(now) %% 1) * 1000))
  temp_suffix <- paste0("_temp_", Sys.getpid(), "_", timestamp, "_", millis)

  # auto-append .eyerisdb extension if not present
  if (!grepl("\\.eyerisdb$", base_db_path)) {
    base_db_path <- paste0(base_db_path, ".eyerisdb")
  }

  # create temp database name
  temp_db_name <- gsub(
    "\\.eyerisdb$",
    paste0(temp_suffix, ".eyerisdb"),
    base_db_path
  )

  if (dirname(temp_db_name) == ".") {
    temp_db_path <- file.path(derivatives_dir, temp_db_name)
  } else {
    temp_db_path <- temp_db_name
  }

  tryCatch(
    {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db_path)

      log_success(
        "Created temporary database: {temp_db_path}",
        verbose = verbose
      )

      return(list(
        connection = con,
        temp_path = temp_db_path,
        base_path = gsub(
          paste0(temp_suffix, "\\.eyerisdb"),
          ".eyerisdb",
          temp_db_path
        )
      ))
    },
    error = function(e) {
      log_warn(
        "Failed to create temporary database: {e$message}",
        verbose = TRUE
      )
      return(NULL)
    }
  )
}

#' Merge temporary database into main database
#'
#' Safely merges data from a temporary database into the main project database
#' using file locking to prevent concurrent access issues. This function handles
#' the transactional copying of all tables from the temporary database.
#'
#' @param temp_db_info List containing temp database connection and paths
#' @param verbose Whether to print verbose output
#' @param max_retries Maximum number of retry attempts for file locking
#' @param retry_delay Delay between retry attempts in seconds
#'
#' @return Logical indicating success
#'
#' @keywords internal
merge_temp_database <- function(
  temp_db_info,
  verbose = FALSE,
  max_retries = 10,
  retry_delay = 1
) {
  if (is.null(temp_db_info) || is.null(temp_db_info$connection)) {
    log_warn("Invalid temporary database info provided", verbose = verbose)
    return(FALSE)
  }

  temp_con <- temp_db_info$connection
  temp_path <- temp_db_info$temp_path
  main_path <- temp_db_info$base_path

  # get all tables from temp database
  temp_tables <- DBI::dbListTables(temp_con)

  if (length(temp_tables) == 0) {
    log_info("No tables to merge from temporary database", verbose = verbose)
    # still consider this successful - just cleanup temp db
    disconnect_eyeris_database(temp_con, verbose = FALSE)
    if (file.exists(temp_path)) {
      unlink(temp_path)
    }
    return(TRUE)
  }

  log_info(
    "Merging {length(temp_tables)} tables from temporary database to main database",
    verbose = verbose
  )

  # file-based locking mechanism for safe concurrent access
  lock_file <- paste0(main_path, ".lock")

  # attempt to acquire lock with retries
  lock_acquired <- FALSE
  for (attempt in 1:max_retries) {
    tryCatch(
      {
        # try to create lock file (fails if already exists)
        if (!file.exists(lock_file)) {
          # write process info to lock file
          writeLines(
            paste("PID:", Sys.getpid(), "Time:", Sys.time()),
            lock_file
          )
          lock_acquired <- TRUE
          break
        } else {
          # check if lock is stale (older than 5 minutes)
          lock_age <- difftime(
            Sys.time(),
            file.info(lock_file)$mtime,
            units = "mins"
          )
          if (lock_age > 5) {
            log_warn(
              "Removing stale lock file (age: {round(lock_age, 1)} minutes)",
              verbose = verbose
            )
            unlink(lock_file)
            next # try again on next iteration
          }
        }
      },
      error = function(e) {
        # lock creation failed, continue to retry
      }
    )

    if (attempt < max_retries) {
      log_info(
        "Database locked, retrying in {retry_delay}s (attempt {attempt}/{max_retries})",
        verbose = verbose
      )
      Sys.sleep(retry_delay)
    }
  }

  if (!lock_acquired) {
    log_warn(
      "Could not acquire database lock after {max_retries} attempts",
      verbose = verbose
    )
    return(FALSE)
  }

  # ensure lock is removed on exit
  on.exit({
    if (file.exists(lock_file)) {
      unlink(lock_file)
    }
  })

  tryCatch(
    {
      # connect to main database
      main_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = main_path)
      on.exit(DBI::dbDisconnect(main_con), add = TRUE)

      # merge each table
      success_count <- 0
      for (table_name in temp_tables) {
        tryCatch(
          {
            # read data from temp database
            temp_data <- DBI::dbReadTable(temp_con, table_name)

            if (nrow(temp_data) == 0) {
              log_info("Skipping empty table: {table_name}", verbose = verbose)
              next
            }

            # check if table exists in main database
            main_tables <- DBI::dbListTables(main_con)

            if (table_name %in% main_tables) {
              # append to existing table
              DBI::dbWriteTable(
                conn = main_con,
                name = table_name,
                value = temp_data,
                append = TRUE,
                overwrite = FALSE
              )
              log_info(
                "Merged {nrow(temp_data)} rows into existing table '{table_name}'",
                verbose = verbose
              )
            } else {
              # create new table
              DBI::dbWriteTable(
                conn = main_con,
                name = table_name,
                value = temp_data,
                append = FALSE,
                overwrite = FALSE
              )
              log_info(
                "Created new table '{table_name}' with {nrow(temp_data)} rows",
                verbose = verbose
              )
            }

            success_count <- success_count + 1
          },
          error = function(e) {
            log_warn(
              "Failed to merge table '{table_name}': {e$message}",
              verbose = verbose
            )
          }
        )
      }

      log_success(
        "Successfully merged {success_count}/{length(temp_tables)} tables",
        verbose = verbose
      )

      return(success_count == length(temp_tables))
    },
    error = function(e) {
      log_warn("Error during database merge: {e$message}", verbose = verbose)
      return(FALSE)
    }
  )
}

#' Process large database query in chunks
#'
#' Handles really large databases by processing queries in reasonably sized chunks
#' to avoid memory issues. Data can be written to CSV or Parquet files as it's processed.
#'
#' @param con Database connection
#' @param query SQL query string to execute
#' @param chunk_size Number of rows to fetch per chunk (default: 1000000)
#' @param output_file Optional output file path for writing chunks. If provided,
#'   chunks will be appended to this file. File format determined by extension (.csv or .parquet)
#' @param process_chunk Optional function to process each chunk.
#'   Function should accept a data.frame and return logical indicating success.
#'   If not provided and output_file is specified, chunks are written to file.
#' @param verbose Whether to print progress messages (default: TRUE)
#'
#' @return List containing summary information about the chunked processing
#'
#' @examples
#' \dontrun{
#' # These examples require an existing eyeris database
#'
#' con <- eyeris_db_connect("/path/to/bids", "my-project")
#'
#' # Process large query and write to CSV
#' process_chunked_query(
#'   con,
#'   "SELECT * FROM large_table WHERE condition = 'something'",
#'   chunk_size = 50000,
#'   output_file = "large_export.csv"
#' )
#'
#' # Process large query with custom chunk processing
#' process_chunked_query(
#'   con,
#'   "SELECT * FROM large_table",
#'   chunk_size = 25000,
#'   process_chunk = function(chunk) {
#'     # Custom processing here
#'     processed_data <- some_analysis(chunk)
#'     return(TRUE)
#'   }
#' )
#'
#' eyeris_db_disconnect(con)
#' }
#'
#' @export
process_chunked_query <- function(
  con,
  query,
  chunk_size = 1000000,
  output_file = NULL,
  process_chunk = NULL,
  verbose = TRUE
) {
  if (is.null(con)) {
    log_error("Database connection is required")
  }

  if (is.null(query) || !is.character(query) || nchar(query) == 0) {
    log_error("Valid SQL query string is required")
  }

  if (chunk_size < 1) {
    log_error("chunk_size must be at least 1")
  }

  log_info(
    "Starting chunked query processing (chunk size: {chunk_size})",
    verbose = verbose
  )
  log_info("Query: {query}", verbose = verbose)

  # send query and prepare for chunked fetching
  res <- tryCatch(
    {
      DBI::dbSendQuery(con, query)
    },
    error = function(e) {
      log_error("Failed to send query: {e$message}")
    }
  )

  on.exit(DBI::dbClearResult(res))

  # initialize tracking variables
  total_rows_processed <- 0
  chunk_count <- 0
  start_time <- Sys.time()
  first_chunk <- TRUE

  # determine output format if file is specified
  write_to_csv <- FALSE
  write_to_parquet <- FALSE
  if (!is.null(output_file)) {
    if (grepl("\\.csv$", output_file, ignore.case = TRUE)) {
      write_to_csv <- TRUE
    } else if (grepl("\\.parquet$", output_file, ignore.case = TRUE)) {
      write_to_parquet <- TRUE
    } else {
      log_warn(
        "Unknown file extension for {output_file}, defaulting to CSV",
        verbose = verbose
      )
      write_to_csv <- TRUE
    }
  }

  # process chunks
  repeat {
    chunk <- tryCatch(
      {
        DBI::dbFetch(res, n = chunk_size)
      },
      error = function(e) {
        log_error("Failed to fetch chunk: {e$message}")
      }
    )

    if (nrow(chunk) == 0) {
      log_info("No more data to process", verbose = verbose)
      break
    }

    chunk_count <- chunk_count + 1
    chunk_rows <- nrow(chunk)
    total_rows_processed <- total_rows_processed + chunk_rows

    log_info(
      "Processing chunk {chunk_count}: {chunk_rows} rows",
      verbose = verbose
    )

    # process chunk based on provided options
    chunk_success <- TRUE

    if (!is.null(process_chunk) && is.function(process_chunk)) {
      # Use custom chunk processing function
      chunk_success <- tryCatch(
        {
          process_chunk(chunk)
        },
        error = function(e) {
          log_warn(
            "Error in custom chunk processing: {e$message}",
            verbose = verbose
          )
          FALSE
        }
      )
    } else if (!is.null(output_file)) {
      # write chunk to file
      chunk_success <- tryCatch(
        {
          if (write_to_csv) {
            if (first_chunk) {
              # Create new CSV file with headers
              write.csv(chunk, output_file, row.names = FALSE)
              log_info("Created output file: {output_file}", verbose = verbose)
            } else {
              # append to existing CSV file without headers
              write.table(
                chunk,
                output_file,
                sep = ",",
                row.names = FALSE,
                col.names = FALSE,
                append = TRUE
              )
            }
          } else if (write_to_parquet) {
            if (first_chunk) {
              # create new parquet file
              if (requireNamespace("arrow", quietly = TRUE)) {
                arrow::write_parquet(chunk, output_file)
              } else {
                log_error(
                  "Arrow package required for Parquet output but not available"
                )
              }
              log_info("Created output file: {output_file}", verbose = verbose)
            } else {
              # append to existing parquet file
              if (requireNamespace("arrow", quietly = TRUE)) {
                tryCatch(
                  {
                    existing_data <- arrow::read_parquet(output_file)
                    # ensure compatible column types for windows compatibility
                    # convert both to base R data.frames to ensure compatibility
                    existing_df <- as.data.frame(existing_data)
                    chunk_df <- as.data.frame(chunk)
                    combined_data <- rbind(existing_df, chunk_df)
                    arrow::write_parquet(combined_data, output_file)
                  },
                  error = function(e) {
                    # create separate file for this chunk to preserve data integrity
                    chunk_file <- gsub(
                      "\\.parquet$",
                      paste0("_chunk_", chunk_count, ".parquet"),
                      output_file
                    )
                    arrow::write_parquet(chunk, chunk_file)
                    log_warn(
                      "failed to append to main parquet file, saved chunk to separate file: {basename(chunk_file)}",
                      verbose = verbose
                    )
                    log_warn(
                      "you may need to manually combine: {basename(output_file)} and {basename(chunk_file)}",
                      verbose = verbose
                    )
                  }
                )
              } else {
                log_error(
                  "arrow package required for parquet output but not available"
                )
              }
            }
          }
          TRUE
        },
        error = function(e) {
          log_warn(
            "Failed to write chunk to {output_file}: {e$message}",
            verbose = verbose
          )
          FALSE
        }
      )
    }

    if (!chunk_success) {
      log_warn(
        "Chunk {chunk_count} processing failed, but continuing...",
        verbose = verbose
      )
    }

    first_chunk <- FALSE

    # progress update for large datasets
    if (chunk_count %% 10 == 0) {
      elapsed_time <- as.numeric(difftime(
        Sys.time(),
        start_time,
        units = "secs"
      ))
      rate <- total_rows_processed / elapsed_time
      log_info(
        "Progress: {chunk_count} chunks, {total_rows_processed} rows ({round(rate, 0)} rows/sec)",
        verbose = verbose
      )
    }
  }

  # final summary
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  result <- list(
    total_rows = total_rows_processed,
    chunks_processed = chunk_count,
    chunk_size = chunk_size,
    processing_time_seconds = total_time,
    output_file = output_file,
    query = query
  )

  log_success("Chunked processing complete:", verbose = verbose)
  log_info("  Total rows: {total_rows_processed}", verbose = verbose)
  log_info("  Chunks: {chunk_count}", verbose = verbose)
  log_info(
    "  Processing time: {round(total_time, 1)} seconds",
    verbose = verbose
  )
  log_info(
    "  Average rate: {round(total_rows_processed / total_time, 0)} rows/second",
    verbose = verbose
  )

  if (!is.null(output_file)) {
    log_info("  Output written to: {output_file}", verbose = verbose)
  }

  return(result)
}

#' Export eyeris database to chunked files
#'
#' High-level wrapper function to export large eyeris databases to chunked CSV or Parquet files
#' by data type. Uses chunked processing to handle very large datasets without memory issues.
#'
#' @param bids_dir Path to the BIDS directory containing the database
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#' @param output_dir Directory to save output files (defaults to bids_dir/derivatives/eyerisdb_export)
#' @param chunk_size Number of rows to process per chunk (default: 1000000)
#' @param file_format Output format: "csv" or "parquet" (default: "csv")
#' @param data_types Vector of data types to export. If NULL (default), exports all available
#' @param subjects Vector of subject IDs to include. If NULL (default), includes all subjects
#' @param max_file_size_mb Maximum file size in MB per output file (default: 50). When exceeded,
#'   automatically creates numbered files (e.g., data_01-of-03.csv, data_02-of-03.csv)
#' @param verbose Whether to print progress messages (default: TRUE)
#'
#' @return List containing information about exported files
#'
#' @examples
#' \dontrun{
#' # These examples require an existing eyeris database
#'
#' # Export entire database to CSV files
#' if (file.exists(file.path(tempdir(), "derivatives", "large-project.eyerisdb"))) {
#'   export_info <- eyeris_db_to_chunked_files(
#'     bids_dir = tempdir(),
#'     db_path = "large-project",
#'     chunk_size = 50000,
#'     file_format = "csv"
#'   )
#' }
#'
#' # Export specific data types to Parquet
#' if (file.exists(file.path(tempdir(), "derivatives", "large-project.eyerisdb"))) {
#'   export_info <- eyeris_db_to_chunked_files(
#'     bids_dir = tempdir(),
#'     db_path = "large-project",
#'     data_types = c("timeseries", "events"),
#'     file_format = "parquet",
#'     chunk_size = 75000
#'   )
#' }
#' }
#'
#' @export
eyeris_db_to_chunked_files <- function(
  bids_dir,
  db_path = "my-project",
  output_dir = NULL,
  chunk_size = 1000000,
  file_format = "csv",
  data_types = NULL,
  subjects = NULL,
  max_file_size_mb = 50,
  verbose = TRUE
) {
  # validate inputs
  if (!dir.exists(bids_dir)) {
    log_error("BIDS directory does not exist: {bids_dir}")
  }

  if (!file_format %in% c("csv", "parquet")) {
    log_error("file_format must be 'csv' or 'parquet'")
  }

  if (file_format == "parquet" && !requireNamespace("arrow", quietly = TRUE)) {
    log_error("Arrow package is required for Parquet output but not available")
  }

  # setup output directory
  db_name <- gsub("\\.eyerisdb$", "", basename(db_path))
  if (is.null(output_dir)) {
    output_dir <- file.path(bids_dir, "derivatives", "eyerisdb_export", db_name)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    log_info("Created output directory: {output_dir}", verbose = verbose)
  }

  con <- tryCatch(
    {
      eyeris_db_connect(bids_dir, db_path)
    },
    error = function(e) {
      log_error("Failed to connect to database: {e$message}")
    }
  )

  on.exit(eyeris_db_disconnect(con))

  # get available data types
  all_tables <- eyeris_db_list_tables(con)

  # filter out temporary tables to prevent contamination
  temp_tables <- all_tables[grepl("^temp_", all_tables)]
  if (length(temp_tables) > 0) {
    log_warn(
      "Found {length(temp_tables)} temporary tables in database - these will be EXCLUDED from export",
      verbose = verbose
    )
  }
  all_tables <- all_tables[!grepl("^temp_", all_tables)]

  if (length(all_tables) == 0) {
    log_warn(
      "No valid tables found in database (after excluding temp tables)",
      verbose = verbose
    )
    return(list(files = character(0), total_rows = 0))
  }

  # group tables by data type (handle compound data types like "confounds_summary")
  available_data_types <- c()
  known_data_types <- c(
    "timeseries",
    "epochs",
    "epoch_summary",
    "epoch_timeseries",
    "events",
    "blinks",
    "confounds_events",
    "confounds_summary",
    "run_confounds"
  )

  for (table in all_tables) {
    # try to match known compound data types first
    matched_type <- NULL
    for (known_type in known_data_types) {
      if (grepl(paste0("^", known_type, "_"), table)) {
        matched_type <- known_type
        break
      }
    }

    # fall back to first part before underscore if no known type matched
    if (is.null(matched_type)) {
      matched_type <- gsub("^([^_]+)_.*", "\\1", table)
    }

    available_data_types <- c(available_data_types, matched_type)
  }

  available_data_types <- unique(available_data_types)

  if (is.null(data_types)) {
    data_types <- available_data_types
  } else {
    # Validate requested data types
    invalid_types <- setdiff(data_types, available_data_types)
    if (length(invalid_types) > 0) {
      log_warn(
        "Invalid data types ignored: {paste(invalid_types, collapse = ', ')}",
        verbose = verbose
      )
      data_types <- intersect(data_types, available_data_types)
    }
  }

  if (length(data_types) == 0) {
    log_warn("No valid data types to export", verbose = verbose)
    return(list(files = character(0), total_rows = 0))
  }

  log_info(
    "Exporting data types: {paste(data_types, collapse = ', ')}",
    verbose = verbose
  )

  # export each data type
  exported_files <- list()
  total_rows_exported <- 0

  for (data_type in data_types) {
    log_info("Processing data type: {data_type}", verbose = verbose)

    # get all tables for this data type (handle compound types correctly)
    if (
      data_type %in%
        c(
          "confounds_events",
          "confounds_summary",
          "epoch_summary",
          "epoch_timeseries"
        )
    ) {
      # use exact match for compound data types
      type_tables <- all_tables[grepl(paste0("^", data_type, "_"), all_tables)]
    } else {
      # for simple data types, make sure we don't match compound types
      type_tables <- all_tables[
        grepl(paste0("^", data_type, "_"), all_tables) &
          !grepl(
            "^(confounds_events|confounds_summary|epoch_summary|epoch_timeseries)_",
            all_tables
          )
      ]
    }

    if (length(type_tables) == 0) {
      log_warn("No tables found for data type: {data_type}", verbose = verbose)
      next
    }

    # group tables by column structure (for epoch-related data types)
    epoch_related_types <- c(
      "epochs",
      "epoch_summary",
      "epoch_timeseries",
      "confounds_events",
      "confounds_summary"
    )

    if (data_type %in% epoch_related_types) {
      # group tables by their column structure to avoid UNION errors
      table_groups <- list()
      column_signatures <- list()

      log_info(
        "  Analyzing column structures to group compatible tables...",
        verbose = verbose
      )

      for (table in type_tables) {
        # get column names for this table
        tryCatch(
          {
            cols <- DBI::dbListFields(con, table)
            col_signature <- paste(sort(cols), collapse = "|")

            # find existing group with same signature or create new one
            group_found <- FALSE
            for (existing_sig in names(column_signatures)) {
              if (column_signatures[[existing_sig]] == col_signature) {
                table_groups[[existing_sig]] <- c(
                  table_groups[[existing_sig]],
                  table
                )
                group_found <- TRUE
                break
              }
            }

            if (!group_found) {
              # extract potential suffix from table name for grouping
              suffix <- gsub(".*_([^_]+)$", "\\1", table)
              # use suffix as group key, but ensure uniqueness
              group_key <- suffix
              counter <- 1
              while (group_key %in% names(column_signatures)) {
                group_key <- paste0(suffix, "_", counter)
                counter <- counter + 1
              }

              column_signatures[[group_key]] <- col_signature
              table_groups[[group_key]] <- c(table)
            }
          },
          error = function(e) {
            log_warn(
              "Could not analyze table structure for '{table}': {e$message}",
              verbose = verbose
            )
            # put in a separate group
            group_key <- paste0("error_", table)
            table_groups[[group_key]] <- c(table)
          }
        )
      }

      log_info(
        "Found {length(table_groups)} column-compatible groups for {data_type}: {paste(names(table_groups), collapse = ', ')}",
        verbose = verbose
      )
    } else {
      # non-epoch data types - process all together
      table_groups <- list("all" = type_tables)
    }

    # process each group separately
    for (group_name in names(table_groups)) {
      group_tables <- table_groups[[group_name]]

      # setup output file name
      file_ext <- if (file_format == "csv") ".csv" else ".parquet"

      if (length(table_groups) > 1) {
        # multiple groups - include group name in filename
        output_file <- file.path(
          output_dir,
          paste0(data_type, "_", group_name, "_chunked", file_ext)
        )
      } else {
        # single group - use simple filename
        output_file <- file.path(
          output_dir,
          paste0(data_type, "_chunked", file_ext)
        )
      }

      log_info(
        "  Processing {data_type} group '{group_name}' ({length(group_tables)} tables)",
        verbose = verbose
      )

      # process tables in batches to avoid SQL query length limits
      max_tables_per_query <- 50
      table_batches <- split(
        group_tables,
        ceiling(seq_along(group_tables) / max_tables_per_query)
      )

      total_group_rows <- 0
      total_group_chunks <- 0
      total_group_time <- 0

      # file size tracking variables
      current_file_number <- 1
      current_file_size_mb <- 0
      max_file_size_bytes <- max_file_size_mb * 1024 * 1024
      created_files <- list()
      is_first_write <- TRUE

      # get current output file name (will be updated for numbered files)
      get_current_output_file <- function(file_num, total_files = NULL) {
        base_name <- if (length(table_groups) > 1) {
          paste0(db_name, "_", data_type, "_", group_name, "_chunked")
        } else {
          paste0(db_name, "_", data_type, "_chunked")
        }

        file_ext <- if (file_format == "csv") ".csv" else ".parquet"

        if (is.null(total_files)) {
          # use temporary numbering during processing
          numbered_name <- paste0(
            base_name,
            sprintf("_%02d", file_num),
            file_ext
          )
        } else {
          # final numbering with total count
          numbered_name <- paste0(
            base_name,
            sprintf("_%02d-of-%02d", file_num, total_files),
            file_ext
          )
        }

        return(file.path(output_dir, numbered_name))
      }

      current_output_file <- get_current_output_file(current_file_number)

      for (batch_idx in seq_along(table_batches)) {
        batch_tables <- table_batches[[batch_idx]]

        if (length(table_batches) > 1) {
          log_info(
            "    Processing batch {batch_idx}/{length(table_batches)} ({length(batch_tables)} tables)",
            verbose = verbose
          )
        }

        # build UNION ALL query for this batch
        union_queries <- sapply(batch_tables, function(table) {
          paste0("SELECT * FROM \"", table, "\"")
        })

        batch_union_sql <- paste(union_queries, collapse = " UNION ALL ")

        # add subject filter if specified
        if (!is.null(subjects)) {
          subjects_str <- paste0("'", subjects, "'", collapse = ", ")
          batch_union_sql <- paste0(
            "SELECT * FROM (",
            batch_union_sql,
            ") WHERE subject_id IN (",
            subjects_str,
            ")"
          )
        }

        # custom chunk processor with file size limits
        chunk_processor <- function(chunk) {
          if (nrow(chunk) == 0) {
            return(TRUE)
          }

          tryCatch(
            {
              # calculate approximate size of this chunk
              chunk_size_bytes <- as.numeric(object.size(chunk))

              # check if adding this chunk would exceed file size limit
              if (
                !is_first_write &&
                  (current_file_size_mb * 1024 * 1024 + chunk_size_bytes) >
                    max_file_size_bytes
              ) {
                # start a new file
                current_file_number <<- current_file_number + 1
                current_output_file <<- get_current_output_file(
                  current_file_number
                )
                current_file_size_mb <<- 0
                is_first_write <<- TRUE

                log_info(
                  "    Starting new file due to size limit: {basename(current_output_file)}",
                  verbose = verbose
                )
              }

              if (file_format == "csv") {
                if (is_first_write) {
                  # create first chunk of this file with headers
                  write.csv(chunk, current_output_file, row.names = FALSE)
                  log_info(
                    "    Created output file: {basename(current_output_file)}",
                    verbose = verbose
                  )
                  is_first_write <<- FALSE
                } else {
                  # append subsequent chunks without headers
                  write.table(
                    chunk,
                    current_output_file,
                    sep = ",",
                    row.names = FALSE,
                    col.names = FALSE,
                    append = TRUE
                  )
                }
              } else if (file_format == "parquet") {
                if (is_first_write) {
                  # first chunk - create new parquet file
                  if (requireNamespace("arrow", quietly = TRUE)) {
                    arrow::write_parquet(chunk, current_output_file)
                    log_info(
                      "    Created output file: {basename(current_output_file)}",
                      verbose = verbose
                    )
                    is_first_write <<- FALSE
                  } else {
                    log_error(
                      "Arrow package required for Parquet output but not available"
                    )
                  }
                } else {
                  # append to existing parquet file
                  if (requireNamespace("arrow", quietly = TRUE)) {
                    tryCatch(
                      {
                        existing_data <- arrow::read_parquet(
                          current_output_file
                        )
                        # ensure compatible column types for windows compatibility
                        # convert both to base R data.frames to ensure compatibility
                        existing_df <- as.data.frame(existing_data)
                        chunk_df <- as.data.frame(chunk)
                        combined_data <- rbind(existing_df, chunk_df)
                        arrow::write_parquet(combined_data, current_output_file)
                      },
                      error = function(e) {
                        # create separate file for this chunk to preserve data integrity
                        chunk_file <- gsub(
                          "\\.parquet$",
                          paste0("_chunk_", total_group_chunks + 1, ".parquet"),
                          current_output_file
                        )
                        arrow::write_parquet(chunk, chunk_file)
                        log_warn(
                          "failed to append to main parquet file, saved chunk to separate file: {basename(chunk_file)}",
                          verbose = verbose
                        )
                        log_warn(
                          "you may need to manually combine: {basename(current_output_file)} and {basename(chunk_file)}",
                          verbose = verbose
                        )
                      }
                    )
                  } else {
                    log_error(
                      "Arrow package required for Parquet output but not available"
                    )
                  }
                }
              }

              # update file size tracking
              if (file.exists(current_output_file)) {
                current_file_size_mb <<- file.size(current_output_file) /
                  (1024 * 1024)
              }

              return(TRUE)
            },
            error = function(e) {
              log_warn("Failed to write chunk: {e$message}", verbose = verbose)
              return(FALSE)
            }
          )
        }

        # process this batch with chunking
        batch_result <- process_chunked_query(
          con = con,
          query = batch_union_sql,
          chunk_size = chunk_size,
          process_chunk = chunk_processor,
          verbose = verbose
        )

        total_group_rows <- total_group_rows + batch_result$total_rows
        total_group_chunks <- total_group_chunks + batch_result$chunks_processed
        total_group_time <- total_group_time +
          batch_result$processing_time_seconds
      }

      # rename files to include final total count if multiple files were created
      if (current_file_number > 1) {
        log_info(
          "  Finalizing {current_file_number} output files with proper numbering...",
          verbose = verbose
        )

        for (file_num in 1:current_file_number) {
          old_file <- get_current_output_file(file_num)
          new_file <- get_current_output_file(file_num, current_file_number)

          if (file.exists(old_file) && old_file != new_file) {
            file.rename(old_file, new_file)
            log_info(
              "    Renamed: {basename(old_file)} -> {basename(new_file)}",
              verbose = verbose
            )
          }

          if (file.exists(new_file)) {
            file_size_mb <- file.size(new_file) / (1024 * 1024)
            created_files[[paste0("file_", file_num)]] <- list(
              path = new_file,
              size_mb = file_size_mb
            )
          }
        }

        # store all created files info
        final_output_files <- sapply(created_files, function(x) x$path)
        total_size_mb <- sum(sapply(created_files, function(x) x$size_mb))
      } else {
        # single file case
        final_output_files <- current_output_file
        total_size_mb <- if (file.exists(current_output_file)) {
          file.size(current_output_file) / (1024 * 1024)
        } else {
          0
        }
      }

      if (total_group_rows > 0) {
        group_key <- if (length(table_groups) > 1) {
          paste0(data_type, "_", group_name)
        } else {
          data_type
        }

        if (current_file_number > 1) {
          # multiple files - store all file paths and info
          exported_files[[group_key]] <- list(
            files = final_output_files,
            total_files = current_file_number,
            rows = total_group_rows,
            chunks = total_group_chunks,
            processing_time = total_group_time,
            total_size_mb = total_size_mb
          )
        } else {
          # single file - maintain backward compatibility
          exported_files[[group_key]] <- list(
            file = final_output_files,
            rows = total_group_rows,
            chunks = total_group_chunks,
            processing_time = total_group_time,
            size_mb = total_size_mb
          )
        }

        total_rows_exported <- total_rows_exported + total_group_rows
      }
    }
  }

  # export summary
  log_success("Export complete!", verbose = verbose)
  log_info("  Data types exported: {length(exported_files)}", verbose = verbose)
  log_info("  Total rows exported: {total_rows_exported}", verbose = verbose)
  log_info("  Output directory: {output_dir}", verbose = verbose)

  return(list(
    files = exported_files,
    total_rows = total_rows_exported,
    output_dir = output_dir,
    file_format = file_format,
    chunk_size = chunk_size
  ))
}

#' Cleanup temporary database
#'
#' Safely disconnects and removes temporary database files after successful merge.
#'
#' @param temp_db_info List containing temp database connection and paths
#' @param verbose Whether to print verbose output
#'
#' @return Logical indicating success
#'
#' @keywords internal
cleanup_temp_database <- function(temp_db_info, verbose = FALSE) {
  if (is.null(temp_db_info)) {
    return(TRUE)
  }

  success <- TRUE

  # disconnect from temp database
  if (!is.null(temp_db_info$connection)) {
    tryCatch(
      {
        DBI::dbDisconnect(temp_db_info$connection)
        log_info("Disconnected from temporary database", verbose = verbose)
      },
      error = function(e) {
        log_warn(
          "Error disconnecting from temp database: {e$message}",
          verbose = verbose
        )
        success <- FALSE
      }
    )
  }

  # remove temp database file
  if (!is.null(temp_db_info$temp_path) && file.exists(temp_db_info$temp_path)) {
    tryCatch(
      {
        unlink(temp_db_info$temp_path)
        log_success("Cleaned up temporary database file", verbose = verbose)
      },
      error = function(e) {
        log_warn(
          "Error removing temp database file: {e$message}",
          verbose = verbose
        )
        success <- FALSE
      }
    )
  }

  return(success)
}
