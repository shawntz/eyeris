#' Create or connect to eyeris project database
#'
#' Creates a new DuckDB database for the eyeris project or connects to an existing one.
#' The database will be created in the BIDS derivatives directory.
#'
#' @param bids_dir Path to the BIDS directory containing derivatives
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#' @param verbose Whether to print verbose output
#'
#' @return DBI database connection object
#'
#' @keywords internal
connect_eyeris_database <- function(bids_dir, db_path = "my-project", verbose = FALSE) {
  derivatives_dir <- file.path(bids_dir, "derivatives")
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    if (verbose) {
      cli::cli_alert_info(glue::glue("[INFO] Created derivatives directory: {derivatives_dir}"), wrap = TRUE)
    }
  }

  # Auto-append .eyerisdb extension if not present
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

      if (verbose) {
        if (file.exists(full_db_path)) {
          cli::cli_alert_success(glue::glue("[OKAY] Connected to existing eyeris project database: {full_db_path}"), wrap = TRUE)
        } else {
          cli::cli_alert_success(glue::glue("[OKAY] Created new eyeris database: {full_db_path}"), wrap = TRUE)
        }
      }

      return(con)
    },
    error = function(e) {
      cli::cli_alert_warning(glue::glue("[WARN] Failed to connect to database: {e$message}"), wrap = TRUE)
      return(NULL)
    }
  )
}

#' Disconnect from eyeris database
#'
#' Safely disconnects from the eyeris project database.
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
      if (verbose) {
        cli::cli_alert_info("[INFO] Disconnected from eyeris database", wrap = TRUE)
      }
      return(TRUE)
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(glue::glue("[WARN] Error disconnecting from database: {e$message}"), wrap = TRUE)
      }
      return(FALSE)
    }
  )
}

#' Create table name for eyeris data
#'
#' Generates a standardized table name for eyeris data based on the data type
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
create_table_name <- function(data_type, sub, ses, task, run = NULL, eye_suffix = NULL, epoch_label = NULL) {
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
#' Writes eyeris data to the project database as an alternative to CSV files.
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
    if (verbose) {
      cli::cli_alert_warning("[WARN] No database connection provided", wrap = TRUE)
    }
    return(FALSE)
  }

  if (is.null(data) || nrow(data) == 0) {
    if (verbose) {
      cli::cli_alert_warning("[WARN] No data to write to database", wrap = TRUE)
    }
    return(FALSE)
  }

  table_name <- create_table_name(data_type, sub, ses, task, run, eye_suffix, epoch_label)

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

      if (verbose) {
        cli::cli_alert_info(
          glue::glue("[INFO] Writing {nrow(data)} rows to table '{table_name}' (append={actual_append})"),
          wrap = TRUE
        )
      }

      DBI::dbWriteTable(
        conn = con,
        name = table_name,
        value = data,
        append = actual_append,
        overwrite = !actual_append
      )

      if (verbose) {
        action <- if (append) "Added" else "Created"
        cli::cli_alert_success(
          glue::glue("[OKAY] {action} table '{table_name}' with {nrow(data)} rows"),
          wrap = TRUE
        )
      }

      return(TRUE)
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(
          glue::glue("[WARN] Failed to write data to table '{table_name}': {e$message}"),
          wrap = TRUE
        )
      }
      return(FALSE)
    }
  )
}

#' List available tables in eyeris database
#'
#' Lists all tables in the eyeris project database with optional filtering.
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
    cli::cli_alert_warning("[WARN] No database connection provided", wrap = TRUE)
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
      cli::cli_alert_warning(glue::glue("[WARN] Failed to list tables: {e$message}"), wrap = TRUE)
      return(character(0))
    }
  )
}

#' Read eyeris data from database
#'
#' Reads eyeris data from the project database with dplyr-style interface.
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
    cli::cli_abort("[EXIT] No database connection provided")
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
        cli::cli_alert_warning("[WARN] No tables found in database", wrap = TRUE)
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
        # Verify table actually exists before adding to query
        if (table %in% DBI::dbListTables(con)) {
          valid_tables <- c(valid_tables, table)
        }
      }

      if (length(valid_tables) == 0) {
        return(data.frame())
      }

      union_queries <- c()
      for (table in valid_tables) {
        union_queries <- c(union_queries, paste0("SELECT * FROM \"", table, "\""))
      }

      query <- paste(union_queries, collapse = " UNION ALL ")
      query <- paste0("SELECT * FROM (", query, ") as combined_data WHERE 1=1")

      if (!is.null(subject)) {
        query <- paste(query, "AND subject_id =", shQuote(subject))
      }

      if (!is.null(session)) {
        query <- paste(query, "AND session_id =", shQuote(session))
      }

      if (!is.null(task)) {
        query <- paste(query, "AND task_name =", shQuote(task))
      }

      if (!is.null(run)) {
        query <- paste(query, "AND run_number =", shQuote(run))
      }

      if (!is.null(eye_suffix)) {
        query <- paste(query, "AND eye_suffix =", shQuote(eye_suffix))
      }

      # Note: epoch_label filtering is handled by table selection above
      # No additional WHERE clause needed since tables are already filtered by epoch

      # execute query
      cli::cli_alert_info(
        glue::glue("[INFO] Executing query: {query}"), wrap = TRUE
      )
      result <- DBI::dbGetQuery(con, query)

      return(result)
    },
    error = function(e) {
      cli::cli_alert_warning(glue::glue("[WARN] Failed to read from database: {e$message}"), wrap = TRUE)
      return(data.frame())
    }
  )
}

#' Connect to eyeris project database (user-facing)
#'
#' User-friendly function to connect to an existing eyeris project database.
#' This function provides easy access for users to query their eyeris data.
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
    cli::cli_abort(
      "[EXIT] No eyeris database found at: {full_db_path}\\n
       Run bidsify() with db_enabled = TRUE to create a database first."
    )
  }

  tryCatch(
    {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = full_db_path)
      cli::cli_alert_success(glue::glue("[OKAY] Connected to eyeris database: {full_db_path}"), wrap = TRUE)
      return(con)
    },
    error = function(e) {
      cli::cli_abort("[EXIT] Failed to connect to database: %s", e$message)
    }
  )
}

#' Disconnect from eyeris database (user-facing)
#'
#' User-friendly function to disconnect from the eyeris project database.
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
        if (verbose) {
          cli::cli_alert_warning(glue::glue("[WARN] Failed to write CSV file: {csv_path}"), wrap = TRUE)
        }
        csv_success <- FALSE
      }
    )
  }

  if (!is.null(db_con) && !is.null(data_type) && !is.null(sub) && !is.null(ses) && !is.null(task)) {
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
    cli::cli_alert_success(
      glue::glue("[OKAY] Wrote {data_type_str} data ({nrow(data)} rows) to {output_str}"),
      wrap = TRUE
    )
  }

  return(csv_success && db_success)
}

#' Extract and aggregate eyeris data across subjects from database
#'
#' A comprehensive wrapper function that simplifies extracting eyeris data from
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
#' @param return_list Logical. If TRUE (default), returns a named list with one dataframe per data type.
#'   If FALSE, returns a single long-format dataframe with a 'data_type' column
#' @param verbose Logical. Whether to print progress messages (default TRUE)
#'
#' @return Either a named list of dataframes (one per data type) or a single combined dataframe,
#'   depending on the `return_list` parameter
#'
#' @examples
#' \donttest{
#' # Extract all data for all subjects (returns list of dataframes)
#' all_data <- eyeris_extract_data("~/my_bids_project")
#'
#' # View available data types
#' names(all_data)
#'
#' # Access specific data type
#' blinks_data <- all_data$blinks
#' epochs_data <- all_data$epochs
#'
#' # Extract specific subjects and data types
#' subset_data <- eyeris_extract_data(
#'   bids_dir = "~/my_bids_project",
#'   subjects = c("001", "002", "003"),
#'   data_types = c("blinks", "epochs", "timeseries")
#' )
#'
#' # Extract epoch data for specific epoch label
#' epoch_data <- eyeris_extract_data(
#'   bids_dir = "~/my_bids_project",
#'   data_types = "epochs",
#'   epoch_labels = "prepostprobe"
#' )
#'
#' # Return as single combined dataframe instead of list
#' combined_data <- eyeris_extract_data(
#'   bids_dir = "~/my_bids_project",
#'   return_list = FALSE
#' )
#' }
#'
#' @export
eyeris_extract_data <- function(
  bids_dir,
  db_path = "my-project",
  subjects = NULL,
  data_types = NULL,
  sessions = NULL,
  tasks = NULL,
  epoch_labels = NULL,
  eye_suffixes = NULL,
  return_list = TRUE,
  verbose = TRUE
) {

  # Connect to database
  if (verbose) {
    cli::cli_alert_info("[INFO] Connecting to eyeris database...")
  }

  con <- tryCatch({
    eyeris_db_connect(bids_dir, db_path)
  }, error = function(e) {
    cli::cli_abort("[EXIT] Failed to connect to database: {e$message}")
  })

  # Ensure disconnection on exit
  on.exit(eyeris_db_disconnect(con))

  # Get available tables
  all_tables <- eyeris_db_list_tables(con)

  if (length(all_tables) == 0) {
    cli::cli_alert_warning("[WARN] No tables found in database")
    return(if (return_list) list() else data.frame())
  }

  if (verbose) {
    cli::cli_alert_info("[INFO] Found {length(all_tables)} tables in database")
  }

  # Define all possible data types
  all_data_types <- c("blinks", "events", "timeseries", "epochs", "epoch_summary",
                     "run_confounds", "confounds_events", "confounds_summary")

  # Use all data types if none specified
  if (is.null(data_types)) {
    data_types <- all_data_types
  } else {
    # Validate specified data types
    invalid_types <- setdiff(data_types, all_data_types)
    if (length(invalid_types) > 0) {
      cli::cli_alert_warning("[WARN] Invalid data types ignored: {paste(invalid_types, collapse = ', ')}")
      data_types <- intersect(data_types, all_data_types)
    }
  }

  if (verbose) {
    cli::cli_alert_info("[INFO] Extracting data types: {paste(data_types, collapse = ', ')}")
  }

  # Extract data for each type
  result_list <- list()

  for (data_type in data_types) {
    if (verbose) {
      cli::cli_alert_info("[INFO] Processing {data_type}...")
    }

    tryCatch({
      # Handle epoch-specific data types
      if (data_type %in% c("epochs", "confounds_events", "confounds_summary")) {
        if (is.null(epoch_labels)) {
          # Get all available epoch labels for this data type
          type_tables <- all_tables[grepl(paste0("^", data_type, "_"), all_tables)]
          if (length(type_tables) > 0) {
            # Extract unique epoch labels from table names
            # Pattern handles both eye-L/eye-R and eyeL/eyeR formats
            epoch_pattern <- paste0(data_type, "_[^_]+_[^_]+_[^_]+_[^_]+_(.+?)(?:_eye[LR]|_eye-[LR])?$")
            extracted_labels <- unique(gsub(epoch_pattern, "\\1", type_tables))
            # Remove failed matches (when pattern doesn't match, returns original string)
            extracted_labels <- extracted_labels[extracted_labels != type_tables & !is.na(extracted_labels) & extracted_labels != ""]
            epoch_labels_to_use <- if (length(extracted_labels) > 0) extracted_labels else NULL
          } else {
            epoch_labels_to_use <- NULL
          }
        } else {
          epoch_labels_to_use <- epoch_labels
        }

        # Extract data for each epoch label
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

        # Combine all epoch data
        if (length(epoch_data_list) > 0) {
          combined_data <- do.call(rbind, epoch_data_list)
          result_list[[data_type]] <- combined_data
        } else if (verbose) {
          # Check if there are any tables for this data type at all
          type_tables <- all_tables[grepl(paste0("^", data_type, "_"), all_tables)]
          if (length(type_tables) == 0) {
            cli::cli_alert_warning("[WARN] No tables found for data type: {data_type}")
          }
        }

      } else {
        # Handle non-epoch data types
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
        } else if (verbose) {
          # Check if there are any tables for this data type at all
          type_tables <- all_tables[grepl(paste0("^", data_type, "_"), all_tables)]
          if (length(type_tables) == 0) {
            cli::cli_alert_warning("[WARN] No tables found for data type: {data_type}")
          }
        }
      }

    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("[WARN] Failed to extract {data_type}: {e$message}")
      }
    })
  }

  # Filter out empty results
  result_list <- result_list[lengths(result_list) > 0]

  if (verbose) {
    cli::cli_alert_success("[OKAY] Successfully extracted {length(result_list)} data types")
    for (dtype in names(result_list)) {
      n_rows <- nrow(result_list[[dtype]])
      n_subjects <- length(unique(result_list[[dtype]]$subject_id))
      cli::cli_alert_info("  {dtype}: {n_rows} rows across {n_subjects} subjects")
    }
  }

  # Return format based on user preference
  if (return_list) {
    return(result_list)
  } else {
    # Combine into single dataframe with data_type column
    if (length(result_list) == 0) {
      return(data.frame())
    }

    combined_list <- list()
    for (dtype in names(result_list)) {
      df <- result_list[[dtype]]
      df$data_type <- dtype
      combined_list[[dtype]] <- df
    }

    # Combine all dataframes
    combined_df <- do.call(rbind, combined_list)
    rownames(combined_df) <- NULL

    return(combined_df)
  }
}

#' Get summary statistics for eyeris database
#'
#' Provides a quick overview of the contents of an eyeris database,
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
#' # Get database summary
#' summary <- eyeris_db_summary("~/my_bids_project")
#'
#' # View available subjects
#' summary$subjects
#'
#' # View available data types
#' summary$data_types
#'
#' # View table counts
#' summary$table_counts
#' }
#'
#' @export
eyeris_db_summary <- function(bids_dir, db_path = "my-project", verbose = TRUE) {

  # Connect to database
  if (verbose) {
    cli::cli_alert_info("[INFO] Connecting to eyeris database...")
  }

  con <- tryCatch({
    eyeris_db_connect(bids_dir, db_path)
  }, error = function(e) {
    cli::cli_abort("[EXIT] Failed to connect to database: {e$message}")
  })

  # Ensure disconnection on exit
  on.exit(eyeris_db_disconnect(con))

  # Get all tables
  all_tables <- eyeris_db_list_tables(con)

  if (length(all_tables) == 0) {
    cli::cli_alert_warning("[WARN] No tables found in database")
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

  # Parse table names to extract metadata
  # Table name format: datatype_subject_session_task_run[_epochlabel][_eyesuffix]

  # Extract data types
  data_types <- unique(gsub("^([^_]+)_.*", "\\1", all_tables))

  # Get unique subjects, sessions, tasks by querying a sample of tables
  subjects <- character(0)
  sessions <- character(0)
  tasks <- character(0)
  eye_suffixes <- character(0)
  table_counts <- integer(0)

  # Sample a few tables to get metadata (avoid querying every table for performance)
  sample_tables <- head(all_tables, min(10, length(all_tables)))

  for (table in sample_tables) {
    tryCatch({
      # Query just one row to get metadata
      sample_data <- DBI::dbGetQuery(con, paste0("SELECT * FROM \"", table, "\" LIMIT 1"))
      if (nrow(sample_data) > 0) {
        if ("subject_id" %in% colnames(sample_data)) {
          subjects <- c(subjects, sample_data$subject_id)
        }
        if ("session_id" %in% colnames(sample_data)) {
          sessions <- c(sessions, sample_data$session_id)
        }
        if ("task_name" %in% colnames(sample_data)) {
          tasks <- c(tasks, sample_data$task_name)
        }
        if ("eye_suffix" %in% colnames(sample_data)) {
          eye_suffixes <- c(eye_suffixes, sample_data$eye_suffix)
        }
      }
    }, error = function(e) {
      # Skip tables that can't be queried
    })
  }

  # Get row counts for each table
  for (table in all_tables) {
    tryCatch({
      count <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) as n FROM \"", table, "\""))$n
      table_counts[table] <- count
    }, error = function(e) {
      table_counts[table] <- NA
    })
  }

  # Remove duplicates and NAs
  subjects <- unique(subjects[!is.na(subjects)])
  sessions <- unique(sessions[!is.na(sessions)])
  tasks <- unique(tasks[!is.na(tasks)])
  eye_suffixes <- unique(eye_suffixes[!is.na(eye_suffixes)])

  result <- list(
    subjects = sort(subjects),
    sessions = sort(sessions),
    tasks = sort(tasks),
    data_types = sort(data_types),
    eye_suffixes = sort(eye_suffixes),
    table_counts = table_counts,
    total_tables = length(all_tables)
  )

  if (verbose) {
    cli::cli_alert_success("[OKAY] Database summary:")
    cli::cli_alert_info("  Total tables: {result$total_tables}")
    cli::cli_alert_info("  Subjects: {length(result$subjects)} ({paste(head(result$subjects, 5), collapse = ', ')}{if(length(result$subjects) > 5) '...' else ''})")
    cli::cli_alert_info("  Sessions: {length(result$sessions)} ({paste(result$sessions, collapse = ', ')})")
    cli::cli_alert_info("  Tasks: {length(result$tasks)} ({paste(result$tasks, collapse = ', ')})")
    cli::cli_alert_info("  Data types: {length(result$data_types)} ({paste(result$data_types, collapse = ', ')})")
    if (length(result$eye_suffixes) > 0) {
      cli::cli_alert_info("  Eye suffixes: {length(result$eye_suffixes)} ({paste(result$eye_suffixes, collapse = ', ')})")
    }

    total_rows <- sum(table_counts, na.rm = TRUE)
    cli::cli_alert_info("  Total rows: {total_rows}")
  }

  return(result)
}
