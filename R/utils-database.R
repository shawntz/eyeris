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
        cli::cli_alert_warning("[WARN] No matching tables found", wrap = TRUE)
        return(data.frame())
      }

      # unite all matching tables
      union_queries <- c()
      for (table in tables) {
        union_queries <- c(union_queries, paste0("SELECT * FROM ", table))
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

      if (!is.null(epoch_label)) {
        query <- paste(query, "AND LOWER(epoch_label) =", tolower(shQuote(epoch_label)))
      }

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
