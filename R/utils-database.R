#' Create or connect to eyeris project database
#'
#' Creates a new DuckDB database for the eyeris project or connects to an existing one.
#' The database will be created in the BIDS derivatives directory.
#'
#' @param bids_dir Path to the BIDS directory containing derivatives
#' @param db_path Database filename (defaults to "eyeris-proj.duckdb")
#' @param verbose Whether to print verbose output
#'
#' @return DBI database connection object
#'
#' @keywords internal
connect_eyeris_database <- function(bids_dir, db_path = "eyeris-proj.duckdb", verbose = FALSE) {
  derivatives_dir <- file.path(bids_dir, "derivatives")
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    if (verbose) {
      cli::cli_alert_info("[INFO] Created derivatives directory: %s", derivatives_dir)
    }
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
          cli::cli_alert_info("[INFO] Connected to existing eyeris database: %s", full_db_path)
        } else {
          cli::cli_alert_success("[OKAY] Created new eyeris database: %s", full_db_path)
        }
      }

      return(con)
    },
    error = function(e) {
      cli::cli_alert_warning("[WARN] Failed to connect to database: %s", e$message)
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
        cli::cli_alert_info("[INFO] Disconnected from eyeris database")
      }
      return(TRUE)
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("[WARN] Error disconnecting from database: %s", e$message)
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
#' @param data_type Type of data ("timeseries", "epochs", "events", "blinks")
#' @param sub Subject ID
#' @param ses Session ID
#' @param task Task name
#' @param run Run number
#' @param eye_suffix Optional eye suffix for binocular data
#'
#' @return Character string with table name
#'
#' @keywords internal
create_table_name <- function(data_type, sub, ses, task, run = NULL, eye_suffix = NULL) {
  # base table name
  table_name <- paste0(data_type, "_", sub, "_", ses, "_", task)

  # add run if provided
  if (!is.null(run)) {
    table_name <- paste0(table_name, "_run", run)
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
#' @param data_type Type of data ("timeseries", "epochs", "events", "blinks")
#' @param sub Subject ID
#' @param ses Session ID
#' @param task Task name
#' @param run Run number
#' @param eye_suffix Optional eye suffix for binocular data
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
  append = TRUE,
  verbose = FALSE
) {
  if (is.null(con)) {
    if (verbose) {
      cli::cli_alert_warning("[WARN] No database connection provided")
    }
    return(FALSE)
  }

  if (is.null(data) || nrow(data) == 0) {
    if (verbose) {
      cli::cli_alert_warning("[WARN] No data to write to database")
    }
    return(FALSE)
  }

  table_name <- create_table_name(data_type, sub, ses, task, run, eye_suffix)

  tryCatch(
    {
      # add metadata columns
      data$subject_id <- sub
      data$session_id <- ses
      data$task_name <- task
      data$data_type <- data_type

      if (!is.null(run)) {
        data$run_number <- run
      }

      if (!is.null(eye_suffix)) {
        data$eye_suffix <- eye_suffix
      }

      data$created_timestamp <- Sys.time()

      DBI::dbWriteTable(
        conn = con,
        name = table_name,
        value = data,
        append = append,
        overwrite = !append
      )

      if (verbose) {
        action <- if (append) "Added" else "Created"
        cli::cli_alert_success(
          "[OKAY] %s table '%s' with %d rows",
          action,
          table_name,
          nrow(data)
        )
      }

      return(TRUE)
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(
          "[WARN] Failed to write data to table '%s': %s",
          table_name,
          e$message
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
    cli::cli_alert_warning("[WARN] No database connection provided")
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
      cli::cli_alert_warning("[WARN] Failed to list tables: %s", e$message)
      return(character(0))
    }
  )
}

#' Read eyeris data from database
#'
#' Reads eyeris data from the project database with dplyr-style interface.
#'
#' @param con Database connection
#' @param data_type Type of data to read ("timeseries", "epochs", "events", "blinks")
#' @param subject Optional subject ID filter
#' @param session Optional session ID filter
#' @param task Optional task name filter
#' @param run Optional run number filter
#' @param eye_suffix Optional eye suffix filter
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
        cli::cli_alert_warning("[WARN] No tables found in database")
        return(data.frame())
      }

      # filter tables based on criteria
      if (!is.null(data_type)) {
        pattern <- paste0("^", data_type, "_")
        tables <- tables[grepl(pattern, tables)]
      }

      if (length(tables) == 0) {
        cli::cli_alert_warning("[WARN] No matching tables found")
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

      # execute query
      result <- DBI::dbGetQuery(con, query)

      return(result)
    },
    error = function(e) {
      cli::cli_alert_warning("[WARN] Failed to read from database: %s", e$message)
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
#' @param db_path Database filename (defaults to "eyeris-proj.duckdb")
#'   If just a filename, will look in `derivatives/` directory.
#'   If includes path, will use as provided.
#'
#' @return Database connection object for use with other eyeris database functions
#'
#' @examples
#' \donttest{
#' # connect to your eyeris project database
#' con <- eyeris_db_connect("/path/to/your/bids/directory")
#'
#' # connect to custom database name
#' con <- eyeris_db_connect("/path/to/bids", db_path = "my-study.duckdb")
#'
#' # list available tables
#' tables <- eyeris_db_list_tables(con)
#'
#' # read timeseries data for a specific subject
#' data <- eyeris_db_read(con, data_type = "timeseries", subject = "001")
#'
#' # close connection when done
#' eyeris_db_disconnect(con)
#' }
#'
#' @export
eyeris_db_connect <- function(bids_dir, db_path = "eyeris-proj.duckdb") {
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
      cli::cli_alert_success("[OKAY] Connected to eyeris database: %s", full_db_path)
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
  disconnect_eyeris_database(con, verbose = TRUE)
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
#' @param data_type Type of data ("timeseries", "epochs", "events", "blinks", "confounds")
#' @param sub Subject ID
#' @param ses Session ID
#' @param task Task name
#' @param run Run number (optional)
#' @param eye_suffix Eye suffix for binocular data (optional)
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
          cli::cli_alert_warning("[WARN] Failed to write CSV file: %s", csv_path)
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
    cli::cli_alert_success(
      "[OKAY] Written %s data (%d rows) to %s",
      data_type %||% "data",
      nrow(data),
      output_str
    )
  }

  return(csv_success && db_success)
}
