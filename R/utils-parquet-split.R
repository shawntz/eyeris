#' Split eyeris database into N parquet files by data type
#'
#' Utility function that takes an eyerisdb DuckDB database and splits it into
#' N reasonably sized parquet files for easy management with GitHub, downloading,
#' and distribution. Data is first grouped by table type (timeseries, epochs,
#' events, etc.) since each has different columnar structures, then each group
#' is split into the specified number of files. Files are organized in folders
#' matching the database name for easy identification.
#'
#' @section Database Safety:
#' This function creates temporary tables during parquet export when the arrow
#' package is not available. All temporary tables are automatically cleaned up,
#' but if the process crashes, leftover tables may remain. The function checks
#' for and warns about existing temporary tables before starting.
#'
#' @param bids_dir Path to the BIDS directory containing the database
#' @param db_path Database name (defaults to "my-project", becomes "my-project.eyerisdb")
#' @param n_files_per_type Number of parquet files to create per data type (default: 1)
#' @param output_dir Directory to save parquet files (defaults to bids_dir/derivatives/parquet)
#' @param max_file_size Maximum file size in MB per parquet file (default: 512)
#'   Used as a constraint when n_files_per_type would create files larger than this
#' @param data_types Vector of data types to include. If NULL (default), includes all available.
#'   Valid types: "timeseries", "epochs", "epoch_summary", "events", "blinks", "confounds_*"
#' @param verbose Whether to print progress messages (default: TRUE)
#' @param include_metadata Whether to include eyeris metadata columns in output (default: TRUE)
#' @param epoch_labels Optional character vector of epoch labels to include (e.g., "prepostprobe").
#'   Only applies to epoch-related data types. If NULL, includes all labels.
#' @param group_by_epoch_label If TRUE, processes epoch-related data types separately by epoch label
#'   to reduce memory footprint and produce label-specific parquet files (default: TRUE).
#'
#' @return List containing information about created parquet files
#'
#' @examples
#' \donttest{
#' # create demo database
#' demo_data <- eyelink_asc_demo_dataset()
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
#'     task_name = "memory",
#'     db_enabled = TRUE,
#'     db_path = "memory-task"
#'   )
#'
#' # split into 3 parquet files per data type - creates memory-task/ folder
#' split_info <- eyeris_db_to_parquet(
#'   bids_dir = tempdir(),
#'   db_path = "memory-task",
#'   n_files_per_type = 3
#' )
#'
#' # split with size constraint and specific data types using the same database
#' split_info <- eyeris_db_to_parquet(
#'   bids_dir = tempdir(),
#'   db_path = "memory-task",
#'   n_files_per_type = 5,
#'   max_file_size = 50,  # max 50MB per file
#'   data_types = c("timeseries", "epochs", "events")
#' )
#' }
#'
#' @export
eyeris_db_to_parquet <- function(
  bids_dir,
  db_path = "my-project",
  n_files_per_type = 1,
  output_dir = NULL,
  max_file_size = 512,
  data_types = NULL,
  verbose = TRUE,
  include_metadata = TRUE,
  epoch_labels = NULL,
  group_by_epoch_label = TRUE
) {
  # first check if duckdb is installed
  if (!check_duckdb()) {
    log_error(
      "DuckDB is required for this feature. See installation instructions above.",
      verbose = TRUE
    )
  }

  # validate inputs
  if (!dir.exists(bids_dir)) {
    log_error("BIDS directory does not exist: {bids_dir}")
  }

  if (n_files_per_type < 1) {
    log_error("n_files_per_type must be at least 1")
  }

  if (max_file_size < 1) {
    log_error("max_file_size must be at least 1 MB")
  }

  # extract database name (remove .eyerisdb extension if present)
  db_name <- gsub("\\.eyerisdb$", "", basename(db_path))

  # setup output directory - create folder named after database
  if (is.null(output_dir)) {
    output_dir <- file.path(bids_dir, "derivatives", "parquet", db_name)
  } else {
    output_dir <- file.path(output_dir, db_name)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    log_info("Created output directory: {output_dir}", verbose = verbose)
  }

  # connect to database
  log_info(
    "Connecting to eyeris database: {db_name}.eyerisdb",
    verbose = verbose
  )
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

  # helper function for safe temporary table operations
  safe_temp_table_export <- function(chunk_data, filepath, data_type, file_id) {
    temp_table <- paste0(
      "temp_export_",
      data_type,
      "_",
      file_id,
      "_",
      sample(10000:99999, 1)
    )

    # ensure cleanup even if export fails
    on.exit({
      tryCatch(
        {
          if (DBI::dbExistsTable(con, temp_table)) {
            DBI::dbExecute(con, glue::glue("DROP TABLE {temp_table}"))
          }
        },
        error = function(e) {
          log_warn(
            "Failed to cleanup temp table {temp_table}: {e$message}",
            verbose = verbose
          )
        }
      )
    })

    # create temp table and export
    DBI::dbWriteTable(con, temp_table, chunk_data, overwrite = TRUE)
    DBI::dbExecute(
      con,
      glue::glue("COPY {temp_table} TO '{filepath}' (FORMAT PARQUET)")
    )

    # manual cleanup (on.exit still provides backup)
    DBI::dbExecute(con, glue::glue("DROP TABLE {temp_table}"))
  }

  # get all tables and filter out temporary tables
  all_tables <- eyeris_db_list_tables(con)

  # check for and warn about existing temp tables
  temp_tables <- all_tables[grepl("^temp_", all_tables)]
  if (length(temp_tables) > 0) {
    log_warn(
      "Found {length(temp_tables)} temporary tables in database: {paste(head(temp_tables, 3), collapse = ', ')}{if (length(temp_tables) > 3) '...' else ''}",
      verbose = TRUE
    )
    log_warn(
      "Temporary tables will be EXCLUDED from export to prevent contamination.",
      verbose = TRUE
    )
  }

  # filter out temporary tables from export
  all_tables <- all_tables[!grepl("^temp_", all_tables)]

  if (length(all_tables) == 0) {
    log_warn(
      "No valid tables found in database (after excluding temp tables)",
      verbose = TRUE
    )
    return(list(
      files = character(0),
      total_rows = 0,
      database_name = db_name,
      data_types = character(0)
    ))
  }

  log_info(
    "Found {length(all_tables)} valid tables in database (excluded {length(temp_tables)} temp tables)",
    verbose = verbose
  )

  # group tables by data type (extract from table name)
  log_info("Grouping tables by data type...", verbose = verbose)
  data_type_groups <- list()

  for (table in all_tables) {
    # extract data type from table name (first part before first underscore)
    data_type <- gsub("^([^_]+)_.*", "\\1", table)

    if (is.null(data_types) || data_type %in% data_types) {
      if (is.null(data_type_groups[[data_type]])) {
        data_type_groups[[data_type]] <- character(0)
      }
      data_type_groups[[data_type]] <- c(data_type_groups[[data_type]], table)
    }
  }

  if (length(data_type_groups) == 0) {
    log_warn("No tables found matching specified data types", verbose = TRUE)
    return(list(
      files = character(0),
      total_rows = 0,
      database_name = db_name,
      data_types = character(0)
    ))
  }

  log_info(
    "Found {length(data_type_groups)} data types: {paste(names(data_type_groups), collapse = ', ')}",
    verbose = verbose
  )

  # process each data type separately
  all_created_files <- character(0)
  all_file_info <- list()
  total_rows <- 0
  total_output_size <- 0

  for (data_type in names(data_type_groups)) {
    tables_for_type <- data_type_groups[[data_type]]

    log_info(
      "Processing data type: {data_type} ({length(tables_for_type)} tables)",
      verbose = verbose
    )

    # helper: get epoch label for a given table
    extract_epoch_label <- function(table_name) {
      # try reading metadata column quickly
      label <- NA_character_
      got <- try(
        {
          DBI::dbGetQuery(
            con,
            paste0('SELECT epoch_label FROM "', table_name, '" LIMIT 1')
          )
        },
        silent = TRUE
      )
      if (
        !inherits(got, "try-error") &&
          is.data.frame(got) &&
          nrow(got) > 0 &&
          "epoch_label" %in% names(got)
      ) {
        label <- got$epoch_label[[1]]
      }
      if (!is.na(label) && !is.null(label) && nzchar(label)) {
        return(tolower(as.character(label)))
      }
      # fallback to pattern-based extraction
      lbl <- NA_character_
      if (grepl('^epochs_', table_name)) {
        lbl <- sub(
          '^epochs_[^_]+_[^_]+_[^_]+_[^_]+_(.+?)(?:_(?:eye[LR]|eye-[LR]))?$',
          '\\1',
          table_name
        )
      } else if (grepl('^confounds_(events|summary)_', table_name)) {
        lbl <- sub(
          '^confounds_(?:events|summary)_[^_]+_[^_]+_[^_]+_[^_]+_(.+?)(?:_(?:eye[LR]|eye-[LR]))?$',
          '\\1',
          table_name
        )
      } else if (grepl('^epoch_(summary|timeseries)_', table_name)) {
        lbl <- sub(
          '^epoch_(?:summary|timeseries)_[^_]+_[^_]+_[^_]+_[^_]+_(.+?)(?:_(?:eye[LR]|eye-[LR]))?$',
          '\\1',
          table_name
        )
      }
      if (!is.na(lbl) && !identical(lbl, table_name)) {
        return(tolower(lbl))
      }
      return(NA_character_)
    }

    # determine whether to group by epoch label for this data type
    epoch_related <- data_type %in% c("epochs", "epoch", "confounds")
    label_filter <- if (!is.null(epoch_labels)) tolower(epoch_labels) else NULL

    if (group_by_epoch_label && epoch_related) {
      # build map from label -> tables
      label_to_tables <- list()
      for (table in tables_for_type) {
        lbl <- extract_epoch_label(table)
        if (!is.null(label_filter) && !(lbl %in% label_filter)) {
          next
        }
        key <- if (is.na(lbl) || !nzchar(lbl)) "_nolabel" else lbl
        if (is.null(label_to_tables[[key]])) {
          label_to_tables[[key]] <- character(0)
        }
        label_to_tables[[key]] <- c(label_to_tables[[key]], table)
      }
      if (length(label_to_tables) == 0) {
        log_warn(
          "No data found for data type: {data_type} after epoch label filtering",
          verbose = verbose
        )
        next
      }
      # process each label subgroup individually
      for (label_key in names(label_to_tables)) {
        tables_subset <- label_to_tables[[label_key]]
        # first pass: count rows per table to determine chunk sizes without loading all data
        type_total_rows <- 0
        for (table in tables_subset) {
          cnt <- tryCatch(
            {
              DBI::dbGetQuery(
                con,
                paste0('SELECT COUNT(*) as n FROM "', table, '"')
              )$n
            },
            error = function(e) NA_integer_
          )
          if (!is.na(cnt)) type_total_rows <- type_total_rows + as.integer(cnt)
        }
        if (type_total_rows == 0) {
          next
        }
        # determine rows per file from target n_files_per_type
        n_files_for_type <- n_files_per_type
        rows_per_file <- ceiling(type_total_rows / n_files_for_type)
        log_info(
          "  {data_type}[{if (label_key == '_nolabel') 'nolabel' else label_key}]: {type_total_rows} rows (~size est deferred)",
          verbose = verbose
        )
        log_info(
          "  Splitting {data_type}[{if (label_key == '_nolabel') 'nolabel' else label_key}] into {n_files_for_type} files (~{rows_per_file} rows per file)",
          verbose = verbose
        )

        # streaming buffer to avoid holding all tables in memory
        buffer <- NULL
        current_cols <- character(0)
        part_idx <- 1
        emitted_rows <- 0

        align_cols <- function(df, cols) {
          missing <- setdiff(cols, colnames(df))
          for (m in missing) {
            df[[m]] <- NA
          }
          # also ensure order matches desired cols
          df <- df[, cols, drop = FALSE]
          df
        }

        write_part <- function(chunk) {
          label_component <- if (label_key == "_nolabel") {
            NULL
          } else {
            paste0("_", label_key)
          }
          filename <- sprintf(
            "%s_%s%s_part-%02d-of-%02d.parquet",
            db_name,
            data_type,
            if (is.null(label_component)) "" else label_component,
            part_idx,
            n_files_for_type
          )
          filepath <- file.path(output_dir, filename)
          tryCatch(
            {
              if (requireNamespace("arrow", quietly = TRUE)) {
                arrow::write_parquet(chunk, filepath)
              } else {
                safe_temp_table_export(chunk, filepath, data_type, part_idx)
              }
              all_created_files <<- c(all_created_files, filepath)
              actual_size_mb <- file.size(filepath) / (1024^2)
              total_output_size <<- total_output_size + actual_size_mb
              all_file_info[[filename]] <<- list(
                path = filepath,
                data_type = data_type,
                epoch_label = if (label_key == "_nolabel") {
                  NA_character_
                } else {
                  label_key
                },
                rows = nrow(chunk),
                size_mb = actual_size_mb,
                part = part_idx,
                total_parts = n_files_for_type
              )
              log_success(
                "  Created {filename}: {nrow(chunk)} rows ({round(actual_size_mb, 1)} MB)",
                verbose = verbose
              )
            },
            error = function(e) {
              log_warn(
                "Failed to create parquet file {filename}: {e$message}",
                verbose = verbose
              )
            }
          )
          part_idx <<- part_idx + 1
        }

        for (table in tables_subset) {
          dat <- tryCatch(DBI::dbReadTable(con, table), error = function(e) {
            NULL
          })
          if (is.null(dat) || nrow(dat) == 0) {
            next
          }
          # optionally strip metadata as we go
          if (!include_metadata) {
            metadata_cols <- c(
              "subject_id",
              "session_id",
              "task_name",
              "data_type",
              "run_number",
              "eye_suffix",
              "epoch_label",
              "created_timestamp"
            )
            drop_cols <- intersect(metadata_cols, colnames(dat))
            if (length(drop_cols) > 0) {
              dat <- dat[, !colnames(dat) %in% drop_cols, drop = FALSE]
            }
          }
          # maintain union of columns across tables
          if (is.null(buffer)) {
            current_cols <- colnames(dat)
            buffer <- dat
          } else {
            # update union columns
            new_union <- union(current_cols, colnames(dat))
            if (!identical(new_union, current_cols)) {
              # expand buffer and dat to new union
              if (!identical(colnames(buffer), new_union)) {
                buffer <- align_cols(buffer, union(colnames(buffer), new_union))
              }
              if (!identical(colnames(dat), new_union)) {
                dat <- align_cols(dat, union(colnames(dat), new_union))
              }
              current_cols <- new_union
            }
            # bind
            buffer <- rbind(buffer, dat)
          }
          # flush full parts
          while (nrow(buffer) >= rows_per_file) {
            chunk <- buffer[seq_len(rows_per_file), , drop = FALSE]
            write_part(chunk)
            emitted_rows <- emitted_rows + nrow(chunk)
            # keep remainder in buffer
            if (nrow(buffer) > rows_per_file) {
              buffer <- buffer[(rows_per_file + 1):nrow(buffer), , drop = FALSE]
            } else {
              buffer <- buffer[0, , drop = FALSE]
            }
          }
        }
        # write remainder
        if (!is.null(buffer) && nrow(buffer) > 0) {
          write_part(buffer)
          emitted_rows <- emitted_rows + nrow(buffer)
        }

        total_rows <- total_rows + type_total_rows
      }
      next # done with this data type via label grouping
    }

    # non-epoch or no grouping: collect data for this data type (as before)
    type_data_list <- list()
    type_total_rows <- 0
    type_total_size_mb <- 0
    for (table in tables_for_type) {
      tryCatch(
        {
          data <- DBI::dbReadTable(con, table)
          if (nrow(data) > 0) {
            type_data_list[[table]] <- data
            type_total_rows <- type_total_rows + nrow(data)
            type_total_size_mb <- type_total_size_mb +
              as.numeric(object.size(data) / (1024^2))
          }
        },
        error = function(e) {
          log_warn(
            "Failed to read table '{table}': {e$message}",
            verbose = verbose
          )
        }
      )
    }
    if (length(type_data_list) == 0 || type_total_rows == 0) {
      log_warn("No data found for data type: {data_type}", verbose = verbose)
      next
    }
    log_info(
      "  {data_type}: {type_total_rows} rows (~{round(type_total_size_mb, 1)} MB)",
      verbose = verbose
    )
    combined_type_data <- as.data.frame(data.table::rbindlist(
      type_data_list,
      use.names = TRUE,
      fill = TRUE
    ))

    # optionally remove eyeris metadata columns for cleaner output
    if (!include_metadata) {
      metadata_cols <- c(
        "subject_id",
        "session_id",
        "task_name",
        "data_type",
        "run_number",
        "eye_suffix",
        "epoch_label",
        "created_timestamp"
      )

      cols_to_remove <- intersect(metadata_cols, colnames(combined_type_data))
      if (length(cols_to_remove) > 0) {
        combined_type_data <- combined_type_data[,
          !colnames(combined_type_data) %in% cols_to_remove,
          drop = FALSE
        ]
      }
    }

    # determine number of files for this data type
    n_files_for_type <- n_files_per_type
    target_size_per_file <- type_total_size_mb / n_files_for_type

    if (target_size_per_file > max_file_size) {
      adjusted_n_files <- ceiling(type_total_size_mb / max_file_size)
      log_info(
        "  Adjusting files for {data_type} from {n_files_for_type} to {adjusted_n_files} due to size constraint",
        verbose = verbose
      )
      n_files_for_type <- adjusted_n_files
    }

    # split this data type into files
    rows_per_file <- ceiling(nrow(combined_type_data) / n_files_for_type)

    log_info(
      "  Splitting {data_type} into {n_files_for_type} files (~{rows_per_file} rows per file)",
      verbose = verbose
    )

    for (i in 1:n_files_for_type) {
      start_row <- (i - 1) * rows_per_file + 1
      end_row <- min(i * rows_per_file, nrow(combined_type_data))

      if (start_row > nrow(combined_type_data)) {
        break # no more data to write
      }

      chunk_data <- combined_type_data[start_row:end_row, , drop = FALSE]

      # create filename: dbname_datatype_part-XX-of-YY.parquet
      filename <- sprintf(
        "%s_%s_part-%02d-of-%02d.parquet",
        db_name,
        data_type,
        i,
        n_files_for_type
      )
      filepath <- file.path(output_dir, filename)

      tryCatch(
        {
          # write parquet file using arrow (if available) or fallback to DuckDB
          if (requireNamespace("arrow", quietly = TRUE)) {
            arrow::write_parquet(chunk_data, filepath)
          } else {
            # fallback: use DuckDB to write parquet with safe temp table handling
            safe_temp_table_export(chunk_data, filepath, data_type, i)
          }

          all_created_files <- c(all_created_files, filepath)
          actual_size_mb <- file.size(filepath) / (1024^2)
          total_output_size <- total_output_size + actual_size_mb

          all_file_info[[filename]] <- list(
            path = filepath,
            data_type = data_type,
            rows = nrow(chunk_data),
            size_mb = actual_size_mb,
            row_range = c(start_row, end_row),
            part = i,
            total_parts = n_files_for_type
          )

          log_success(
            "  Created {filename}: {nrow(chunk_data)} rows ({round(actual_size_mb, 1)} MB)",
            verbose = verbose
          )
        },
        error = function(e) {
          log_warn(
            "Failed to create parquet file {filename}: {e$message}",
            verbose = verbose
          )
        }
      )
    }

    total_rows <- total_rows + type_total_rows
  }

  log_success(
    "Successfully created {length(all_created_files)} parquet files across {length(data_type_groups)} data types",
    verbose = verbose
  )
  log_info("Total output: {round(total_output_size, 1)} MB", verbose = verbose)

  result <- list(
    files = all_created_files,
    file_info = all_file_info,
    total_rows = total_rows,
    total_size_mb = total_output_size,
    output_dir = output_dir,
    database_name = db_name,
    data_types = names(data_type_groups),
    n_files_created = length(all_created_files)
  )

  return(result)
}

#' Read parquet files back into R
#'
#' Convenience function to read the parquet files created by eyeris_db_to_parquet
#' back into a single data frame or list of data frames by data type.
#'
#' @param parquet_dir Directory containing the parquet files, or path to database-specific folder
#' @param db_name Optional database name to read from (if parquet_dir contains multiple database folders)
#' @param data_type Optional data type to read (if NULL, reads all data types)
#' @param return_list Whether to return a list by data type (TRUE) or combined data frame (FALSE, default)
#' @param pattern Pattern to match parquet files (default: "*.parquet")
#' @param verbose Whether to print progress messages (default: TRUE)
#'
#' @return Combined data frame from all parquet files, or list of data frames by data type
#'
#' @examples
#' \donttest{
#' # Minimal self-contained example that avoids database creation
#' if (requireNamespace("arrow", quietly = TRUE)) {
#'   # create a temporary folder structure: parquet/<db_name>
#'   base_dir <- file.path(tempdir(), "derivatives", "parquet")
#'   db_name <- "example-db"
#'   dir.create(file.path(base_dir, db_name), recursive = TRUE, showWarnings = FALSE)
#'
#'   # write two small parquet parts for a single data type
#'   part1 <- data.frame(time = 1:5, value = 1:5)
#'   part2 <- data.frame(time = 6:10, value = 6:10)
#'   arrow::write_parquet(
#'     part1,
#'     file.path(
#'       base_dir, db_name, paste0(db_name, "_timeseries_part-01-of-02.parquet")
#'     )
#'   )
#'   arrow::write_parquet(
#'     part2,
#'     file.path(
#'       base_dir, db_name, paste0(db_name, "_timeseries_part-02-of-02.parquet")
#'     )
#'   )
#'
#'   # read them back as combined data frame
#'   data <- read_eyeris_parquet(base_dir, db_name = db_name)
#'
#'   # read as list by data type
#'   data_by_type <- read_eyeris_parquet(base_dir, db_name = db_name, return_list = TRUE)
#'
#'   # read specific data type only
#'   timeseries_data <- read_eyeris_parquet(base_dir, db_name = db_name, data_type = "timeseries")
#' }
#' }
#'
#' @export
read_eyeris_parquet <- function(
  parquet_dir,
  db_name = NULL,
  data_type = NULL,
  return_list = FALSE,
  pattern = "*.parquet",
  verbose = TRUE
) {
  # if db_name is provided, look in that subfolder
  if (!is.null(db_name)) {
    # first check if duckdb is installed
    if (!check_duckdb()) {
      log_error(
        "DuckDB is required for this feature. See installation instructions above.",
        verbose = TRUE
      )
    }
    parquet_dir <- file.path(parquet_dir, db_name)
  }

  if (!dir.exists(parquet_dir)) {
    log_error("Parquet directory does not exist: {parquet_dir}")
  }

  # find parquet files
  parquet_files <- list.files(
    parquet_dir,
    pattern = gsub("\\*", ".*", pattern),
    full.names = TRUE
  )

  if (length(parquet_files) == 0) {
    log_warn(
      "No parquet files found in directory: {parquet_dir}",
      verbose = verbose
    )
    if (return_list) {
      return(list())
    } else {
      return(data.frame())
    }
  }

  # filter by data type if specified
  if (!is.null(data_type)) {
    type_pattern <- paste0("_", data_type, "_")
    parquet_files <- parquet_files[grepl(type_pattern, basename(parquet_files))]

    if (length(parquet_files) == 0) {
      log_warn(
        "No parquet files found for data type: {data_type}",
        verbose = verbose
      )
      if (return_list) {
        return(list())
      } else {
        return(data.frame())
      }
    }
  }

  # sort files to maintain order (part-01, part-02, etc.)
  parquet_files <- sort(parquet_files)

  log_info("Found {length(parquet_files)} parquet files", verbose = verbose)

  # group files by data type if returning list
  if (return_list) {
    data_type_files <- list()

    for (file in parquet_files) {
      filename <- basename(file)
      # extract data type from filename: dbname_datatype_part-XX-of-YY.parquet
      extracted_type <- gsub("^[^_]+_([^_]+)_part-.*", "\\1", filename)

      if (is.null(data_type_files[[extracted_type]])) {
        data_type_files[[extracted_type]] <- character(0)
      }
      data_type_files[[extracted_type]] <- c(
        data_type_files[[extracted_type]],
        file
      )
    }

    # read each data type separately
    result_list <- list()

    for (type in names(data_type_files)) {
      type_files <- sort(data_type_files[[type]])
      type_data_list <- list()

      for (file in type_files) {
        filename <- basename(file)

        tryCatch(
          {
            if (requireNamespace("arrow", quietly = TRUE)) {
              data <- arrow::read_parquet(file)
            } else {
              # fallback: read via DuckDB (disconnect immediately after read)
              temp_con <- DBI::dbConnect(duckdb::duckdb())
              data <- DBI::dbGetQuery(
                temp_con,
                glue::glue("SELECT * FROM read_parquet('{file}')")
              )
              DBI::dbDisconnect(temp_con)
            }

            type_data_list[[filename]] <- data

            log_info("Read {filename}: {nrow(data)} rows", verbose = verbose)
          },
          error = function(e) {
            log_warn(
              "Failed to read {filename}: {e$message}",
              verbose = verbose
            )
          }
        )
      }

      if (length(type_data_list) > 0) {
        result_list[[type]] <- as.data.frame(data.table::rbindlist(
          type_data_list,
          use.names = TRUE,
          fill = TRUE
        ))
        log_info(
          "Combined {type}: {nrow(result_list[[type]])} total rows",
          verbose = verbose
        )
      }
    }

    return(result_list)
  } else {
    # read and combine all files into single data frame
    data_list <- list()
    total_rows <- 0

    for (i in seq_along(parquet_files)) {
      file <- parquet_files[i]
      filename <- basename(file)

      tryCatch(
        {
          if (requireNamespace("arrow", quietly = TRUE)) {
            data <- arrow::read_parquet(file)
          } else {
            # fallback: read via DuckDB (disconnect immediately after read)
            temp_con <- DBI::dbConnect(duckdb::duckdb())
            data <- DBI::dbGetQuery(
              temp_con,
              glue::glue("SELECT * FROM read_parquet('{file}')")
            )
            DBI::dbDisconnect(temp_con)
          }

          data_list[[i]] <- data
          total_rows <- total_rows + nrow(data)

          log_info("Read {filename}: {nrow(data)} rows", verbose = verbose)
        },
        error = function(e) {
          log_warn("Failed to read {filename}: {e$message}", verbose = verbose)
        }
      )
    }

    if (length(data_list) == 0) {
      log_warn(
        "No data successfully read from parquet files",
        verbose = verbose
      )
      return(data.frame())
    }

    # combine all data (align by column names; fill missing)
    log_info("Combining {length(data_list)} files...", verbose = verbose)
    combined_data <- as.data.frame(data.table::rbindlist(
      data_list,
      use.names = TRUE,
      fill = TRUE
    ))

    log_success(
      "Successfully read {total_rows} total rows from parquet files",
      verbose = verbose
    )

    return(combined_data)
  }
}
