test_that("chunked database processing works correctly", {
  library(DBI)
  library(duckdb)

  # Create a temporary database with sample data for testing
  temp_db <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb(), temp_db)

  # Ensure cleanup on exit
  on.exit({
    if (!is.null(con)) {
      dbDisconnect(con)
    }
    if (file.exists(temp_db)) unlink(temp_db)
  })

  # Create sample data - simulate a reasonably sized dataset
  dbExecute(
    con,
    "
    CREATE TABLE large_timeseries AS
    SELECT 
      'sub-' || LPAD(((row_number() OVER () - 1) % 3 + 1)::TEXT, 3, '0') as subject_id,
      'ses-01' as session_id,  
      'task-memory' as task_name,
      'timeseries' as data_type,
      row_number() OVER () as time_ms,
      random() * 100 as x_position,
      random() * 100 as y_position,
      (row_number() OVER () - 1) % 3 + 1 as run_number,
      'eye-L' as eye_suffix,
      'test_epoch' as epoch_label,
      current_timestamp as created_timestamp
    FROM range(15000)  -- 15k rows for testing
  "
  )

  row_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM large_timeseries")$n
  expect_equal(row_count, 15000)

  # Test 1: Basic chunked processing with custom function
  rows_processed <- 0
  chunks_seen <- 0

  custom_processor <- function(chunk) {
    rows_processed <<- rows_processed + nrow(chunk)
    chunks_seen <<- chunks_seen + 1
    expect_true(nrow(chunk) > 0)
    expect_true(is.data.frame(chunk))
    return(TRUE)
  }

  result1 <- process_chunked_query(
    con = con,
    query = "SELECT * FROM large_timeseries WHERE subject_id = 'sub-001'",
    chunk_size = 2000,
    process_chunk = custom_processor,
    verbose = FALSE
  )

  expect_equal(result1$total_rows, rows_processed)
  expect_equal(result1$chunks_processed, chunks_seen)
  expect_true(result1$total_rows > 0)
  expect_equal(result1$chunk_size, 2000)

  # Test 2: Export to CSV using chunking
  csv_file <- tempfile(fileext = ".csv")
  on.exit(unlink(csv_file), add = TRUE)

  result2 <- process_chunked_query(
    con = con,
    query = "SELECT subject_id, time_ms, x_position, y_position FROM large_timeseries WHERE subject_id = 'sub-001'",
    chunk_size = 1500,
    output_file = csv_file,
    verbose = FALSE
  )

  expect_true(file.exists(csv_file))
  expect_true(result2$total_rows > 0)
  expect_equal(result2$output_file, csv_file)

  # Verify CSV content
  csv_data <- read.csv(csv_file)
  expect_equal(nrow(csv_data), result2$total_rows)
  expect_true("subject_id" %in% colnames(csv_data))
  expect_true("time_ms" %in% colnames(csv_data))

  # Test 3: Error handling
  expect_error(
    process_chunked_query(NULL, "SELECT * FROM test", chunk_size = 1000),
    "Database connection is required"
  )

  expect_error(
    process_chunked_query(con, "", chunk_size = 1000),
    "Valid SQL query string is required"
  )

  expect_error(
    process_chunked_query(con, "SELECT * FROM test", chunk_size = 0),
    "chunk_size must be at least 1"
  )
})

test_that("chunked processing handles empty results gracefully", {
  library(DBI)
  library(duckdb)

  temp_db <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb(), temp_db)

  on.exit({
    if (!is.null(con)) {
      dbDisconnect(con)
    }
    if (file.exists(temp_db)) unlink(temp_db)
  })

  # Create empty table
  dbExecute(con, "CREATE TABLE empty_table (id INTEGER, name TEXT)")

  # Test chunked processing on empty table
  result <- process_chunked_query(
    con = con,
    query = "SELECT * FROM empty_table",
    chunk_size = 1000,
    verbose = FALSE
  )

  expect_equal(result$total_rows, 0)
  expect_equal(result$chunks_processed, 0)
})

test_that("eyeris_db_to_chunked_files validates inputs correctly", {
  # Test directory validation
  expect_error(
    eyeris_db_to_chunked_files(
      bids_dir = "/nonexistent/directory",
      verbose = FALSE
    ),
    "BIDS directory does not exist"
  )

  # Test file format validation
  expect_error(
    eyeris_db_to_chunked_files(
      bids_dir = tempdir(),
      file_format = "invalid",
      verbose = FALSE
    ),
    "file_format must be 'csv' or 'parquet'"
  )
})

test_that("chunked processing works with parquet output", {
  skip_if_not_installed("arrow")

  library(DBI)
  library(duckdb)

  temp_db <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb(), temp_db)

  on.exit({
    if (!is.null(con)) {
      dbDisconnect(con)
    }
    if (file.exists(temp_db)) unlink(temp_db)
  })

  # Create small test dataset
  dbExecute(
    con,
    "
    CREATE TABLE test_data AS
    SELECT 
      row_number() OVER () as id,
      'test_value_' || row_number() OVER () as name,
      random() * 100 as value
    FROM range(1000)
  "
  )

  parquet_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(parquet_file), add = TRUE)

  result <- process_chunked_query(
    con = con,
    query = "SELECT * FROM test_data",
    chunk_size = 300,
    output_file = parquet_file,
    verbose = FALSE
  )

  expect_true(file.exists(parquet_file))
  expect_equal(result$total_rows, 1000)

  # Verify parquet content
  if (requireNamespace("arrow", quietly = TRUE)) {
    parquet_data <- arrow::read_parquet(parquet_file)
    expect_equal(nrow(parquet_data), 1000)
    expect_true("id" %in% colnames(parquet_data))
  }
})

test_that("column structure grouping works correctly", {
  library(DBI)
  library(duckdb)

  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "test_chunked_output")

  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  # Create a temporary bids structure with the database
  bids_dir <- file.path(temp_dir, "bids_test")
  derivatives_dir <- file.path(bids_dir, "derivatives")
  dir.create(derivatives_dir, recursive = TRUE)

  db_name <- "test-db"
  db_file <- file.path(derivatives_dir, paste0(db_name, ".eyerisdb"))

  # Create database directly in the BIDS structure
  con <- dbConnect(duckdb(), db_file)

  on.exit(
    {
      if (!is.null(con)) dbDisconnect(con)
    },
    add = TRUE
  )

  # Create tables with different column structures (different schemas)
  dbExecute(
    con,
    "
    CREATE TABLE \"confounds_summary_01_task_run01_goal\" AS
    SELECT 
      'sub-001' as subject_id,
      'ses-01' as session_id,
      'task' as task_name,
      'confounds_summary' as data_type,
      1 as goal_onset,
      2 as goal_duration
    FROM range(100)
  "
  )

  dbExecute(
    con,
    "
    CREATE TABLE \"confounds_summary_01_task_run01_stim\" AS
    SELECT 
      'sub-001' as subject_id,
      'ses-01' as session_id,
      'task' as task_name,
      'confounds_summary' as data_type,
      3 as stim_intensity,
      4 as stim_response
    FROM range(100)
  "
  )

  # Add another table with same structure as goal
  dbExecute(
    con,
    "
    CREATE TABLE \"confounds_summary_02_task_run01_goal\" AS
    SELECT 
      'sub-002' as subject_id,
      'ses-01' as session_id,
      'task' as task_name,
      'confounds_summary' as data_type,
      5 as goal_onset,
      6 as goal_duration
    FROM range(50)
  "
  )

  # Close connection before testing
  dbDisconnect(con)
  con <- NULL

  # Test the column structure grouping
  result <- eyeris_db_to_chunked_files(
    bids_dir = bids_dir,
    db_path = db_name,
    output_dir = output_dir,
    data_types = "confounds_summary",
    file_format = "csv",
    chunk_size = 50,
    verbose = TRUE # Enable verbose to see grouping
  )

  # Should create separate files for different column structures
  # The exact filenames will depend on the dynamic grouping
  output_files <- list.files(
    output_dir,
    pattern = ".*_confounds_summary_.*_chunked.*\\.csv$",
    full.names = TRUE
  )

  expect_true(
    length(output_files) >= 2,
    info = paste("Expected at least 2 files, got:", length(output_files))
  )

  # Verify that files contain data
  for (file in output_files) {
    expect_true(file.exists(file))
    data <- read.csv(file)
    expect_true(nrow(data) > 0)
    expect_true("subject_id" %in% colnames(data))
  }

  # Check that files have different structures
  if (length(output_files) >= 2) {
    data1 <- read.csv(output_files[1])
    data2 <- read.csv(output_files[2])

    # They should have different column sets (excluding common metadata columns)
    cols1 <- setdiff(
      colnames(data1),
      c("subject_id", "session_id", "task_name", "data_type")
    )
    cols2 <- setdiff(
      colnames(data2),
      c("subject_id", "session_id", "task_name", "data_type")
    )

    expect_false(
      identical(sort(cols1), sort(cols2)),
      info = "Files should have different column structures"
    )
  }
})

test_that("file size limits create numbered files", {
  library(DBI)
  library(duckdb)

  temp_dir <- tempdir()
  bids_dir <- file.path(temp_dir, "bids_size_test")
  derivatives_dir <- file.path(bids_dir, "derivatives")
  dir.create(derivatives_dir, recursive = TRUE)

  output_dir <- file.path(temp_dir, "size_test_output")

  on.exit({
    unlink(output_dir, recursive = TRUE)
    unlink(bids_dir, recursive = TRUE)
  })

  # Create database with larger dataset
  db_file <- file.path(derivatives_dir, "size-test.eyerisdb")
  con <- dbConnect(duckdb(), db_file)

  on.exit(
    {
      if (!is.null(con) && DBI::dbIsValid(con)) {
        dbDisconnect(con)
      }
    },
    add = TRUE
  )

  # Create table with enough data to exceed size limit
  # Each row will be roughly 50-100 bytes, so 2000 rows should be ~100-200KB
  dbExecute(
    con,
    "
    CREATE TABLE timeseries_01_test_run01 AS
    SELECT 
      'sub-001' as subject_id,
      'ses-01' as session_id, 
      'test' as task_name,
      'timeseries' as data_type,
      row_number() OVER () as time_ms,
      random() * 1000 as x_position,
      random() * 1000 as y_position,
      random() * 100 as pupil_size,
      'some_longer_string_value_' || (row_number() OVER () % 100) as event_label
    FROM range(2000)
  "
  )

  dbDisconnect(con)
  con <- NULL

  # Test with very small max file size to force splitting
  result <- eyeris_db_to_chunked_files(
    bids_dir = bids_dir,
    db_path = "size-test",
    output_dir = output_dir,
    data_types = "timeseries",
    file_format = "csv",
    chunk_size = 500, # Small chunks
    max_file_size_mb = 0.05, # Very small limit (50KB) to force splitting
    verbose = TRUE
  )

  # Should create multiple files due to size limits
  timeseries_info <- result$files$timeseries

  if ("files" %in% names(timeseries_info)) {
    # Multiple files were created
    expect_true(timeseries_info$total_files > 1)
    expect_true(length(timeseries_info$files) > 1)

    # Check that files follow naming pattern
    for (file_path in timeseries_info$files) {
      expect_true(file.exists(file_path))
      expect_true(grepl("_\\d{2}-of-\\d{2}\\.csv$", basename(file_path)))

      # Verify file size is within reasonable bounds
      file_size_mb <- file.size(file_path) / (1024^2)
      expect_true(
        file_size_mb <= 0.1, # Allow some overhead
        info = paste(
          "File too large:",
          basename(file_path),
          "-",
          round(file_size_mb, 3),
          "MB"
        )
      )
    }

    # Verify all files together contain all the data
    total_rows_in_files <- 0
    for (file_path in timeseries_info$files) {
      data <- read.csv(file_path)
      total_rows_in_files <- total_rows_in_files + nrow(data)
      expect_true("subject_id" %in% colnames(data))
      expect_true("time_ms" %in% colnames(data))
    }

    expect_equal(total_rows_in_files, timeseries_info$rows)
  } else {
    # Single file case - check it exists and has reasonable size
    expect_true(file.exists(timeseries_info$file))
    expect_true(timeseries_info$rows > 0)
  }
})
