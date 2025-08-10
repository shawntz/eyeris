# Test 1: Manual parallel processing flag
test_that("manual parallel processing flag enables temporary databases", {
  # Set environment variable to simulate parallel processing
  withr::with_envvar(c(PARALLEL_PROCESSING = "1"), {
    # Load demo data
    demo_data <- eyelink_asc_demo_dataset()

    # Test with parallel processing enabled
    expect_no_error({
      demo_data |>
        eyeris::glassbox() |>
        eyeris::bidsify(
          bids_dir = tempdir(),
          participant_id = "001",
          session_num = "01",
          task_name = "test",
          db_enabled = TRUE,
          parallel_processing = TRUE, # explicitly enable
          verbose = FALSE # reduce test output
        )
    })
  })
})

# Test 2: Environment variable detection
test_that("SLURM environment variables are detected for parallel processing", {
  # Simulate SLURM environment
  withr::with_envvar(c(SLURM_JOB_ID = "12345"), {
    demo_data <- eyelink_asc_demo_dataset()

    expect_no_error({
      demo_data |>
        eyeris::glassbox() |>
        eyeris::bidsify(
          bids_dir = tempdir(),
          participant_id = "002",
          session_num = "01",
          task_name = "test",
          db_enabled = TRUE,
          verbose = FALSE
        )
    })
  })
})

# Test 3: PBS environment variables are detected
test_that("PBS environment variables are detected for parallel processing", {
  withr::with_envvar(c(PBS_JOBID = "67890"), {
    demo_data <- eyelink_asc_demo_dataset()

    expect_no_error({
      demo_data |>
        eyeris::glassbox() |>
        eyeris::bidsify(
          bids_dir = tempdir(),
          participant_id = "003",
          session_num = "01",
          task_name = "test",
          db_enabled = TRUE,
          verbose = FALSE
        )
    })
  })
})

# Test 4: Normal database operation (no parallel)
test_that("normal database operation works without parallel processing", {
  demo_data <- eyelink_asc_demo_dataset()

  expect_no_error({
    demo_data |>
      eyeris::glassbox() |>
      eyeris::bidsify(
        bids_dir = tempdir(),
        participant_id = "004",
        session_num = "01",
        task_name = "test",
        db_enabled = TRUE,
        parallel_processing = FALSE, # explicitly disable
        verbose = FALSE
      )
  })
})

# Test 5: Temporary database creation
test_that("temporary database creation works", {
  temp_db_info <- create_temp_eyeris_database(
    bids_dir = tempdir(),
    base_db_path = "test-project",
    verbose = FALSE
  )

  expect_type(temp_db_info, "list")
  expect_true(!is.null(temp_db_info$connection))
  expect_true(!is.null(temp_db_info$temp_path))
  expect_true(!is.null(temp_db_info$base_path))
  expect_true(file.exists(temp_db_info$temp_path))

  # Cleanup
  cleanup_temp_database(temp_db_info, verbose = FALSE)
  expect_false(file.exists(temp_db_info$temp_path))
})

# Test 6: Database merging functionality
test_that("temporary database merging works", {
  # Create temporary database
  temp_db_info <- create_temp_eyeris_database(
    bids_dir = tempdir(),
    base_db_path = "merge-test",
    verbose = FALSE
  )

  # Write some test data to temp database
  test_data <- data.frame(
    subject_id = "001",
    session_id = "01",
    task_name = "test",
    data_type = "test",
    created_timestamp = Sys.time(),
    value = 1:5
  )

  DBI::dbWriteTable(
    conn = temp_db_info$connection,
    name = "test_table",
    value = test_data,
    overwrite = TRUE
  )

  # Test merge function
  merge_success <- merge_temp_database(
    temp_db_info = temp_db_info,
    verbose = FALSE,
    max_retries = 3,
    retry_delay = 0.1
  )

  expect_true(merge_success)

  # Verify main database was created and contains data
  expect_true(file.exists(temp_db_info$base_path))

  main_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db_info$base_path)
  tables <- DBI::dbListTables(main_con)
  expect_true("test_table" %in% tables)

  merged_data <- DBI::dbReadTable(main_con, "test_table")
  expect_equal(nrow(merged_data), 5)
  expect_equal(merged_data$subject_id, rep("001", 5))

  DBI::dbDisconnect(main_con)

  # Cleanup
  cleanup_temp_database(temp_db_info, verbose = FALSE)
  if (file.exists(temp_db_info$base_path)) {
    unlink(temp_db_info$base_path)
  }
})
