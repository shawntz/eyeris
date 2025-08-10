test_that("eyeris_db_to_parquet creates correctly named files and folders", {
  skip_if_not_installed("arrow")

  # setup temporary directory
  temp_dir <- tempdir()
  bids_dir <- file.path(temp_dir, "test_bids")
  dir.create(bids_dir, recursive = TRUE)

  # create a simple test database with some dummy data
  db_path <- "test-study"
  con <- connect_eyeris_database(
    bids_dir = bids_dir,
    db_path = db_path,
    verbose = FALSE
  )

  # create test data
  test_data <- data.frame(
    time = 1:100,
    pupil_left = rnorm(100, 3, 0.5),
    pupil_right = rnorm(100, 3, 0.5),
    x_gaze = rnorm(100, 500, 50),
    y_gaze = rnorm(100, 400, 50)
  )

  # write test data to database
  success <- write_eyeris_data_to_db(
    data = test_data,
    con = con,
    data_type = "timeseries",
    sub = "001",
    ses = "01",
    task = "test",
    run = 1,
    verbose = FALSE
  )

  expect_true(success)

  # disconnect before splitting
  eyeris_db_disconnect(con)

  # test the parquet split function
  result <- eyeris_db_to_parquet(
    bids_dir = bids_dir,
    db_path = db_path,
    n_files_per_type = 2,
    verbose = FALSE
  )

  # check that result structure is correct
  expect_true(is.list(result))
  expect_true("files" %in% names(result))
  expect_true("database_name" %in% names(result))
  expect_true("data_types" %in% names(result))
  expect_equal(result$database_name, "test-study")
  expect_equal(length(result$files), 2) # 2 files for 1 data type (timeseries)
  expect_equal(result$data_types, "timeseries")

  # check that output directory has correct structure
  expected_output_dir <- file.path(
    bids_dir,
    "derivatives",
    "parquet",
    "test-study"
  )
  expect_equal(result$output_dir, expected_output_dir)
  expect_true(dir.exists(expected_output_dir))

  # check that files have correct naming pattern (datatype-specific)
  for (i in 1:2) {
    expected_filename <- sprintf(
      "test-study_timeseries_part-%02d-of-%02d.parquet",
      i,
      2
    )
    expected_filepath <- file.path(expected_output_dir, expected_filename)
    expect_true(file.exists(expected_filepath))
    expect_true(expected_filepath %in% result$files)
  }

  # cleanup
  unlink(bids_dir, recursive = TRUE)
})

test_that("read_eyeris_parquet correctly reads back split data", {
  skip_if_not_installed("arrow")

  # setup temporary directory
  temp_dir <- tempdir()
  bids_dir <- file.path(temp_dir, "test_bids_read")
  dir.create(bids_dir, recursive = TRUE)

  # create test database
  db_path <- "read-test"
  con <- connect_eyeris_database(
    bids_dir = bids_dir,
    db_path = db_path,
    verbose = FALSE
  )

  # create predictable test data
  original_data <- data.frame(
    time = 1:50,
    pupil_left = seq(1, 50),
    pupil_right = seq(51, 100),
    x_gaze = seq(101, 150),
    y_gaze = seq(151, 200)
  )

  # write to database
  success <- write_eyeris_data_to_db(
    data = original_data,
    con = con,
    data_type = "timeseries",
    sub = "002",
    ses = "01",
    task = "read",
    verbose = FALSE
  )

  expect_true(success)
  eyeris_db_disconnect(con)

  # split into parquet files
  split_result <- eyeris_db_to_parquet(
    bids_dir = bids_dir,
    db_path = db_path,
    n_files_per_type = 2,
    include_metadata = FALSE, # exclude metadata for cleaner comparison
    verbose = FALSE
  )

  expect_equal(length(split_result$files), 2)

  # read back the data
  read_data <- read_eyeris_parquet(
    parquet_dir = split_result$output_dir,
    verbose = FALSE
  )

  # check that we got back the same data (excluding metadata)
  expect_equal(nrow(read_data), nrow(original_data))
  expect_true(all(
    c("time", "pupil_left", "pupil_right", "x_gaze", "y_gaze") %in%
      colnames(read_data)
  ))

  # check that data values are preserved (may be in different order due to splitting)
  expect_equal(sort(read_data$time), sort(original_data$time))
  expect_equal(sort(read_data$pupil_left), sort(original_data$pupil_left))

  # cleanup
  unlink(bids_dir, recursive = TRUE)
})

test_that("eyeris_db_to_parquet handles file size constraints", {
  skip_if_not_installed("arrow")

  # setup temporary directory
  temp_dir <- tempdir()
  bids_dir <- file.path(temp_dir, "test_bids_size")
  dir.create(bids_dir, recursive = TRUE)

  # create test database with larger data
  db_path <- "size-test"
  con <- connect_eyeris_database(
    bids_dir = bids_dir,
    db_path = db_path,
    verbose = FALSE
  )

  # create larger test dataset
  large_data <- data.frame(
    time = 1:1000,
    pupil_left = rnorm(1000, 3, 0.5),
    pupil_right = rnorm(1000, 3, 0.5),
    x_gaze = rnorm(1000, 500, 50),
    y_gaze = rnorm(1000, 400, 50),
    extra_col = paste0("data_", 1:1000) # make it larger
  )

  success <- write_eyeris_data_to_db(
    data = large_data,
    con = con,
    data_type = "timeseries",
    sub = "003",
    ses = "01",
    task = "size",
    verbose = FALSE
  )

  expect_true(success)
  eyeris_db_disconnect(con)

  # test with very small max file size to force more files
  result <- eyeris_db_to_parquet(
    bids_dir = bids_dir,
    db_path = db_path,
    n_files_per_type = 2,
    max_file_size = 1, # very small limit
    verbose = FALSE
  )

  # should create more than 2 files due to size constraint
  expect_true(length(result$files) >= 2)
  expect_true(result$n_files_created >= 2)

  # check that files actually exist
  for (file in result$files) {
    expect_true(file.exists(file))
    expect_true(file.size(file) > 0)
  }

  # cleanup
  unlink(bids_dir, recursive = TRUE)
})

test_that("eyeris_db_to_parquet handles empty database gracefully", {
  # setup temporary directory
  temp_dir <- tempdir()
  bids_dir <- file.path(temp_dir, "test_bids_empty")
  dir.create(bids_dir, recursive = TRUE)

  # create empty database
  db_path <- "empty-test"
  con <- connect_eyeris_database(
    bids_dir = bids_dir,
    db_path = db_path,
    verbose = FALSE
  )

  # disconnect immediately without adding data
  eyeris_db_disconnect(con)

  # test splitting empty database
  result <- eyeris_db_to_parquet(
    bids_dir = bids_dir,
    db_path = db_path,
    n_files_per_type = 3,
    verbose = FALSE
  )

  # should return empty result
  expect_equal(length(result$files), 0)
  expect_equal(result$total_rows, 0)
  expect_equal(result$database_name, "empty-test")

  # cleanup
  unlink(bids_dir, recursive = TRUE)
})

test_that("read_eyeris_parquet works with db_name parameter", {
  skip_if_not_installed("arrow")

  # setup temporary directory with parquet structure
  temp_dir <- tempdir()
  parquet_base_dir <- file.path(temp_dir, "parquet_test")
  db_folder <- file.path(parquet_base_dir, "my-study")
  dir.create(db_folder, recursive = TRUE)

  # create test parquet files directly
  test_data_1 <- data.frame(time = 1:25, value = 1:25)

  test_data_2 <- data.frame(time = 26:50, value = 26:50)

  # write test parquet files with new naming convention
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(
      test_data_1,
      file.path(db_folder, "my-study_timeseries_part-01-of-02.parquet")
    )
    arrow::write_parquet(
      test_data_2,
      file.path(db_folder, "my-study_timeseries_part-02-of-02.parquet")
    )

    # test reading with db_name parameter
    read_data <- read_eyeris_parquet(
      parquet_dir = parquet_base_dir,
      db_name = "my-study",
      verbose = FALSE
    )

    expect_equal(nrow(read_data), 50)
    expect_equal(sort(read_data$time), 1:50)
    expect_equal(sort(read_data$value), 1:50)
  }

  # cleanup
  unlink(parquet_base_dir, recursive = TRUE)
})
