skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")

temp_dir <- tempdir()
temp_bids_dir <- file.path(temp_dir, "test_bids")

# clean up any existing test files
if (dir.exists(temp_bids_dir)) {
  unlink(temp_bids_dir, recursive = TRUE)
}
dir.create(temp_bids_dir, recursive = TRUE)

# test 1: database connection and creation
test_that("database connection works", {
  # test database creation
  con <- connect_eyeris_database(temp_bids_dir, "test-db", verbose = FALSE)
  expect_s4_class(con, "duckdb_connection")

  # test database file exists
  db_path <- file.path(temp_bids_dir, "derivatives", "test-db.eyerisdb")
  expect_true(file.exists(db_path))

  disconnect_eyeris_database(con, verbose = FALSE)
})

# test 2: table creation and data writing
test_that("data writing to database works", {
  con <- connect_eyeris_database(temp_bids_dir, "test-db", verbose = FALSE)

  # create test data
  test_data <- data.frame(
    pupil_raw = c(1000, 1100, 1200),
    time_secs = c(0, 1, 2),
    trial = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  # test writing data
  result <- write_eyeris_data_to_db(
    data = test_data,
    con = con,
    data_type = "timeseries",
    sub = "001",
    ses = "01",
    task = "test",
    run = "01",
    eye_suffix = NULL,
    verbose = FALSE
  )

  expect_true(result)

  # check table was created
  tables <- DBI::dbListTables(con)
  expect_true("timeseries_001_01_test_run01" %in% tables)

  # read data back and verify
  written_data <- DBI::dbReadTable(con, "timeseries_001_01_test_run01")
  expect_equal(nrow(written_data), 3)
  expect_true("subject_id" %in% colnames(written_data))
  expect_true("session_id" %in% colnames(written_data))
  expect_true("task_name" %in% colnames(written_data))
  expect_true("data_type" %in% colnames(written_data))
  expect_true("run_number" %in% colnames(written_data))
  expect_true("created_timestamp" %in% colnames(written_data))

  # verify metadata was added correctly
  expect_equal(unique(written_data$subject_id), "001")
  expect_equal(unique(written_data$session_id), "01")
  expect_equal(unique(written_data$task_name), "test")
  expect_equal(unique(written_data$data_type), "timeseries")
  expect_equal(unique(written_data$run_number), "01")

  # verify original data columns exist
  expect_true("pupil_raw" %in% colnames(written_data))
  expect_true("time_secs" %in% colnames(written_data))
  expect_true("trial" %in% colnames(written_data))

  disconnect_eyeris_database(con, verbose = FALSE)
})

# test 3: user-facing database functions
test_that("user-facing database functions work", {
  # create a database with some data first
  con <- connect_eyeris_database(temp_bids_dir, "user-test", verbose = FALSE)

  test_data <- data.frame(
    pupil_raw = c(1000, 1100),
    matched_event = c("start", "stop"),
    stringsAsFactors = FALSE
  )

  write_eyeris_data_to_db(
    data = test_data,
    con = con,
    data_type = "epochs",
    sub = "002",
    ses = "02",
    task = "memory",
    run = "01",
    verbose = FALSE
  )

  disconnect_eyeris_database(con, verbose = FALSE)

  # test user connection function
  user_con <- eyeris_db_connect(temp_bids_dir, "user-test")
  expect_s4_class(user_con, "duckdb_connection")

  # test listing tables
  tables <- eyeris_db_list_tables(user_con)
  expect_true(length(tables) > 0)
  expect_true("epochs_002_02_memory_run01" %in% tables)

  # test reading data
  data <- eyeris_db_read(user_con, data_type = "epochs", subject = "002")
  expect_true(nrow(data) > 0)
  expect_equal(unique(data$subject_id), "002")
  expect_equal(unique(data$data_type), "epochs")

  # test filtering
  filtered_data <- eyeris_db_read(
    user_con,
    data_type = "epochs",
    subject = "002",
    session = "02",
    task = "memory"
  )
  expect_equal(nrow(filtered_data), 2)

  eyeris_db_disconnect(user_con)
})

# test 4: csv and database helper function
test_that("write_csv_and_db helper function works", {
  con <- connect_eyeris_database(temp_bids_dir, "helper-test", verbose = FALSE)

  test_data <- data.frame(
    value = c(100, 200, 300),
    condition = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  csv_path <- file.path(temp_dir, "test_data.csv")

  # test writing to both csv and database
  result <- write_csv_and_db(
    data = test_data,
    csv_path = csv_path,
    csv_enabled = TRUE,
    db_con = con,
    data_type = "confounds",
    sub = "003",
    ses = "01",
    task = "helper",
    run = "02",
    verbose = FALSE
  )

  expect_true(result)

  # check csv was created
  expect_true(file.exists(csv_path))
  csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(csv_data), 3)

  # check database table was created
  tables <- DBI::dbListTables(con)
  expect_true("confounds_003_01_helper_run02" %in% tables)

  # test csv-only mode
  csv_only_path <- file.path(temp_dir, "csv_only.csv")
  result2 <- write_csv_and_db(
    data = test_data,
    csv_path = csv_only_path,
    csv_enabled = TRUE,
    db_con = NULL, # no db connection
    verbose = FALSE
  )

  expect_true(result2)
  expect_true(file.exists(csv_only_path))

  # test database-only mode
  result3 <- write_csv_and_db(
    data = test_data,
    csv_path = file.path(temp_dir, "not_created.csv"),
    csv_enabled = FALSE, # no csv
    db_con = con,
    data_type = "events",
    sub = "004",
    ses = "01",
    task = "dbonly",
    verbose = FALSE
  )

  expect_true(result3)
  expect_false(file.exists(file.path(temp_dir, "not_created.csv")))

  # check database table was still created
  tables_final <- DBI::dbListTables(con)
  expect_true("events_004_01_dbonly" %in% tables_final)

  disconnect_eyeris_database(con, verbose = FALSE)
})

# test 5: database path handling and extensions
test_that("database path handling works correctly", {
  # test default path
  con1 <- connect_eyeris_database(temp_bids_dir, verbose = FALSE)
  expect_s4_class(con1, "duckdb_connection")
  expect_true(file.exists(file.path(
    temp_bids_dir,
    "derivatives",
    "my-project.eyerisdb"
  )))
  disconnect_eyeris_database(con1, verbose = FALSE)

  # test custom name (extension should be auto-added)
  con2 <- connect_eyeris_database(
    temp_bids_dir,
    "custom-study",
    verbose = FALSE
  )
  expect_s4_class(con2, "duckdb_connection")
  expect_true(file.exists(file.path(
    temp_bids_dir,
    "derivatives",
    "custom-study.eyerisdb"
  )))
  disconnect_eyeris_database(con2, verbose = FALSE)

  # test name with extension already present
  con3 <- connect_eyeris_database(
    temp_bids_dir,
    "already-has.eyerisdb",
    verbose = FALSE
  )
  expect_s4_class(con3, "duckdb_connection")
  expect_true(file.exists(file.path(
    temp_bids_dir,
    "derivatives",
    "already-has.eyerisdb"
  )))
  expect_false(file.exists(file.path(
    temp_bids_dir,
    "derivatives",
    "already-has.eyerisdb.eyerisdb"
  )))
  disconnect_eyeris_database(con3, verbose = FALSE)
})

# test 6: error handling
test_that("database error handling works", {
  # test connection to non-existent database
  expect_error(
    eyeris_db_connect(temp_bids_dir, "non-existent-db"),
    "No eyeris database found"
  )

  # test writing with no connection
  result <- write_eyeris_data_to_db(
    data = data.frame(x = 1),
    con = NULL,
    data_type = "test",
    sub = "001",
    ses = "01",
    task = "test",
    verbose = FALSE
  )
  expect_false(result)

  # test writing empty data
  con <- connect_eyeris_database(temp_bids_dir, "error-test", verbose = FALSE)
  result2 <- write_eyeris_data_to_db(
    data = data.frame(),
    con = con,
    data_type = "test",
    sub = "001",
    ses = "01",
    task = "test",
    verbose = FALSE
  )
  expect_false(result2)

  disconnect_eyeris_database(con, verbose = FALSE)
})

# test 7: epoch label in table names
test_that("epoch labels are included in table names", {
  con <- connect_eyeris_database(temp_bids_dir, "epoch-test", verbose = FALSE)

  test_data <- data.frame(
    pupil_raw = c(1000, 1100),
    matched_event = c("start", "stop"),
    stringsAsFactors = FALSE
  )

  # test epoched time series with epoch label
  result <- write_eyeris_data_to_db(
    data = test_data,
    con = con,
    data_type = "epoch_timeseries",
    sub = "005",
    ses = "01",
    task = "epochtest",
    run = "01",
    epoch_label = "prePostProbe",
    verbose = FALSE
  )

  expect_true(result)

  # check table was created with epoch label
  tables <- DBI::dbListTables(con)
  expect_true(
    "epoch_timeseries_005_01_epochtest_run01_prepostprobe" %in% tables
  )

  # verify data contains epoch label metadata
  table_data <- DBI::dbReadTable(
    con,
    "epoch_timeseries_005_01_epochtest_run01_prepostprobe"
  )
  expect_true("epoch_label" %in% colnames(table_data))
  expect_equal(unique(table_data$epoch_label), "prePostProbe")

  # test reading data with epoch label filter
  filtered_data <- eyeris_db_read(
    con,
    data_type = "epoch_timeseries",
    subject = "005",
    epoch_label = "prePostProbe"
  )
  expect_true(nrow(filtered_data) > 0)
  expect_equal(unique(filtered_data$epoch_label), "prePostProbe")
  expect_equal(unique(filtered_data$subject_id), "005")

  disconnect_eyeris_database(con, verbose = FALSE)
})

# clean up test files
if (dir.exists(temp_bids_dir)) {
  unlink(temp_bids_dir, recursive = TRUE)
}
