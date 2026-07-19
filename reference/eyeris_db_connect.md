# Connect to eyeris project database (user-facing)

User-friendly function to connect to an existing `eyeris` project
database. This function provides easy access for users to query their
`eyeris` data.

## Usage

``` r
eyeris_db_connect(bids_dir, db_path = "my-project")
```

## Arguments

- bids_dir:

  Path to the BIDS directory containing the database

- db_path:

  Database name (defaults to "my-project", becomes
  "my-project.eyerisdb") If just a filename, will look in `derivatives/`
  directory. If includes path, will use as provided.

## Value

Database connection object for use with other eyeris database functions

## Examples

``` r
# \donttest{
# step 1: create a database using bidsify with db_enabled = TRUE
# (This example assumes you have already run bidsify to create a database)

# temp dir for testing
temp_dir <- tempdir()

# step 2: connect to eyeris DB (will fail gracefully if no DB exists)
tryCatch({
  con <- eyeris_db_connect(temp_dir)

  tables <- eyeris_db_list_tables(con)

  # read timeseries data for a specific subject
  data <- eyeris_db_read(con, data_type = "timeseries", subject = "001")

  # close connection when done
  eyeris_db_disconnect(con)
}, error = function(e) {
  message("No eyeris DB found - create one first with bidsify(db_enabled = TRUE)")
})
#> ✔ [2026-07-19 05:49:01] [OKAY] Connected to eyeris database:
#> /tmp/RtmpJ5QJn3/derivatives/my-project.eyerisdb
#> ℹ [2026-07-19 05:49:01] [INFO] Executing query: SELECT * FROM (SELECT
#> "subject_id", "session_id", "task_name", "data_type", "run_number",
#> "created_timestamp", "block", "time_orig", "time_secs", "time_scaled", "eye_x",
#> "eye_y", "eye", "hz", "type", "pupil_raw", "pupil_raw_deblink",
#> "pupil_raw_deblink_detransient", "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z" FROM
#> "timeseries_001_01_assocret_run01" UNION ALL SELECT "subject_id", "session_id",
#> "task_name", "data_type", "run_number", "created_timestamp", "block",
#> "time_orig", "time_secs", "time_scaled", "eye_x", "eye_y", "eye", "hz", "type",
#> "pupil_raw", "pupil_raw_deblink", "pupil_raw_deblink_detransient",
#> "pupil_raw_deblink_detransient_interpolate",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt",
#> "pupil_raw_deblink_detransient_interpolate_lpfilt_z" FROM
#> "timeseries_001_01_assocret_run03") as combined_data WHERE 1=1 AND subject_id =
#> '001'
#> ℹ [2026-07-19 05:49:01] [INFO] Disconnected from eyeris database
# }
```
