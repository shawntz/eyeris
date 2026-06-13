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
#> ✔ [2026-06-13 02:19:22] [OKAY] Connected to eyeris database:
#> /tmp/RtmpooO0Ae/derivatives/my-project.eyerisdb
#> ℹ [2026-06-13 02:19:22] [INFO] Executing query: SELECT * FROM (SELECT * FROM
#> "timeseries_001_01_assocret_run01" UNION ALL SELECT * FROM
#> "timeseries_001_01_assocret_run03") as combined_data WHERE 1=1 AND subject_id =
#> '001'
#> ℹ [2026-06-13 02:19:22] [INFO] Disconnected from eyeris database
# }
```
