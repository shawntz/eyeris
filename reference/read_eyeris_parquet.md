# Read parquet files back into R

Convenience function to read the parquet files created by
eyeris_db_to_parquet back into a single data frame or list of data
frames by data type.

## Usage

``` r
read_eyeris_parquet(
  parquet_dir,
  db_name = NULL,
  data_type = NULL,
  return_list = FALSE,
  pattern = "*.parquet",
  verbose = TRUE
)
```

## Arguments

- parquet_dir:

  Directory containing the parquet files, or path to database-specific
  folder

- db_name:

  Optional database name to read from (if parquet_dir contains multiple
  database folders)

- data_type:

  Optional data type to read (if NULL, reads all data types)

- return_list:

  Whether to return a list by data type (TRUE) or combined data frame
  (FALSE, default)

- pattern:

  Pattern to match parquet files (default: "\*.parquet")

- verbose:

  Whether to print progress messages (default: TRUE)

## Value

Combined data frame from all parquet files, or list of data frames by
data type

## Examples

``` r
# \donttest{
# Minimal self-contained example that avoids database creation
if (requireNamespace("arrow", quietly = TRUE)) {
  # create a temporary folder structure: parquet/<db_name>
  base_dir <- file.path(tempdir(), "derivatives", "parquet")
  db_name <- "example-db"
  dir.create(file.path(base_dir, db_name), recursive = TRUE, showWarnings = FALSE)

  # write two small parquet parts for a single data type
  part1 <- data.frame(time = 1:5, value = 1:5)
  part2 <- data.frame(time = 6:10, value = 6:10)
  arrow::write_parquet(
    part1,
    file.path(
      base_dir, db_name, paste0(db_name, "_timeseries_part-01-of-02.parquet")
    )
  )
  arrow::write_parquet(
    part2,
    file.path(
      base_dir, db_name, paste0(db_name, "_timeseries_part-02-of-02.parquet")
    )
  )

  # read them back as combined data frame
  data <- read_eyeris_parquet(base_dir, db_name = db_name)

  # read as list by data type
  data_by_type <- read_eyeris_parquet(base_dir, db_name = db_name, return_list = TRUE)

  # read specific data type only
  timeseries_data <- read_eyeris_parquet(base_dir, db_name = db_name, data_type = "timeseries")
}
#> ℹ [2026-07-03 02:19:11] [INFO] Found 2 parquet files
#> ℹ [2026-07-03 02:19:11] [INFO] Read
#> example-db_timeseries_part-01-of-02.parquet: 5 rows
#> ℹ [2026-07-03 02:19:11] [INFO] Read
#> example-db_timeseries_part-02-of-02.parquet: 5 rows
#> ℹ [2026-07-03 02:19:11] [INFO] Combining 2 files...
#> ✔ [2026-07-03 02:19:11] [OKAY] Successfully read 10 total rows from parquet
#> files
#> ℹ [2026-07-03 02:19:11] [INFO] Found 2 parquet files
#> ℹ [2026-07-03 02:19:11] [INFO] Read
#> example-db_timeseries_part-01-of-02.parquet: 5 rows
#> ℹ [2026-07-03 02:19:11] [INFO] Read
#> example-db_timeseries_part-02-of-02.parquet: 5 rows
#> ℹ [2026-07-03 02:19:11] [INFO] Combined timeseries: 10 total rows
#> ℹ [2026-07-03 02:19:11] [INFO] Found 2 parquet files
#> ℹ [2026-07-03 02:19:11] [INFO] Read
#> example-db_timeseries_part-01-of-02.parquet: 5 rows
#> ℹ [2026-07-03 02:19:11] [INFO] Read
#> example-db_timeseries_part-02-of-02.parquet: 5 rows
#> ℹ [2026-07-03 02:19:11] [INFO] Combining 2 files...
#> ✔ [2026-07-03 02:19:11] [OKAY] Successfully read 10 total rows from parquet
#> files
# }
```
