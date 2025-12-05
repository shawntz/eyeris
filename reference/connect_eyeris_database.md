# Create or connect to eyeris project database

Creates a new `DuckDB` database for the `eyeris` project or connects to
an existing one. The database will be created in the BIDS derivatives
directory. When parallel processing is enabled, creates temporary
databases to avoid concurrency issues.

## Usage

``` r
connect_eyeris_database(
  bids_dir,
  db_path = "my-project",
  verbose = FALSE,
  parallel = FALSE
)
```

## Arguments

- bids_dir:

  Path to the BIDS directory containing derivatives

- db_path:

  Database name (defaults to "my-project", becomes
  "my-project.eyerisdb")

- verbose:

  Whether to print verbose output

- parallel:

  Whether to enable parallel processing with temporary databases

## Value

DBI database connection object or temp database info list (when
parallel=TRUE)
