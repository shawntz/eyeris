# Create temporary database for parallel processing

Creates a unique temporary database for use in parallel jobs to avoid
concurrency issues. The temporary database is named using process ID and
timestamp to ensure uniqueness.

## Usage

``` r
create_temp_eyeris_database(
  bids_dir,
  base_db_path = "my-project",
  verbose = FALSE
)
```

## Arguments

- bids_dir:

  Path to the BIDS directory containing derivatives

- base_db_path:

  Base database name (e.g., "my-project")

- verbose:

  Whether to print verbose output

## Value

List containing database connection and temp database path
