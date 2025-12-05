# Cleanup temporary database

Safely disconnects and removes temporary database files after successful
merge.

## Usage

``` r
cleanup_temp_database(temp_db_info, verbose = FALSE)
```

## Arguments

- temp_db_info:

  List containing temp database connection and paths

- verbose:

  Whether to print verbose output

## Value

Logical indicating success
