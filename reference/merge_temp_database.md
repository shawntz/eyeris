# Merge temporary database into main database

Safely merges data from a temporary database into the main project
database using file locking to prevent concurrent access issues. This
function handles the transactional copying of all tables from the
temporary database.

## Usage

``` r
merge_temp_database(
  temp_db_info,
  verbose = FALSE,
  max_retries = 10,
  retry_delay = 1
)
```

## Arguments

- temp_db_info:

  List containing temp database connection and paths

- verbose:

  Whether to print verbose output

- max_retries:

  Maximum number of retry attempts for file locking

- retry_delay:

  Delay between retry attempts in seconds

## Value

Logical indicating success
