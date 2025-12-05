# Process large database query in chunks

Handles really large databases by processing queries in reasonably sized
chunks to avoid memory issues. Data can be written to CSV or Parquet
files as it's processed.

## Usage

``` r
process_chunked_query(
  con,
  query,
  chunk_size = 1e+06,
  output_file = NULL,
  process_chunk = NULL,
  verbose = TRUE
)
```

## Arguments

- con:

  Database connection

- query:

  SQL query string to execute

- chunk_size:

  Number of rows to fetch per chunk (default: 1000000)

- output_file:

  Optional output file path for writing chunks. If provided, chunks will
  be appended to this file. File format determined by extension (.csv or
  .parquet)

- process_chunk:

  Optional function to process each chunk. Function should accept a
  data.frame and return logical indicating success. If not provided and
  output_file is specified, chunks are written to file.

- verbose:

  Whether to print progress messages (default: TRUE)

## Value

List containing summary information about the chunked processing

## Examples

``` r
if (FALSE) { # \dontrun{
# These examples require an existing eyeris database

con <- eyeris_db_connect("/path/to/bids", "my-project")

# Process large query and write to CSV
process_chunked_query(
  con,
  "SELECT * FROM large_table WHERE condition = 'something'",
  chunk_size = 50000,
  output_file = "large_export.csv"
)

# Process large query with custom chunk processing
process_chunked_query(
  con,
  "SELECT * FROM large_table",
  chunk_size = 25000,
  process_chunk = function(chunk) {
    # Custom processing here
    processed_data <- some_analysis(chunk)
    return(TRUE)
  }
)

eyeris_db_disconnect(con)
} # }
```
