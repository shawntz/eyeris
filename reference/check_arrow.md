# Check for Arrow availability

This internal helper checks whether the arrow package is installed. If
it is not available, a status message is displayed with
platform-specific installation instructions. The arrow package is used
for efficient parquet file I/O. When not available, eyeris falls back to
DuckDB for parquet operations, which is slower but functional.

## Usage

``` r
check_arrow()
```

## Value

`TRUE` if arrow is installed, otherwise `FALSE` (with an informative
status message).
