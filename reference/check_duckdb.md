# Check for DuckDB availability

This internal helper checks whether the duckdb package is installed. If
it is not available, a status message is displayed with
platform-specific installation instructions (macOS, Linux, Windows).
Functions that depend on DuckDB call this check before proceeding.

## Usage

``` r
check_duckdb()
```

## Value

`TRUE` if duckdb is installed, otherwise `FALSE` (with an informative
status message).
