# Log an error message and abort

Log an error message and abort

## Usage

``` r
log_error(..., wrap = TRUE, .envir = parent.frame())
```

## Arguments

- ...:

  Character strings to log. Supports glue-style interpolation.

- wrap:

  Logical. Whether to wrap long messages (default TRUE).

- .envir:

  Environment for glue interpolation (default: parent frame).

## Examples

``` r
if (FALSE) { # \dontrun{
log_error("Critical error occurred")
file_path <- "missing.csv"
log_error("File not found: {file_path}")
} # }
```
