# Log a warning message

Log a warning message

## Usage

``` r
log_warn(..., verbose = TRUE, wrap = TRUE, .envir = parent.frame())
```

## Arguments

- ...:

  Character strings to log. Supports glue-style interpolation.

- verbose:

  Logical. Whether to print the message (default TRUE).

- wrap:

  Logical. Whether to wrap long messages (default TRUE).

- .envir:

  Environment for glue interpolation (default: parent frame).

## Examples

``` r
if (FALSE) { # \dontrun{
log_warn("Missing data detected")
missing_count <- 10
log_warn("Found {missing_count} missing values")
} # }
```
