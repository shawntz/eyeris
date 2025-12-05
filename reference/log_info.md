# Log an informational message

Log an informational message

## Usage

``` r
log_info(..., verbose = TRUE, wrap = TRUE, .envir = parent.frame())
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
log_info("Processing file:", "data.csv")
subject_id <- "001"
log_info("Processing subject {subject_id}")
log_info("Found {nrow(data)} rows", "in dataset")
} # }
```
