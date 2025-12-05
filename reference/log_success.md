# Log a success message

Log a success message

## Usage

``` r
log_success(..., verbose = TRUE, wrap = TRUE, .envir = parent.frame())
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
log_success("Processing completed successfully")
n_files <- 5
log_success("Processed {n_files} files successfully")
} # }
```
