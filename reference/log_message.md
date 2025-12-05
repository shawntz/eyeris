# Core logging function with timestamp and glue support

Core logging function with timestamp and glue support

## Usage

``` r
log_message(level, ..., verbose = TRUE, wrap = TRUE, .envir = parent.frame())
```

## Arguments

- level:

  Character string for log level (INFO, OKAY, WARN, EXIT)

- ...:

  Character strings to log

- verbose:

  Logical. Whether to print the message

- wrap:

  Logical. Whether to wrap long messages

- .envir:

  Environment for glue interpolation
