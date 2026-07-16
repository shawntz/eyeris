# Warn when sampling intervals are not uniform

Several `eyeris` steps assume a fixed sampling interval (an
EyeLink-style quirk). This emits a soft warning when consecutive
timestamps within a block are not uniformly spaced, which can indicate
dropped samples.

## Usage

``` r
warn_irregular_sampling(timeseries, verbose)
```

## Arguments

- timeseries:

  A named list of per-block time series data frames.

- verbose:

  Logical. Whether to print verbose output.

## Value

Invisibly `NULL`; called for its side effect (warning).
