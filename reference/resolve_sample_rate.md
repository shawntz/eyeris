# Resolve the sampling rate for a generic load

Returns the user-supplied sampling rate, or infers it from the median
spacing of the timestamps when not provided.

## Usage

``` r
resolve_sample_rate(sample_rate, time_ms, verbose)
```

## Arguments

- sample_rate:

  User-supplied sampling rate in Hz, or `NULL`.

- time_ms:

  Numeric vector of timestamps in milliseconds.

- verbose:

  Logical. Whether to print verbose output.

## Value

A positive numeric sampling rate in Hz.
