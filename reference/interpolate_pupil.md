# Interpolate missing pupil data using linear interpolation

This function fills missing values (NAs) in pupil data using linear
interpolation. It uses the
[`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html)
function with settings optimized for pupillometry data.

## Usage

``` r
interpolate_pupil(x, prev_op, verbose, max_gap_ms = 250)
```

## Arguments

- x:

  A data frame containing the pupil time series data

- prev_op:

  The name of the previous operation's output column

- verbose:

  A flag to indicate whether to print detailed logging messages

- max_gap_ms:

  The maximum duration (in milliseconds) of a gap of missing (`NA`)
  samples to interpolate. Gaps longer than this are left as `NA`. Must
  be greater than `0`; use `Inf` or `NULL` to interpolate across all
  gaps. Defaults to `250`

## Value

A vector of interpolated pupil values with the same length as the input

## Details

Gaps longer than `max_gap_ms` milliseconds are left as `NA` rather than
interpolated. The threshold is converted from milliseconds to a number
of samples using the sampling period of the data (estimated from
`time_orig`), so it is robust to different sampling rates.

This function is called by the exposed wrapper
[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md).
