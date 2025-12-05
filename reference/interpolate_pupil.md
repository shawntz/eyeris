# Interpolate missing pupil data using linear interpolation

This function fills missing values (NAs) in pupil data using linear
interpolation. It uses the
[`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html)
function with settings optimized for pupillometry data.

## Usage

``` r
interpolate_pupil(x, prev_op, verbose)
```

## Arguments

- x:

  A data frame containing the pupil time series data

- prev_op:

  The name of the previous operation's output column

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A vector of interpolated pupil values with the same length as the input

## Details

This function is called by the exposed wrapper
[`interpolate()`](https://shawnschwartz.com/eyeris/reference/interpolate.md).
