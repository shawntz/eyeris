# Bin pupil data into specified time bins

This function bins pupil data into specified time bins using either mean
or median aggregation. It creates evenly spaced bins across the time
series and aggregates pupil values within each bin.

## Usage

``` r
bin_pupil(x, prev_op, bins_per_second, method, current_fs)
```

## Arguments

- x:

  A data frame containing the pupil time series data

- prev_op:

  The name of the previous operation's output column

- bins_per_second:

  Number of bins per second (positive integer)

- method:

  Aggregation method: "mean" or "median"

- current_fs:

  Current sampling rate in Hz

## Value

A data frame with binned pupil data containing columns:

- `time_secs`: Bin center timestamps

- `pupil_binned_{method}_{bins_per_second}hz`: Binned pupil values

## Details

This function is called by the exposed wrapper
[`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md).
