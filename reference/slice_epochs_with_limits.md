# Slice epochs with explicit limits

Creates epochs using explicit time limits around a central timestamp.

## Usage

``` r
slice_epochs_with_limits(x_raw, cur_ts, lims, hz)
```

## Arguments

- x_raw:

  The raw time series data frame

- cur_ts:

  The central timestamp

- lims:

  Time limits in seconds (negative for before, positive for after)

- hz:

  Sampling rate in Hz

## Value

A data frame containing the epoch data
