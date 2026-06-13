# Slice a fixed time window out of a time series for previewing

Extracts the rows of `df` whose `time_secs` fall within
`[start_secs, start_secs + d)` and (re)computes a `time_scaled` column
(in milliseconds, relative to the window start). Used to draw a
full-resolution preview epoch over the same time window selected on the
decimated data.

## Usage

``` r
slice_epoch_window(df, start_secs, d)
```

## Arguments

- df:

  A data frame containing a `time_secs` column

- start_secs:

  The start of the window in seconds

- d:

  The window duration in seconds

## Value

A data frame containing the rows that fall within the window
