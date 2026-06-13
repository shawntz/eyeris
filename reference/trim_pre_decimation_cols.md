# Retain only the columns needed to plot pre-decimation pipeline steps

Subsets a block of time series data down to the timing columns and pupil
signal columns. This trimmed, full-resolution copy is stashed in
`eyeris$timeseries_pre_decimation` before a
[`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md)/[`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md)
step replaces the working time series with its decimated counterpart, so
that diagnostic plots of earlier steps can be shown at the original
sampling rate rather than the decimated rate (see issue \#294).

## Usage

``` r
trim_pre_decimation_cols(data)
```

## Arguments

- data:

  A single block of time series data (a data frame)

## Value

A data frame containing the timing columns (`block`, `time_orig`,
`time_secs`, `time_scaled`) that are present plus all `pupil_*` columns.
A `time_scaled` column mirroring `time_secs` is added if it is missing.
