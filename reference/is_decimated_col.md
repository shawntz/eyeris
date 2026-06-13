# Identify decimated (downsample/bin) pupil columns

A pupil column is considered "decimated" if it was produced by the
[`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md)/[`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md)
step or by any step that follows it. Such columns are stored at the
decimated sampling rate, whereas earlier columns should be plotted from
the preserved full-resolution data (see issue \#294).

## Usage

``` r
is_decimated_col(col)
```

## Arguments

- col:

  A character vector of pupil column names

## Value

A logical vector that is `TRUE` for decimated columns
