# Internal function to z-score pupil data

This function z-scores pupil data by subtracting the mean and dividing
by the standard deviation.

This function is called by the exposed wrapper
[`zscore()`](https://shawnschwartz.com/eyeris/reference/zscore.md)

## Usage

``` r
zscore_pupil(x, prev_op)
```

## Arguments

- x:

  A data frame containing pupil data

- prev_op:

  The name of the previous operation in the pipeline

## Value

A vector of z-scored pupil data
