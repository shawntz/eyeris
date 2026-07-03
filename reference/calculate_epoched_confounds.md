# Calculate confounds for epoched data

Helper function to calculate confounds for epoched time series data.
This function is used internally by both
[`summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md)
and [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md).

## Usage

``` r
calculate_epoched_confounds(eyeris, epoch_names, hz, verbose = TRUE)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- epoch_names:

  A vector of epoch names to process

- hz:

  The sampling rate

- verbose:

  A flag to indicate whether to print progress messages

## Value

An updated `eyeris` object with epoched confounds
