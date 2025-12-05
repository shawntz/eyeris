# Check start and end timestamps are balanced

Validates that start and end timestamp data frames have the same number
of rows.

## Usage

``` r
check_start_end_timestamps(start, end)
```

## Arguments

- start:

  The start timestamp data frame

- end:

  The end timestamp data frame

## Value

No return value; throws error if timestamps are unbalanced
