# Check time series monotonicity

Validates that a time vector is monotonically increasing.

## Usage

``` r
check_time_monotonic(time_vector, time_col_name = "time_secs")
```

## Arguments

- time_vector:

  The time vector to check

- time_col_name:

  The name of the time column for error messages

## Value

No return value; throws error if time series is not monotonic
