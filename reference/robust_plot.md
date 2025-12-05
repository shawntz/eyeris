# Robust plotting function with error handling

A wrapper around base plotting functions that handles errors and missing
data gracefully.

## Usage

``` r
robust_plot(y, x = NULL, ...)
```

## Arguments

- y:

  The y-axis data to plot

- x:

  The x-axis data (optional, defaults to sequence)

- ...:

  Additional arguments passed to plot()

## Value

No return value; creates a plot or displays warning messages
