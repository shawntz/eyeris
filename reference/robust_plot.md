# Robust plotting function with error handling

Builds a single-series pupil time series panel with `reaborn`, handling
errors and missing data gracefully. Returns a `ggplot` object (rather
than drawing directly) so callers can combine several panels into a
single `patchwork` row before printing to the active device.

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

  Additional arguments; `col`, `main`, `xlab`, and `ylab` control the
  line colour, title, and axis labels (any base-graphics style arguments
  such as `type`/`lwd` are accepted and ignored)

## Value

A `ggplot` object
