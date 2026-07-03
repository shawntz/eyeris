# Build a single-series pupil time series panel

Draws `y` against `x` as a `reaborn` line plot in `color`, shading any
missing-sample gaps, and applies the supplied titles/labels.

## Usage

``` r
rb_timeseries_panel(
  x,
  y,
  color = "#377EB8",
  title = NULL,
  xlab = "time (ms)",
  ylab = "pupil size"
)
```

## Arguments

- x:

  Numeric x positions (e.g. time)

- y:

  Numeric y values (e.g. pupil size); `NA` marks missing samples

- color:

  Line colour

- title:

  Panel title

- xlab, ylab:

  Axis labels

## Value

A `ggplot` object
