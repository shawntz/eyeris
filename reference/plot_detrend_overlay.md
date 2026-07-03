# Internal helper to plot detrending overlay

This function replicates the exact detrending visualization from the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
interactive preview mode. It uses
[`robust_plot()`](https://eyeris.shawnschwartz.com/reference/robust_plot.md)
to show the most recent detrended pupil signal overlaid with the fitted
linear trend.

## Usage

``` r
plot_detrend_overlay(
  pupil_data,
  pupil_steps,
  preview_n = preview_n,
  plot_params = list(),
  suppress_prompt = TRUE
)
```

## Arguments

- pupil_data:

  A single block of pupil time series data (e.g.
  `eyeris$timeseries$block_1`)

- preview_n:

  Number of columns for `par(mfrow)`. Default = 3.

- plot_params:

  A named list of additional parameters to forward to
  [`robust_plot()`](https://eyeris.shawnschwartz.com/reference/robust_plot.md)

- suppress_prompt:

  Logical. Whether to skip prompting. Default = TRUE.

## Value

Logical indicating whether detrend overlay was plotted successfully
