# Internal helper to plot detrending overlay

This function replicates the exact detrending visualization from the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
interactive preview mode. It uses `reaborn` to show the most recent
pre-detrend pupil signal overlaid with the fitted trend, and prints the
resulting `ggplot` to the active device.

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

- pupil_steps:

  Character vector of pupil column names

- preview_n:

  Unused; retained for backwards compatibility.

- plot_params:

  Unused; retained for backwards compatibility.

- suppress_prompt:

  Logical. Whether to skip prompting. Default = TRUE.

## Value

Logical indicating whether detrend overlay was plotted successfully
