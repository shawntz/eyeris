# Create progressive preprocessing summary plot

Internal function to create a comprehensive visualization showing the
progressive effects of preprocessing steps on pupil data. This plot
displays multiple preprocessing stages overlaid on the same time series,
allowing users to see how each step modifies the pupil signal.

## Usage

``` r
make_prog_summary_plot(
  pupil_data,
  pupil_steps,
  preview_n = 3,
  plot_params = list(),
  run_id = "run-01",
  cex = 2,
  eye_suffix = NULL,
  full_pupil_data = NULL
)
```

## Arguments

- pupil_data:

  A data frame containing pupil time series data with multiple
  preprocessing columns (e.g., `eyeris$timeseries$block_1`)

- pupil_steps:

  Character vector of column names containing pupil data at different
  preprocessing stages (e.g.,
  `c("pupil_raw", "pupil_deblink", "pupil_detrend")`)

- preview_n:

  Number of columns for subplot layout. Defaults to `3`

- plot_params:

  Named list of additional parameters to forward to plotting functions.
  Defaults to [`list()`](https://rdrr.io/r/base/list.html)

- run_id:

  Character string identifying the run/block (e.g., "run-01"). Used for
  plot titles and file naming. Defaults to `"run-01"`

- cex:

  Character expansion factor for plot elements. Defaults to `2.0`

- eye_suffix:

  Optional eye suffix for binocular data

- full_pupil_data:

  Optional data frame containing the full-resolution (pre-decimation)
  pupil time series, with the same `pupil_*` and `time_secs` columns as
  `pupil_data` (e.g., `eyeris$timeseries_pre_decimation$block_1`). When
  supplied, preprocessing steps that precede a
  [`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md)/[`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md)
  step are drawn from this full-resolution data instead of the decimated
  `pupil_data`, so they are not shown at the decimated sampling rate.
  Defaults to `NULL`

## Value

NULL (invisibly). Creates a plot showing progressive preprocessing
effects with multiple layers overlaid on the same time series

## Details

This function creates a two-panel visualization:

- Top panel: Overlaid time series showing progressive preprocessing
  effects with different colors for each step

- Bottom panel: Legend identifying each preprocessing step

The plot excludes z-scored data (columns ending with "\_z") and only
includes steps with sufficient valid data points (\>100). Each
preprocessing step is displayed with a distinct color, making it easy to
see how the signal changes through the pipeline.

## See also

[`plot.eyeris`](https://shawnschwartz.com/eyeris/reference/plot.eyeris.md)
