# Create gaze heatmap of eye coordinates

Creates a heatmap showing the distribution of eye_x and eye_y
coordinates across the entire screen area. The heatmap shows where the
participant looked most frequently during the recording period.

## Usage

``` r
plot_gaze_heatmap(
  eyeris,
  block = 1,
  screen_width = NULL,
  screen_height = NULL,
  n_bins = 50,
  col_palette = "viridis",
  main = "Gaze Heatmap",
  xlab = "Screen X (pixels)",
  ylab = "Screen Y (pixels)",
  sample_rate = NULL,
  eye_suffix = NULL
)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- block:

  Block number to plot (default: 1)

- screen_width:

  Screen width in pixels from `eyeris$info$screen.x`

- screen_height:

  Screen height in pixels from `eyeris$info$screen.y`

- n_bins:

  Number of bins for the heatmap grid (default: 50)

- col_palette:

  Color palette for the heatmap (default: "viridis")

- main:

  Title for the plot (default: "Fixation Heatmap")

- xlab:

  X-axis label (default: "Screen X (pixels)")

- ylab:

  Y-axis label (default: "Screen Y (pixels)")

- sample_rate:

  Sample rate in Hz (optional)

- eye_suffix:

  Eye suffix for binocular data (default: NULL)

## Value

No return value; creates a heatmap plot

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()
eyeris_preproc <- glassbox(demo_data)
#> ✔ [2026-06-13 06:55:50] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-13 06:55:50] [INFO] Processing block: block_1
#> ✔ [2026-06-13 06:55:50] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-13 06:55:50] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-13 06:55:51] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-13 06:55:51] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-06-13 06:55:51] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-13 06:55:51] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-13 06:55:51] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-13 06:55:51] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-13 06:55:51] [INFO] Block processing summary:
#> ℹ [2026-06-13 06:55:51] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-06-13 06:55:51] [OKAY] Running eyeris::summarize_confounds()
plot_gaze_heatmap(eyeris = eyeris_preproc, block = 1)

```
