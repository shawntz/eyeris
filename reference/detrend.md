# Detrend the pupil time series

Linearly detrend_pupil data by fitting a linear model of
`pupil_data ~ time`, and return the fitted betas and the residuals
(`pupil_data - fitted_values`).

## Usage

``` r
detrend(eyeris, call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call. Defaults to `NULL`

## Value

An `eyeris` object with two new columns in `time series`:
`detrend_fitted_betas`, and `pupil_raw_{...}_detrend`

## Details

This function is automatically called by
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
if `detrend = TRUE`.

Users should prefer using
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

## Note

This function is part of the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Use `glassbox(detrend = TRUE)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
for the recommended way to run this step as part of the full `eyeris`
glassbox preprocessing pipeline

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
  plot(seed = 0)
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-13 02:18:07] [INFO] Processing block: block_1
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-06-13 02:18:07] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-13 02:18:07] [WARN] Skipping eyeris::bin() for block_1
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::detrend() for block_1
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-13 02:18:07] [INFO] Block processing summary:
#> ℹ [2026-06-13 02:18:07] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_detrend_z)
#> ✔ [2026-06-13 02:18:07] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-06-13 02:18:07] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1








```
