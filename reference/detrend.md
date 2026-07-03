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
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call. Defaults to `NULL`

## Value

An `eyeris` object with two new columns in `time series`:
`detrend_fitted_betas`, and `pupil_raw_{...}_detrend`

## Details

This function is automatically called by
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
if `detrend = TRUE`.

Users should prefer using
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

## Note

This function is part of the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Use `glassbox(detrend = TRUE)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
for the recommended way to run this step as part of the full `eyeris`
glassbox preprocessing pipeline

For a complete, end-to-end reference pipeline that demonstrates how all
`eyeris` preprocessing functions are chained together in practice, see
the "Building Blocks Under the Hood" section of the *Anatomy of an
`eyeris` Object* vignette —
[`vignette("anatomy", package = "eyeris")`](https://eyeris.shawnschwartz.com/articles/anatomy.md)
— as well as the *Complete Pupillometry Pipeline Walkthrough* vignette:
[`vignette("complete-pipeline", package = "eyeris")`](https://eyeris.shawnschwartz.com/articles/complete-pipeline.md).

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
  plot(seed = 0)
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-07-03 03:48:08] [INFO] Processing block: block_1
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-03 03:48:08] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-03 03:48:08] [WARN] Skipping eyeris::bin() for block_1
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::detrend() for block_1
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-03 03:48:08] [INFO] Block processing summary:
#> ℹ [2026-07-03 03:48:08] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_detrend_z)
#> ✔ [2026-07-03 03:48:08] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-03 03:48:08] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1








```
