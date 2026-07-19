# Detrend the pupil time series

Detrend pupil data by fitting a model of `pupil_data ~ time` and
returning the fitted values (the estimated trend) together with the
residuals (`pupil_data - fitted_values`). Two trend models are supported
via the `method` argument: `"linear"` (the default) removes a
straight-line drift by fitting an ordinary linear model, while
`"spline"` removes a smooth, potentially nonlinear drift by fitting a
natural cubic spline basis of `time`
(`splines::ns(time, df = spline_df)`).

## Usage

``` r
detrend(
  eyeris,
  method = c("linear", "spline"),
  spline_df = 5,
  call_info = NULL
)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- method:

  A string indicating the detrending model to fit. Either `"linear"`
  (the default) to remove a straight-line trend by regressing pupil size
  on time, or `"spline"` to remove a smooth, potentially nonlinear trend
  by fitting a natural cubic spline basis of time
  (`splines::ns(time, df = spline_df)`)

- spline_df:

  The degrees of freedom for the natural cubic spline basis used when
  `method = "spline"`. Higher values allow the fitted trend to follow
  more rapid, nonlinear drift; lower values enforce a smoother trend.
  Must be a single whole number `>= 1`. Defaults to `5`. Ignored when
  `method = "linear"`

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call. Defaults to `NULL`

## Value

An `eyeris` object with two new columns in `time series`:
`detrend_fitted_values`, and `pupil_raw_{...}_detrend`

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
Use `glassbox(detrend = TRUE)` for linear detrending, or
`glassbox(detrend = list(method = "spline"))` for spline detrending.

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

# (a) linear detrending (the default)
demo_data |>
  eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
  plot(seed = 0)
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::resample()
#> ℹ [2026-07-19 05:47:58] [INFO] Processing block: block_1
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-07-19 05:47:58] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-19 05:47:58] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-19 05:47:58] [WARN] Skipping eyeris::bin() for block_1
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::detrend() for block_1
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-19 05:47:58] [INFO] Block processing summary:
#> ℹ [2026-07-19 05:47:58] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_detrend_z)
#> ✔ [2026-07-19 05:47:58] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-19 05:47:58] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1









# (b) spline detrending (removes a smooth, nonlinear trend)
demo_data |>
  eyeris::glassbox(detrend = list(method = "spline", spline_df = 5)) |>
  plot(seed = 0)
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::resample()
#> ℹ [2026-07-19 05:48:01] [INFO] Processing block: block_1
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-07-19 05:48:01] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-19 05:48:01] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-19 05:48:01] [WARN] Skipping eyeris::bin() for block_1
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::detrend() for block_1
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-19 05:48:01] [INFO] Block processing summary:
#> ℹ [2026-07-19 05:48:01] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_detrend_z)
#> ✔ [2026-07-19 05:48:01] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-19 05:48:02] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1








```
