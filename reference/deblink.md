# NA-pad blink events / missing data

Deblinking (a.k.a. NA-padding) of time series data. The intended use of
this method is to remove blink-related artifacts surrounding periods of
missing data. For instance, when an individual blinks, there are usually
rapid decreases followed by increases in pupil size, with a chunk of
data missing in-between these 'spike'-looking events. The deblinking
procedure here will NA-pad each missing data point by your specified
number of ms.

## Usage

``` r
deblink(eyeris, extend = 50, call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- extend:

  Either a single number indicating the number of milliseconds to pad
  forward/backward around each missing sample, or, a vector of length
  two indicating different numbers of milliseconds pad forward/backward
  around each missing sample, in the format `c(backward, forward)`

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call

## Value

An `eyeris` object with a new column: `pupil_raw_{...}_deblink`

## Details

This function is automatically called by
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
by default. If needed, customize the parameters for `deblink` by
providing a parameter list. Use `glassbox(deblink = FALSE)` to disable
this step as needed.

Users should prefer using
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

## Note

This function is part of the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `deblink = list(...)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
for the recommended way to run this step as part of the full eyeris
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

# 50 ms in both directions (the default)
demo_data |>
  eyeris::glassbox(deblink = list(extend = 50)) |>
  plot(seed = 0)
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-07-16 23:58:01] [INFO] Processing block: block_1
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-16 23:58:01] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-16 23:58:01] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-07-16 23:58:01] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-16 23:58:01] [INFO] Block processing summary:
#> ℹ [2026-07-16 23:58:01] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-07-16 23:58:01] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-16 23:58:01] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1







# 40 ms backward, 50 ms forward
demo_data |>
  # set deblink to FALSE (instead of a list of params)
  #  to skip step (not recommended)
  eyeris::glassbox(deblink = list(extend = c(40, 50))) |>
  plot(seed = 0)
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-07-16 23:58:04] [INFO] Processing block: block_1
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-16 23:58:04] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-16 23:58:04] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-07-16 23:58:04] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-16 23:58:04] [INFO] Block processing summary:
#> ℹ [2026-07-16 23:58:04] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-07-16 23:58:04] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-16 23:58:04] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1






```
