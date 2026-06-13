# Interpolate missing pupil samples

Linear interpolation of time series data. The intended use of this
method is for filling in missing pupil samples (NAs) in the time series.
This method uses "na.approx()" function from the zoo package, which
implements linear interpolation using the "approx()" function from the
stats package. Currently, NAs at the beginning and the end of the data
are replaced with values on either end, respectively, using the "rule =
2" argument in the [`approx()`](https://rdrr.io/r/stats/approxfun.html)
function.

## Usage

``` r
interpolate(eyeris, verbose = TRUE, call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- verbose:

  A flag to indicate whether to print detailed logging messages.
  Defaults to `TRUE`. Set to `FALSE` to suppress messages about the
  current processing step and run silently

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call

## Value

An `eyeris` object with a new column in `timeseries`:
`pupil_raw_{...}_interpolate`

## Details

This function is automatically called by
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
by default. Use `glassbox(interpolate = FALSE)` to disable this step as
needed.

Users should prefer using
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

## Note

This function is part of the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Use `glassbox(interpolate = TRUE)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
for the recommended way to run this step as part of the full `eyeris`
glassbox preprocessing pipeline.

For a complete, end-to-end reference pipeline that demonstrates how all
`eyeris` preprocessing functions are chained together in practice, see
the "Building Blocks Under the Hood" section of the *Anatomy of an
`eyeris` Object* vignette —
[`vignette("anatomy", package = "eyeris")`](https://shawnschwartz.com/eyeris/articles/anatomy.md)
— as well as the *Complete Pupillometry Pipeline Walkthrough* vignette:
[`vignette("complete-pipeline", package = "eyeris")`](https://shawnschwartz.com/eyeris/articles/complete-pipeline.md).

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  # set to FALSE to skip (not recommended)
  eyeris::glassbox(interpolate = TRUE) |>
  plot(seed = 0)
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-13 07:37:47] [INFO] Processing block: block_1
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-06-13 07:37:47] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-13 07:37:47] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-13 07:37:47] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-13 07:37:47] [INFO] Block processing summary:
#> ℹ [2026-06-13 07:37:47] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-06-13 07:37:47] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-06-13 07:37:47] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1






```
