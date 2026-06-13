# Lowpass filtering of time series data

The intended use of this method is for smoothing, although by specifying
`wp` and `ws` differently one can achieve highpass or bandpass filtering
as well. However, only lowpass filtering should be done on pupillometry
data.

## Usage

``` r
lpfilt(
  eyeris,
  wp = 4,
  ws = 8,
  rp = 1,
  rs = 35,
  plot_freqz = FALSE,
  call_info = NULL
)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- wp:

  The end of passband frequency in Hz (desired lowpass cutoff). Defaults
  to `4`

- ws:

  The start of stopband frequency in Hz (required lowpass cutoff).
  Defaults to `8`

- rp:

  Required maximal ripple within passband in dB. Defaults to `1`

- rs:

  Required minimal attenuation within stopband in dB. Defaults to `35`

- plot_freqz:

  A flag to indicate whether to display the filter frequency response.
  Defaults to `FALSE`

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call. Defaults to `NULL`

## Value

An `eyeris` object with a new column in `time series`:
`pupil_raw_{...}_lpfilt`

## Details

This function is automatically called by
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
by default. If needed, customize the parameters for `lpfilt` by
providing a parameter list. Use `glassbox(lpfilt = FALSE)` to disable
this step as needed.

Users should prefer using
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

## Note

This function is part of the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `lpfilt = list(...)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
for the recommended way to run this step as part of the full eyeris
glassbox preprocessing pipeline

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
  # set lpfilt to FALSE (instead of a list of params) to skip step
  eyeris::glassbox(lpfilt = list(plot_freqz = TRUE)) |>
  plot(seed = 0)
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-13 07:38:28] [INFO] Processing block: block_1
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-06-13 07:38:28] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-13 07:38:28] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-13 07:38:28] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-13 07:38:28] [INFO] Block processing summary:
#> ℹ [2026-06-13 07:38:28] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-06-13 07:38:28] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-06-13 07:38:28] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1






```
