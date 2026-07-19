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
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

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
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
by default. If needed, customize the parameters for `lpfilt` by
providing a parameter list. Use `glassbox(lpfilt = FALSE)` to disable
this step as needed.

Users should prefer using
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

When the input contains gaps longer than the interpolation limit (see
[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md)'s
`max_gap_ms`) that were left as `NA`, those gaps are temporarily filled
so the Butterworth filter can run and then masked back to `NA`. This can
slightly bias the valid samples immediately adjacent to each gap toward
the interpolated values, so a warning is emitted in this case. If that
bias is a concern, consider disabling this step
(`glassbox(lpfilt = FALSE)`).

## Note

This function is part of the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `lpfilt = list(...)`.

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

demo_data |>
  # set lpfilt to FALSE (instead of a list of params) to skip step
  eyeris::glassbox(lpfilt = list(plot_freqz = TRUE)) |>
  plot(seed = 0)
#> ✔ [2026-07-19 05:51:22] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-07-19 05:51:22] [OKAY] Running eyeris::resample()
#> ℹ [2026-07-19 05:51:23] [INFO] Processing block: block_1
#> ✔ [2026-07-19 05:51:23] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-19 05:51:23] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-19 05:51:23] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-07-19 05:51:23] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-07-19 05:51:23] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-19 05:51:23] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-19 05:51:23] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-07-19 05:51:23] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-19 05:51:23] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-19 05:51:23] [INFO] Block processing summary:
#> ℹ [2026-07-19 05:51:23] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-07-19 05:51:23] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-19 05:51:23] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1






```
