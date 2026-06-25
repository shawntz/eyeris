# Downsample pupil time series with anti-aliasing filtering

This function downsamples pupillometry data by applying an anti-aliasing
filter before decimation. Unlike binning, downsampling preserves the
original temporal dynamics without averaging within bins.

## Usage

``` r
downsample(
  eyeris,
  target_fs,
  plot_freqz = FALSE,
  rp = 1,
  rs = 35,
  call_info = NULL
)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md).

- target_fs:

  The target sampling frequency in Hz after downsampling.

- plot_freqz:

  Boolean flag for displaying filter frequency response (default FALSE).

- rp:

  Passband ripple in dB (default 1).

- rs:

  Stopband attenuation in dB (default 35).

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call.

## Value

An `eyeris` object with downsampled data and updated sampling rate.

## Details

Downsampling reduces the sampling frequency by decimating data points.
The function automatically designs an anti-aliasing filter using the
[`lpfilt()`](https://shawnschwartz.com/eyeris/reference/lpfilt.md)
function with carefully chosen parameters:

- `ws` (stopband frequency) = Fs_new / 2 (Nyquist freq of new sampling
  rate)

- `wp` (passband frequency) = ws - max(5, Fs_nq \* 0.2)

- An error is raised if `wp < 4` to prevent loss of pupillary responses

The resulting time points will be: 0, 1/X, 2/X, 3/X, ..., etc. where X
is the new sampling frequency.

## Note

This function is part of the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `downsample = list(...)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
for the recommended way to run this step as part of the full `eyeris`
glassbox preprocessing pipeline.
[`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md) for binning
functionality.

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

# downsample pupil data recorded at 1000 Hz to 100 Hz with the default params
demo_data |>
  eyeris::glassbox(downsample = list(target_fs = 100)) |>
  plot(seed = 0)
#> ✔ [2026-06-25 04:50:44] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-25 04:50:45] [INFO] Processing block: block_1
#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::lpfilt() for block_1

#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::downsample() for block_1

#> ✔ [2026-06-25 04:50:45] [OKAY] Decimating sampling rate from 1000 Hz --> 100
#> Hz...
#> ! [2026-06-25 04:50:45] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-25 04:50:45] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-25 04:50:45] [INFO] Block processing summary:
#> ℹ [2026-06-25 04:50:45] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_downsample_z)
#> ✔ [2026-06-25 04:50:45] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-06-25 04:50:45] [INFO] Plotting block 1 with sampling rate 100 Hz from
#> possible blocks: 1







```
