# Bin pupil time series by averaging within time bins

This function bins pupillometry data by dividing time into equal
intervals and averaging the data within each bin. Unlike downsampling,
binning averages data points within each time bin.

## Usage

``` r
bin(eyeris, bins_per_second, method = "mean", call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- bins_per_second:

  The number of bins to create per second of data

- method:

  The binning method: "mean" (default) or "median"

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call. Defaults to `NULL`

## Value

An `eyeris` object with binned data and updated sampling rate

## Details

Binning divides one second of pupillary data into X bins and averages
pupillometry data around each bin center. The resulting time points will
be: 1/2X, 3/2X, 5/2X, ..., etc. where X is the number of bins per
second.

This approach is commonly used in pupillometry research to study
temporal dynamics of pupil dilatory response; however, it should be used
with caution (as averaging within bins can distort the pupillary
dynamics).

## Note

This function is part of the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `bin = list(...)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
for the recommended way to run this step as part of the full eyeris
glassbox preprocessing pipeline
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md)
for downsampling functionality

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

# bin data into 10 bins per second using the (default) "mean" method
demo_data |>
  eyeris::glassbox(bin = list(bins_per_second = 10, method = "mean")) |>
  plot(seed = 0)
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::load_asc()
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::resample()
#> ℹ [2026-07-19 05:47:35] [INFO] Processing block: block_1
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-07-19 05:47:35] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-19 05:47:35] [WARN] Skipping eyeris::downsample() for block_1
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::bin() for block_1
#> ✔ [2026-07-19 05:47:35] [OKAY] Decimating sampling rate from 1000 Hz --> 10
#> Hz...
#> ! [2026-07-19 05:47:35] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-19 05:47:35] [INFO] Block processing summary:
#> ℹ [2026-07-19 05:47:35] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_bin_z)
#> ✔ [2026-07-19 05:47:35] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-19 05:47:35] [INFO] Plotting block 1 with sampling rate 10 Hz from
#> possible blocks: 1






```
