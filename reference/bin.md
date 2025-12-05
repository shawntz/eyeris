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
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

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
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `bin = list(...)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
for the recommended way to run this step as part of the full eyeris
glassbox preprocessing pipeline
[`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md)
for downsampling functionality

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

# bin data into 10 bins per second using the (default) "mean" method
demo_data |>
  eyeris::glassbox(bin = list(bins_per_second = 10, method = "mean")) |>
  plot(seed = 0)
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::load_asc()
#> ℹ [2025-12-05 19:57:59] [INFO] Processing block: block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2025-12-05 19:57:59] [WARN] Skipping eyeris::downsample() for block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::bin() for block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Decimating sampling rate from 1000 Hz --> 10
#> Hz...
#> ! [2025-12-05 19:57:59] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2025-12-05 19:57:59] [INFO] Block processing summary:
#> ℹ [2025-12-05 19:57:59] [INFO] block_1: OK (steps: 7, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_bin_z)
#> ✔ [2025-12-05 19:57:59] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2025-12-05 19:57:59] [INFO] Plotting block 1 with sampling rate 10 Hz from
#> possible blocks: 1






```
