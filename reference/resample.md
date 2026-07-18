# Resample an irregularly-sampled pupil timeseries onto a uniform grid

Places each recording segment onto the expected uniform
(consistently-spaced) sampling grid: local sub-period timing jitter is
smoothed by linear interpolation, and longer-than-expected gaps (dropped
samples) become explicit `NA` rows for the later
[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md)
step to handle.

## Usage

``` r
resample(eyeris, verbose = TRUE, call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- verbose:

  A flag to indicate whether to print detailed logging messages.
  Defaults to `TRUE`

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call

## Value

An `eyeris` object whose `timeseries` blocks have been placed on a
uniform time grid, with a new logical `is_resampled` column marking
inserted (gap) rows.

## Details

Most of the `eyeris` pipeline (e.g.,
[`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md),
[`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md),
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md))
assumes a fixed sampling rate. EyeLink trackers honor that assumption by
zero-filling missing pupil samples, but some hardware instead *drops*
samples entirely when pupil data is missing, leaving holes in the
otherwise evenly-spaced time vector. Those holes silently distort any
rate-dependent step.

`resample()` repairs the **time axis** in two stages. Whether a block
needs repair is decided by the robust
[`check_uniform_sampling_intervals()`](https://eyeris.shawnschwartz.com/reference/check_uniform_sampling_intervals.md)
detector, which distinguishes genuine dropped samples from data that
only *looks* irregular – notably high-rate trackers that report
integer-millisecond timestamps for sub-millisecond samples (these are
left untouched, so genuine samples are never collapsed). For blocks it
does repair:

1.  **Build the target grid.** The uniform grid is anchored on the first
    *reliable* regular interval – the first observed interval that
    matches the expected sampling period – rather than on the first
    timestamp. This keeps early sub-period jitter (e.g., intervals of 3,
    3, 4, 4 ms at a 4 ms period) from offsetting the whole grid. The
    grid is then extended in both directions at the expected period so
    that it spans every observed timestamp, including any samples that
    precede the anchor (back-extension).

2.  **Resample onto the grid.** Observed samples are placed on the grid
    by linear interpolation: samples that land on a grid point are kept
    verbatim, and short/jittered intervals are interpolated across so
    their values contribute to the regular grid. Any observed interval
    longer than the expected period is treated as a real gap: the
    missing grid sample(s) inside it are inserted as `NA` rather than
    interpolated across, and flagged in a new logical `is_resampled`
    column so they can be tracked downstream.

`resample()` does **not** fill the inserted `NA` values itself; that is
the job of
[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md),
which decides how much of a missing span to fill according to its own
missing-data policy. Running `resample()` therefore turns the
"dropped-sample" problem into the ordinary "missing-value" (`NA`)
problem that the rest of the pipeline already handles.

For data that is already uniformly sampled (e.g., EyeLink), `resample()`
is a guaranteed no-op: no rows are inserted, no `is_resampled` column is
added, and the data is returned unchanged. The same is true of a block
whose surviving samples already form a uniform (coarser) grid – e.g.,
pure systematic decimation – where there is nothing to insert without
fabricating data.

## Note

This step is part of the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
preprocessing pipeline and runs **automatically by default** (it is a
no-op unless irregular sampling is detected). Opt out with
`glassbox(resample = FALSE)`. Advanced users may call it directly if
needed.

## See also

[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md)
for filling the gaps left by dropped samples, and
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
for the recommended way to run this step as part of the full `eyeris`
glassbox preprocessing pipeline.
