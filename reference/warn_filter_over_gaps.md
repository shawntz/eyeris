# Warn (once per run) that a filter/resampling step is operating over gaps

Emits a one-time-per-run warning explaining that a step which relies on
filtering (e.g.
[`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md) or
the anti-aliasing filter in
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md))
is operating over long gaps that `interpolate(max_gap_ms)` left as `NA`.
Because those gaps are temporarily filled to let the filter run and then
masked back to `NA`, the filter can slightly bias the valid samples
immediately adjacent to each gap toward the interpolated values. Users
may prefer to disable filtering and/or downsampling. The flag is cleared
by
[`reset_gap_notices()`](https://eyeris.shawnschwartz.com/reference/reset_gap_notices.md),
which
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
calls at the start of each run.

## Usage

``` r
warn_filter_over_gaps(step)
```

## Arguments

- step:

  Character label of the calling step (e.g. `"lpfilt"`, `"downsample"`),
  used both in the message and to fire the notice only once per run per
  step

## Value

Invisibly returns `NULL`
