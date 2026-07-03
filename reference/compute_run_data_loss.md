# Compute percent data lost for a given run/block

Calculates the proportion of samples in the raw pupil timeseries that
are invalid (i.e., missing/during a blink, or off-screen) and expresses
it as a percentage. This surfaces data loss directly in the report to
reinforce workflow transparency.

## Usage

``` r
compute_run_data_loss(eyeris, run_num, eye_suffix = NULL)
```

## Arguments

- eyeris:

  An `eyeris` object containing preprocessing results

- run_num:

  Run identifier (numeric or character, e.g. `1` or `"01"`)

- eye_suffix:

  Optional eye suffix (e.g., "eye-L", "eye-R") used to select the
  correct eye from a binocular object

## Value

A numeric percentage in `[0, 100]`, or `NA_real_` when it cannot be
determined

## Details

Prefers the canonical `prop_invalid` metric stored in
`eyeris$confounds$unepoched_timeseries` (computed by
[`summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md)).
Falls back to computing the proportion of missing samples directly from
the raw timeseries when confounds are unavailable.
