# Calculate confounds for a single pupil data step

Computes various metrics from pupil data including:

- Missing data (count and proportion of `NA` samples)

- Blink detection

- Gaze on/off screen detection

- Gap analysis

- Gaze distance from screen center

- Gaze variance

- Blink rate

- Blink duration

- Blink time

## Usage

``` r
get_confounds_for_step(pupil_df, pupil_vec, screen_width, screen_height, hz)
```

## Arguments

- pupil_df:

  A data frame containing pupil data

- pupil_vec:

  A vector of pupil data for the current step

- screen_width:

  The screen width in pixels

- screen_height:

  The screen height in pixels

- hz:

  The sampling rate in Hz

## Value

A data frame containing confounds metrics for the current step,
including `n_missing` and `prop_missing` (the count and proportion of
missing/`NA` samples)

## Details

Missing data is reported as both a raw count (`n_missing`) and a
proportion (`prop_missing`, ranging from `0` to `1`) of samples in the
window for which the pupil signal is `NA` (e.g., blinks and signal
dropout prior to interpolation). Multiply `prop_missing` by `100` to
obtain a percentage. This is distinct from `prop_invalid`, which
additionally folds in samples flagged as blinks or off-screen gaze.
