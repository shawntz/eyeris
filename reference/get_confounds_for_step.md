# Calculate confounds for a single pupil data step

Computes various metrics from pupil data including:

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

A data frame containing confounds metrics for the current step
