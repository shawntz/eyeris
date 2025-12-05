# Extract baseline epochs from time series data

Extracts baseline periods from time series data based on event messages
and time ranges or start/end messages.

## Usage

``` r
extract_baseline_epochs(x, df, evs, time_range, matched_epochs, hz)
```

## Arguments

- x:

  An `eyeris` object containing the latest pupil column pointer

- df:

  The time series data frame

- evs:

  Event messages for baseline extraction

- time_range:

  Time range for baseline extraction

- matched_epochs:

  Matched epoch start/end times

- hz:

  Sampling rate in Hz

## Value

A list of baseline epoch data frames
