# Compute baseline correction for epoch data

Applies baseline correction to epoch data using either subtractive or
divisive methods.

## Usage

``` r
compute_baseline(
  x,
  epochs,
  baseline_epochs,
  mode,
  epoch_events = NULL,
  baseline_events = NULL
)
```

## Arguments

- x:

  An `eyeris` object containing the latest pupil column pointer

- epochs:

  A list of epoch data frames

- baseline_epochs:

  A list of baseline epoch data frames

- mode:

  The baseline correction mode ("sub" for subtractive, "div" for
  divisive)

- epoch_events:

  Event messages for epochs (optional)

- baseline_events:

  Event messages for baselines (optional)

## Value

A list containing baseline correction results and metadata
