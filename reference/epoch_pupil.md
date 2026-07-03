# Main epoching and baselining logic

This function handles the core epoching and baselining operations for
pupil data. It processes time series data to extract epochs based on
specified events and optionally computes and applies baseline
corrections.

## Usage

``` r
epoch_pupil(
  x,
  prev_op,
  evs,
  lims,
  label,
  c_bline,
  a_bline,
  bline_type = c("sub", "div"),
  bline_evs,
  bline_per,
  hz,
  verbose
)
```

## Arguments

- x:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- prev_op:

  The name of the previous operation's output column

- evs:

  Events specification for epoching (character vector or list)

- lims:

  Time limits for epochs (numeric vector)

- label:

  Label for the epoch output

- c_bline:

  Logical indicating whether to calculate baseline

- a_bline:

  Logical indicating whether to apply baseline correction

- bline_type:

  Type of baseline correction ("sub" or "div")

- bline_evs:

  Events specification for baseline calculation

- bline_per:

  Baseline period specification

- hz:

  Sampling rate in Hz

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A list containing epoch and baseline results

## Details

This function is called by the exposed wrapper
[`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md).
