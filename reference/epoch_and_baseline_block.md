# Block-by-block epoch and baseline handler

This function processes a single block of pupil data to extract epochs
and optionally compute and apply baseline corrections. It handles the
core epoching and baselining logic for a single block of data.

## Usage

``` r
epoch_and_baseline_block(
  x,
  blk,
  lab,
  evs,
  lims,
  msg_s,
  msg_e,
  c_bline,
  a_bline,
  bline_type,
  bline_evs,
  bline_per,
  hz,
  verbose
)
```

## Arguments

- x:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- blk:

  A list containing block metadata

- lab:

  Label for the epoch output

- evs:

  Events specification for epoching (character vector or list)

- lims:

  Time limits for epochs (numeric vector)

- msg_s:

  Start message string

- msg_e:

  End message string

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

This function is called by the internal
[`epoch_pupil()`](https://shawnschwartz.com/eyeris/reference/epoch_pupil.md)
function.
