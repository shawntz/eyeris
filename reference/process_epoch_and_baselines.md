# Epoch and baseline processor

This function processes a single block of pupil data to extract epochs
and optionally compute and apply baseline corrections. It handles the
core epoching and baselining logic for a single block of data.

## Usage

``` r
process_epoch_and_baselines(eyeris, timestamps, evs, lims, hz, verbose)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- timestamps:

  A list containing start and end timestamps

- evs:

  Events specification for epoching (character vector or list)

- lims:

  Time limits for epochs (numeric vector)

- hz:

  Sampling rate in Hz

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A list containing epoch and baseline results

## Details

This function is called by the internal
[`epoch_and_baseline_block()`](https://eyeris.shawnschwartz.com/reference/epoch_and_baseline_block.md)
function.
