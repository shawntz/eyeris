# Epoch using a start message with fixed limits around it

This function epochs data using a start message with fixed limits around
it.

## Usage

``` r
epoch_start_msg_and_limits(eyeris, start, lims, hz, verbose)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- start:

  A data frame containing the start timestamps

- lims:

  Time limits for epochs (numeric vector)

- hz:

  Sampling rate in Hz

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A list containing epoch results

## Details

This function is called by the internal `epoch_start_msg_and_limits()`
function.
