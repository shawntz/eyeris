# Epoch based on a single event message (without explicit limits)

This function epochs data based on a single event message (i.e., without
explicit limits).

## Usage

``` r
epoch_only_start_msg(eyeris, start, hz, verbose)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- start:

  A data frame containing the start timestamps

- hz:

  Sampling rate in Hz

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A list containing epoch results

## Details

This function is called by the internal `epoch_only_start_msg()`
function.
