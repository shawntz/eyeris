# Manually epoch using provided start/end data frames of timestamps

This function manually epochs data using provided start/end data frames
of timestamps.

## Usage

``` r
epoch_manually(eyeris, ts_list, hz, verbose)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- ts_list:

  A list containing start/end data frames of timestamps

- hz:

  Sampling rate in Hz

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A list containing epoch results

## Details

This function is called by the internal
[`process_epoch_and_baselines()`](https://shawnschwartz.com/eyeris/reference/process_epoch_and_baselines.md)
function.
