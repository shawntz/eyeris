# Epoch using a start and an end message (explicit timestamps)

This function epochs data using a start and an end message (i.e.,
explicit timestamps).

## Usage

``` r
epoch_start_end_msg(eyeris, start, end, hz, verbose)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- start:

  A data frame containing the start timestamps

- end:

  A data frame containing the end timestamps

- hz:

  Sampling rate in Hz

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

A list containing epoch results

## Details

This function is called by the internal `epoch_start_end_msg()`
function.
