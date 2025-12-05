# Internal function to remove blink artifacts from pupil data

This function implements blink artifact removal by extending the
duration of detected blinks (missing samples) by a specified number of
milliseconds both forward and backward in time. This helps to remove
deflections in pupil size that occur due to eyelid movements during and
around actual blink periods.

This function is called by the exposed wrapper
[`deblink()`](https://shawnschwartz.com/eyeris/reference/deblink.md).

## Usage

``` r
deblink_pupil(x, prev_op, extend)
```

## Arguments

- x:

  A data frame containing pupil data with columns `time_orig` and the
  previous operation's pupil column

- prev_op:

  The name of the previous operation's pupil column

- extend:

  Either a single number indicating symmetric padding in both
  directions, or a vector of length 2 indicating asymmetric padding in
  the format `c(backward, forward)` in milliseconds. Defaults to `50`

## Value

A numeric vector of the same length as the input data with blink
artifacts removed (set to NA)

## Details

The function works by:

- Identifying missing samples as blink periods

- Extending these periods by the specified number of milliseconds

- Setting all samples within the extended blink periods to NA

- Preserving all other samples unchanged

This implementation is based on the approach described in the
pupillometry package by dr-JT
(<https://github.com/dr-JT/pupillometry/blob/main/R/pupil_deblink.R>).
