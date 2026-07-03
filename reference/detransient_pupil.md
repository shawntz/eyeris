# Internal function to remove transient artifacts from pupil data

This function implements transient artifact removal by identifying and
removing samples that exceed a speed-based threshold. The threshold is
computed based on the constant `n`, which defaults to the value `16`.

This function is called by the exposed wrapper
[`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md).

## Usage

``` r
detransient_pupil(x, prev_op, n, mad_thresh)
```

## Arguments

- x:

  A data frame containing pupil data with columns `time_secs` and the
  previous operation's pupil column

- prev_op:

  The name of the previous operation's pupil column

- n:

  The constant used to compute the median absolute deviation (MAD)
  threshold. Defaults to `16`

- mad_thresh:

  The threshold used to identify transient artifacts. Defaults to `NULL`

## Value

A numeric vector of the same length as the input data with transient
artifacts removed (set to NA)

## Details

The function works by:

- Calculating the speed of pupil changes using finite differences

- Identifying samples that exceed a speed-based threshold

- Removing these samples from the pupil data
