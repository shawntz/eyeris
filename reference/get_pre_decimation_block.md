# Retrieve the preserved full-resolution data for a block

Returns the full-resolution (pre-decimation) time series for the
requested block, if it was preserved when a
[`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md)/[`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md)
step ran. Used so that diagnostic plots of earlier pipeline steps can be
rendered at their original sampling rate rather than the decimated rate
(see issue \#294).

## Usage

``` r
get_pre_decimation_block(x, block)
```

## Arguments

- x:

  An object of class `eyeris`

- block:

  The block number to retrieve

## Value

A data frame of full-resolution time series data for the block, or
`NULL` if no pre-decimation data was preserved
