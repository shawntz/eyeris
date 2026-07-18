# Resample a single block onto a uniform time grid

Places one block's samples onto the expected uniform sampling grid:
interpolates values for sub-period timing jitter and inserts `NA` rows
at longer-than-expected gaps (dropped samples).

## Usage

``` r
resample_block(block_df, block_label = NULL, verbose = TRUE)
```

## Arguments

- block_df:

  A single block's timeseries data frame (must contain a `time_orig`
  column in milliseconds)

- block_label:

  Optional character label used in messages

- verbose:

  A flag to indicate whether to print detailed logging messages

## Value

The block data frame placed on a uniform grid, with inserted (gap) rows
marked in a logical `is_resampled` column. Returned unchanged (with no
`is_resampled` column added) when no reconstruction is needed.

## Details

This function is called by the exposed wrapper
[`resample()`](https://eyeris.shawnschwartz.com/reference/resample.md).
