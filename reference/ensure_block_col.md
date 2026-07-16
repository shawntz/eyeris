# Ensure an events/blinks data frame carries a block column

Adds a `block` column to an events or blinks data frame so that it can
be split into the same per-block structure as the time series. If the
data span a single block, every row is assigned to that block; otherwise
rows are assigned to whichever block's timestamp range contains them.
Routing fails (with an informative error) when a timestamp matches no
block or falls within more than one overlapping block range – supply an
explicit `block` column to resolve the ambiguity.

## Usage

``` r
ensure_block_col(df, time_col, raw_df)
```

## Arguments

- df:

  An events or blinks data frame.

- time_col:

  Name of the timestamp column to match against block ranges.

- raw_df:

  The assembled timeseries data frame (with a `block` column).

## Value

`df` with a `block` column added (if not already present).
