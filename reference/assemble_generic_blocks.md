# Split standardized data frames into per-block eyeris structures

Mirrors the block-handling logic of
[`process_eyeris_data()`](https://eyeris.shawnschwartz.com/reference/process_eyeris_data.md)
for the generic loader, producing the named per-block lists for time
series, events, and blinks.

## Usage

``` r
assemble_generic_blocks(raw_df, events_df, blinks_df, block)
```

## Arguments

- raw_df:

  Assembled timeseries data frame with a `block` column.

- events_df:

  Standardized events data frame with a `block` column.

- blinks_df:

  Standardized blinks data frame with a `block` column.

- block:

  Block specification (`"auto"`, `NULL`, or numeric).

## Value

A list with `timeseries`, `events`, and `blinks` named per-block lists.
