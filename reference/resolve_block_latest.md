# Resolve the `latest` pointer for a single block

On multi-block recordings the `latest` pointer of an `eyeris` object is
a named list with one column name per block (e.g., `block_1`, `block_2`,
...). Block-level helpers such as
[`compute_baseline()`](https://eyeris.shawnschwartz.com/reference/compute_baseline.md)
need a single column name, so this returns the pointer for the requested
block.

## Usage

``` r
resolve_block_latest(latest, block_name)
```

## Arguments

- latest:

  The `latest` pointer from an `eyeris` object (a character string or a
  named list of character strings, one per block)

- block_name:

  The name of the block (e.g., `"block_1"`)

## Value

A single character string naming the latest pupil column for
`block_name`
