# Standardize a user-supplied blinks data frame

Coerces an optional blinks data frame to the canonical `stime`/`etime`
columns used internally by `eyeris`, scaling timestamps to milliseconds.

## Usage

``` r
standardize_blinks(blinks, mapping, ms_scale)
```

## Arguments

- blinks:

  Optional blinks data frame (or `NULL`).

- mapping:

  Resolved column-name mapping list.

- ms_scale:

  Numeric factor to convert timestamps to milliseconds.

## Value

A data frame with `stime` and `etime` columns (zero-row if `blinks` is
`NULL`).
