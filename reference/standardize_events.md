# Standardize a user-supplied events data frame

Coerces an optional events data frame to the canonical `time`/`text`
columns used internally by `eyeris`, scaling timestamps to milliseconds.

## Usage

``` r
standardize_events(events, mapping, ms_scale)
```

## Arguments

- events:

  Optional events data frame (or `NULL`).

- mapping:

  Resolved column-name mapping list.

- ms_scale:

  Numeric factor to convert timestamps to milliseconds.

## Value

A data frame with `time` and `text` columns (zero-row if `events` is
`NULL`).
