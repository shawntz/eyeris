# Notify the user (once per run) about the max-gap behavior change

Emits a one-time-per-run warning explaining that interpolation now
leaves gaps longer than `max_gap_ms` as `NA`, a change in default
behavior from `eyeris` versions \<= 3.2.0. The flag is cleared by
[`reset_gap_notices()`](https://eyeris.shawnschwartz.com/reference/reset_gap_notices.md),
which
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
calls at the start of each run.

## Usage

``` r
notify_max_gap_behavior_change(max_gap_ms, verbose = TRUE)
```

## Arguments

- max_gap_ms:

  The active maximum gap duration in milliseconds

- verbose:

  A flag to indicate whether to print the message

## Value

Invisibly returns `NULL`
