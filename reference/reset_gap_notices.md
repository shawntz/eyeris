# Reset the per-run gap-related notice flags

Clears the session flags used by
[`notify_max_gap_behavior_change()`](https://eyeris.shawnschwartz.com/reference/notify_max_gap_behavior_change.md)
and
[`warn_filter_over_gaps()`](https://eyeris.shawnschwartz.com/reference/warn_filter_over_gaps.md)
so that those notices fire at most once per
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
run (rather than once per R session).
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
calls this at the start of each run.

## Usage

``` r
reset_gap_notices()
```

## Value

Invisibly returns `NULL`
