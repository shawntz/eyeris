# Check for uniform (consecutive, equal) sampling intervals

Validates that consecutive samples in a timeseries are uniformly spaced.
Different eye trackers have hardware-specific quirks that violate the
uniform-sampling assumption baked into the `eyeris` pipeline. The most
relevant here is that some hardware *drops* samples when pupil data is
missing, rather than zero-filling the gap (as EyeLink does). Dropped
samples leave holes in the otherwise evenly spaced time vector, and
because downstream steps (e.g.,
[`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md),
[`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md),
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md))
assume a fixed sampling rate, those holes silently distort the results.
This guardrail surfaces the quirk early with an informative warning so
it can be addressed before preprocessing.

## Usage

``` r
check_uniform_sampling_intervals(
  time_vector,
  hz = NULL,
  blocks = NULL,
  tolerance = 0.5,
  block_label = NULL,
  verbose = TRUE
)
```

## Arguments

- time_vector:

  Numeric vector of sample timestamps (in milliseconds).

- hz:

  Optional known sampling rate in Hz (e.g., from the file header). Used
  to annotate the warning with the nominal rate and to cross-check for
  systematic dropout.

- blocks:

  Optional vector (same length as `time_vector`) identifying the
  recording segment each sample belongs to. When supplied, intervals are
  only compared *within* each segment.

- tolerance:

  Relative tolerance for the systematic-dropout cross-check (default
  `0.5`). A rate mismatch is flagged when the data-derived interval
  exceeds the nominal interval (`1000 / hz`) by more than this fraction.

- block_label:

  Optional character label used in the warning to identify the segment
  being checked (e.g., `"block_1"`).

- verbose:

  Logical. Whether to emit the warning message (default `TRUE`).

## Value

Invisibly, a list (or, when `blocks` is supplied, a list of such lists,
one per segment) summarizing the check with elements: `uniform`
(logical), `rate_mismatch` (logical; TRUE when the effective rate is
lower than the nominal `hz`), `expected_interval` (ms), `n_intervals`,
`n_irregular`, `n_missing_samples` (estimated number of dropped
samples), and `prop_irregular`. A warning is emitted via
[`log_warn()`](https://eyeris.shawnschwartz.com/reference/log_warn.md)
when irregular intervals or a rate mismatch are detected.

## Details

The expected inter-sample interval is inferred from the data as the
modal (most frequent) positive interval, which is robust both to a
minority of irregular intervals and to sub-millisecond timestamp
rounding at high sampling rates. Any nonzero interval not exactly equal
to this modal interval is marked irregular; zero-length intervals (the
tell-tale of integer-millisecond rounding of sub-millisecond samples)
are exempt. Intervals longer than the mode are additionally used to
estimate the number of dropped samples. When the timeseries spans
multiple recording segments (`blocks`), each segment is checked
independently so that the expected gap *between* segments is not
mistaken for a dropped sample.

Sporadic dropout leaves visible gaps, but *systematic* dropout (e.g.,
every Nth sample missing) leaves a uniform but coarser grid that the gap
check alone cannot see. When the nominal sampling rate (`hz`) is known,
it is cross-checked against the data-derived interval to surface this
case too, while avoiding false positives from high-rate trackers that
report integer-millisecond timestamps for sub-millisecond samples.
