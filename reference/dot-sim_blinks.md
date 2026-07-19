# Stamp blink artifacts (missing cores + occlusion flank spikes) onto a signal

Stamp blink artifacts (missing cores + occlusion flank spikes) onto a
signal

## Usage

``` r
.sim_blinks(
  pupil,
  centers,
  core_samps,
  flank_samps,
  depth,
  overshoot,
  shape = "linear"
)
```

## Arguments

- pupil:

  Numeric pupil vector to modify

- centers:

  Integer sample indices of blink centers

- core_samps:

  Integer vector of blink core durations in samples

- flank_samps:

  Blink flank duration in samples

- depth:

  Leading occlusion dip depth (a.u.)

- overshoot:

  Trailing recovery overshoot height (a.u.)

- shape:

  Flank ramp shape: `"linear"` or `"cosine"`

## Value

A list with the modified `pupil`, a logical `core_mask` (TRUE over
missing cores), a logical `region_mask` (TRUE over cores + flanks), and
a data frame `df` of blink `start`/`end` sample indices
