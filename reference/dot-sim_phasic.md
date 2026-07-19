# Build the additive phasic (task-evoked) pupil response component

Build the additive phasic (task-evoked) pupil response component

## Usage

``` r
.sim_phasic(t, onsets_s, amps, n, t_max)
```

## Arguments

- t:

  Numeric vector of sample times in seconds

- onsets_s:

  Numeric vector of stimulus onset times in seconds

- amps:

  Numeric vector of per-onset peak amplitudes (a.u.)

- n:

  Erlang shape parameter

- t_max:

  Time-to-peak in seconds

## Value

A numeric vector the same length as `t`
