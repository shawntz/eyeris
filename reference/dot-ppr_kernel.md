# Peak-normalized Hoeks & Levelt (1993) pupil response kernel

Peak-normalized Hoeks & Levelt (1993) pupil response kernel

## Usage

``` r
.ppr_kernel(tau, n, t_max)
```

## Arguments

- tau:

  Numeric vector of times (seconds) relative to stimulus onset

- n:

  Erlang shape parameter

- t_max:

  Time-to-peak in seconds

## Value

A numeric vector the same length as `tau`, zero for `tau < 0` and
peak-normalized to 1
