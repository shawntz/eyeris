# Calculate pupil speed using finite differences

Computes the speed of pupil changes using finite differences between
consecutive time points. This is a helper function for the detransient
step.

## Usage

``` r
speed(x, y)
```

## Arguments

- x:

  A numeric vector of pupil data

- y:

  A numeric vector of time data

## Value

A vector of pupil speeds at each time point
