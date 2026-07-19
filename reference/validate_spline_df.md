# Validate the `spline_df` argument for spline detrending

Ensures `spline_df` (the degrees of freedom for the natural cubic spline
basis used by
[`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md)
when `method = "spline"`) is a single whole number `>= 1`, aborting with
an informative error otherwise.

## Usage

``` r
validate_spline_df(spline_df)
```

## Arguments

- spline_df:

  The candidate degrees-of-freedom value to validate

## Value

The validated value coerced to an integer
