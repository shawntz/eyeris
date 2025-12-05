# Internal function to detrend pupil data

This function detrends pupil data by fitting a linear model of
`pupil_data ~ time`, and returning the fitted betas and the residuals
(`pupil_data - fitted_values`).

This function is called by the exposed wrapper
[`detrend()`](https://shawnschwartz.com/eyeris/reference/detrend.md).

## Usage

``` r
detrend_pupil(x, prev_op)
```

## Arguments

- x:

  A data frame containing pupil data with columns `time_secs` and the
  previous operation's pupil column

- prev_op:

  The name of the previous operation's pupil column

## Value

A list containing the fitted values, coefficients, and residuals
