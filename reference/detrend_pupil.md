# Internal function to detrend pupil data

This function detrends pupil data by fitting a model of
`pupil_data ~ time` and returning the fitted values (the estimated
trend) together with the residuals (`pupil_data - fitted_values`).

When `method = "linear"` (the default) the trend is a straight line fit
with `lm(pupil ~ time)`. When `method = "spline"` the trend is a natural
cubic spline of time fit with
`lm(pupil ~ splines::ns(time, df = spline_df))`, allowing a smooth
nonlinear drift to be removed.

This function is called by the exposed wrapper
[`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md).

## Usage

``` r
detrend_pupil(x, prev_op, method = "linear", spline_df = 5)
```

## Arguments

- x:

  A data frame containing pupil data with columns `time_secs` and the
  previous operation's pupil column

- prev_op:

  The name of the previous operation's pupil column

- method:

  A string indicating the detrending model to fit, either `"linear"`
  (the default) or `"spline"`

- spline_df:

  The degrees of freedom for the natural cubic spline basis used when
  `method = "spline"`. Defaults to `5`

## Value

A list containing the fitted values, coefficients, and residuals
