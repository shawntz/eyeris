# Print plots in markdown format

Generates markdown code to display plots in the report.

## Usage

``` r
print_plots(plots, eye_suffix = NULL, eyeris = NULL)
```

## Arguments

- plots:

  Vector of plot file paths

- eye_suffix:

  Optional eye suffix for binocular data

- eyeris:

  Optional `eyeris` object used to annotate each run with the percent of
  data lost in its timeseries

## Value

A character string containing markdown plot references
