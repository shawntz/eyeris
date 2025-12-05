# Create eyeris report

Generates a comprehensive HTML report for `eyeris` preprocessing
results.

## Usage

``` r
make_report(eyeris, out, plots, eye_suffix = NULL, ...)
```

## Arguments

- eyeris:

  An `eyeris` object containing preprocessing results

- out:

  Output directory for the report

- plots:

  Vector of plot file paths to include in the report

- eye_suffix:

  Optional eye suffix (e.g., "eye-L", "eye-R") for binocular data

- ...:

  Additional parameters passed from bidsify

## Value

Path to the generated `R Markdown` file
