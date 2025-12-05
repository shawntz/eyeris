# Save detrend plots for each block

Generates and saves detrend diagnostic plots for each block in the
`eyeris` object.

## Usage

``` r
save_detrend_plots(
  eyeris,
  out_dir,
  preview_n = 3,
  plot_params = list(),
  eye_suffix = NULL,
  verbose = TRUE
)
```

## Arguments

- eyeris:

  An `eyeris` object containing preprocessing results

- out_dir:

  Output directory for saving plots

- preview_n:

  Number of preview samples for plotting

- plot_params:

  Additional plotting parameters

- eye_suffix:

  Optional eye suffix for binocular data

- verbose:

  Logical. Whether to print verbose output (default TRUE).

## Value

No return value; saves detrend plots to the specified directory
