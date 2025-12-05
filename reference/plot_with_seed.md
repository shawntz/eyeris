# Plot with seed handling for glassbox pipeline

Internal function to handle plotting with consistent seed management for
the glassbox pipeline interactive previews.

## Usage

``` r
plot_with_seed(
  file,
  step_counter,
  seed,
  preview_n,
  preview_duration,
  preview_window,
  only_linear_trend,
  next_step,
  block_name = NULL,
  verbose = TRUE
)
```

## Arguments

- file:

  The `eyeris` object to plot

- step_counter:

  Current step counter

- seed:

  A random seed for reproducible plotting

- preview_n:

  Number of preview epochs

- preview_duration:

  Duration of each preview in seconds

- preview_window:

  Preview window specification

- only_linear_trend:

  A flag to indicate whether to show only linear trend

- next_step:

  Next step information

- block_name:

  Block name (optional, for multi-block processing)

- verbose:

  A flag to indicate whether to show verbose output
