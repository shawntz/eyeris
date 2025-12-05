# Internal glassbox function for processing individual eyes

Internal glassbox function for processing individual eyes

## Usage

``` r
glassbox_internal(
  file,
  interactive_preview = FALSE,
  preview_n = 3,
  preview_duration = 5,
  preview_window = NULL,
  verbose = TRUE,
  params,
  original_call,
  seed
)
```

## Arguments

- file:

  The `eyeris` object to process

- interactive_preview:

  A flag to indicate whether to show interactive previews

- preview_n:

  Number of preview epochs

- preview_duration:

  Duration of each preview in seconds

- preview_window:

  Preview window specification

- verbose:

  A flag to indicate whether to show verbose output

- params:

  A list of pipeline step parameters

- original_call:

  The original call to the glassbox function

- seed:

  A random seed for reproducible plotting

## Value

An `eyeris` object with the processed data lists
