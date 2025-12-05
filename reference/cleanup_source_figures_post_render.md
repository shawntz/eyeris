# Clean up source figures after report generation

Removes the entire source/figures directory after the main HTML report
has been generated since all images are now embedded in the HTML as data
URLs.

## Usage

``` r
cleanup_source_figures_post_render(
  report_path,
  eye_suffix = NULL,
  verbose = FALSE
)
```

## Arguments

- report_path:

  Path to the report directory

- eye_suffix:

  Optional eye suffix for binocular data (unused but kept for
  compatibility)

- verbose:

  Whether to print verbose output

## Value

Invisibly returns TRUE if cleanup was successful, FALSE otherwise
