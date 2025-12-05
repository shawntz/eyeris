# Clean up individual image files in run directories after HTML generation

Removes PNG and JPG files from the root of source/figures/run-xx
directories after all HTML reports have been generated. This cleans up
loose image files that may have been created during report generation.

## Usage

``` r
cleanup_run_dir_images(report_path, eye_suffix = NULL, verbose = FALSE)
```

## Arguments

- report_path:

  Path to the report directory containing source/figures/

- eye_suffix:

  Optional eye suffix for binocular data

- verbose:

  Whether to print verbose output

## Value

Invisibly returns TRUE if cleanup was successful, FALSE otherwise
