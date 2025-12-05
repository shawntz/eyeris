# Zip and cleanup source figure files

Creates zip files for all PNG and JPG files in each run directory under
source/figures/, then deletes the individual image files. This reduces
file count while preserving all figure data in compressed format.

## Usage

``` r
zip_and_cleanup_source_figures(report_path, eye_suffix = NULL, verbose = FALSE)
```

## Arguments

- report_path:

  Path to the report directory containing source/figures/

- eye_suffix:

  Optional eye suffix for binocular data

- verbose:

  Whether to print verbose output

## Value

List of created zip file paths
