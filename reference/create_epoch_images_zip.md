# Create zip file from epoch images

Creates a zip file containing epoch images instead of saving individual
files. This function collects all the image data in memory and then
creates a single zip file, which can be more efficient for the HTML
gallery display.

## Usage

``` r
create_epoch_images_zip(
  epochs_to_save,
  epoch_index,
  block_name,
  run_dir_num,
  epochs_out,
  pupil_steps,
  eyeris_object,
  eye_suffix = NULL,
  report_epoch_grouping_var_col = "matched_event",
  verbose = FALSE
)
```

## Arguments

- epochs_to_save:

  List of epoch data to save

- epoch_index:

  Index of the current epoch being processed

- block_name:

  Name of the current block being processed

- run_dir_num:

  Run directory number

- epochs_out:

  Output directory for the epoch files

- pupil_steps:

  Vector of pupil processing steps

- eyeris_object:

  The full `eyeris` object (needed for screen dimensions)

- eye_suffix:

  Optional eye suffix for binocular data

- report_epoch_grouping_var_col:

  Column name for grouping epochs

- verbose:

  Whether to print verbose output

## Value

Path to the created zip file
