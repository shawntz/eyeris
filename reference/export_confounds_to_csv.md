# Export confounds data to CSV files and/or database

Exports each block's confounds data to a separate CSV file and/or
database table. Each file will contain all pupil steps (e.g., pupil_raw,
pupil_clean) as rows, with confound metrics as columns.

## Usage

``` r
export_confounds_to_csv(
  confounds_list,
  output_dir,
  filename_prefix,
  verbose,
  run_num = NULL,
  csv_enabled = TRUE,
  db_con = NULL,
  sub = NULL,
  ses = NULL,
  task = NULL,
  eye_suffix = NULL,
  epoch_label = NULL
)
```

## Arguments

- confounds_list:

  A nested list structure containing confounds data

- output_dir:

  The directory where CSV files will be saved

- filename_prefix:

  Either a string prefix for filenames or a function that takes a block
  name and returns a prefix

- verbose:

  A flag to indicate whether to print progress messages

- run_num:

  The run number (if NULL, will be extracted from block names)

- csv_enabled:

  Whether to write CSV files (default TRUE)

- db_con:

  Database connection object (NULL if database disabled)

- sub:

  Subject ID for database metadata

- ses:

  Session ID for database metadata

- task:

  Task name for database metadata

- eye_suffix:

  Eye suffix for binocular data (e.g., "eye-L", "eye-R")

- epoch_label:

  Epoch label for epoched data (added as column)

## Value

Invisibly returns a vector of created file paths
