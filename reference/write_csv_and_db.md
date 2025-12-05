# Write data to CSV and/or database (helper function)

This helper function writes data to CSV files and/or database based on
the configuration. Useful for large-scale cloud compute where CSV files
may be unnecessary when using database storage.

## Usage

``` r
write_csv_and_db(
  data,
  csv_path,
  csv_enabled = TRUE,
  db_con = NULL,
  data_type = NULL,
  sub = NULL,
  ses = NULL,
  task = NULL,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame to write

- csv_path:

  Full path where CSV file should be written (ignored if csv_enabled =
  FALSE)

- csv_enabled:

  Whether to write CSV files (defaults to TRUE for backward
  compatibility)

- db_con:

  Database connection (NULL if not enabled)

- data_type:

  Type of data ("timeseries", "epochs", "epoch_timeseries",
  "epoch_summary", "events", "blinks", "confounds")

- sub:

  Subject ID

- ses:

  Session ID

- task:

  Task name

- run:

  Run number (optional)

- eye_suffix:

  Eye suffix for binocular data (optional)

- epoch_label:

  Epoch label for epoched data (optional, used in table naming)

- verbose:

  Whether to print verbose output

## Value

Logical indicating success
