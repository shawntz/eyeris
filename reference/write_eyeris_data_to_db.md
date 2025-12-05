# Write eyeris data to database

Writes `eyeris` data to the project database as an alternative to CSV
files. Creates or updates tables as needed.

## Usage

``` r
write_eyeris_data_to_db(
  data,
  con,
  data_type,
  sub,
  ses,
  task,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL,
  append = TRUE,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame to write

- con:

  Database connection

- data_type:

  Type of data ("timeseries", "epochs", "epoch_timeseries",
  "epoch_summary", "events", "blinks")

- sub:

  Subject ID

- ses:

  Session ID

- task:

  Task name

- run:

  Run number

- eye_suffix:

  Optional eye suffix for binocular data

- epoch_label:

  Optional epoch label for epoched data (used in table naming)

- append:

  Whether to append to existing table (default TRUE)

- verbose:

  Whether to print verbose output

## Value

Logical indicating success
