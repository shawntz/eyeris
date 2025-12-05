# Read eyeris data from database

Reads `eyeris` data from the project database with dplyr-style
interface.

## Usage

``` r
eyeris_db_read(
  con,
  data_type = NULL,
  subject = NULL,
  session = NULL,
  task = NULL,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL,
  table_name = NULL
)
```

## Arguments

- con:

  Database connection

- data_type:

  Type of data to read ("timeseries", "epochs", "epoch_timeseries",
  "epoch_summary", "events", "blinks")

- subject:

  Optional subject ID filter

- session:

  Optional session ID filter

- task:

  Optional task name filter

- run:

  Optional run number filter

- eye_suffix:

  Optional eye suffix filter

- epoch_label:

  Optional epoch label filter (for epoched data)

- table_name:

  Exact table name (overrides other parameters)

## Value

Data frame with requested data
