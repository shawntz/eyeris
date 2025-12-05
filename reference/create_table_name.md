# Create table name for eyeris data

Generates a standardized table name for `eyeris` data based on the data
type and subject information.

## Usage

``` r
create_table_name(
  data_type,
  sub,
  ses,
  task,
  run = NULL,
  eye_suffix = NULL,
  epoch_label = NULL
)
```

## Arguments

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

## Value

Character string with table name
