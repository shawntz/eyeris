# Make a BIDS-compatible filename

Helper function to generate a BIDS-compatible filename based on the
provided parameters.

## Usage

``` r
make_bids_fname(
  sub_id,
  task_name,
  run_num,
  desc = "",
  ses_id = NULL,
  epoch_name = NULL,
  epoch_events = NULL,
  baseline_events = NULL,
  baseline_type = NULL,
  eye_suffix = NULL
)
```

## Arguments

- sub_id:

  The subject ID

- task_name:

  The task name

- run_num:

  The run number

- desc:

  The description

- ses_id:

  The session ID

- epoch_name:

  The epoch name

- epoch_events:

  The epoch events

- baseline_events:

  The baseline events

- baseline_type:

  The baseline type

- eye_suffix:

  The eye suffix

## Value

A BIDS-compatible filename
