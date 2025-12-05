# Internal function to run bidsify on a single eye

Internal function to run bidsify on a single eye

## Usage

``` r
run_bidsify(
  eyeris,
  save_all = TRUE,
  epochs_list = NULL,
  bids_dir = NULL,
  participant_id = NULL,
  session_num = NULL,
  task_name = NULL,
  run_num = NULL,
  save_raw = TRUE,
  html_report = TRUE,
  report_seed = 0,
  report_epoch_grouping_var_col = "matched_event",
  eye_suffix = NULL,
  verbose = TRUE,
  csv_enabled = TRUE,
  db_enabled = FALSE,
  db_path = "my-project",
  parallel_processing = FALSE,
  raw_binocular_object = NULL,
  skip_db_cleanup = FALSE
)
```

## Arguments

- eyeris:

  An `eyeris` object

- save_all:

  Whether to save all data

- epochs_list:

  A list of epochs to include

- bids_dir:

  The directory to save the bids data

- participant_id:

  The participant id

- session_num:

  The session number

- task_name:

  The task name

- run_num:

  The run number

- save_raw:

  Whether to save raw data

- html_report:

  Whether to generate an html report

- report_seed:

  The seed for the report

- report_epoch_grouping_var_col:

  The column to use for grouping epochs in the report

- eye_suffix:

  The suffix to add to the eye data

- verbose:

  Whether to print verbose output

- csv_enabled:

  Whether to save csv files

- db_enabled:

  Whether to save data to the database

- db_path:

  The path to the database

- parallel_processing:

  Whether to enable parallel database processing

- raw_binocular_object:

  The raw binocular object

- skip_db_cleanup:

  Whether to skip database cleanup, used internally to avoid unintended
  overwriting when calling complementary binocular bidsify processing
  commands

## Value

An `eyeris` object
