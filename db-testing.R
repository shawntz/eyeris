con <- eyeris_db_connect(
  file.path("~/Downloads", "testdb")
)

tables <- eyeris_db_list_tables(con)
tables

data <- eyeris_db_read(con, data_type = "blinks", subject = "002")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds", subject = "002")
data |> View()

data <- eyeris_db_read(con, data_type = "epoch_summary", subject = "002")
data |> View()

data <- eyeris_db_read(con, data_type = "epoch_timeseries", subject = "002", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "epochs", subject = "002", epoch_label = "prepostprobe")
data |> View() # TODO: remove (duplicate table entry of epoch_timeseries)

data <- eyeris_db_read(con, data_type = "events", subject = "002")
data |> View()

data <- eyeris_db_read(con, data_type = "timeseries", subject = "002")
data |> View()

# TODO: missing sub-002_ses-01_task-assocret_run-01_desc-confounds_summary_PROBE_START_22_epoch-prepostprobe.csv
# TODO: missing sub-002_ses-01_task-assocret_run-01_desc-confounds_events_PROBE_START_22_epoch-prepostprobe.csv
# TODO: test baselining condition
# TODO: prevent duplicate entries (drop all entries for given subid beforehand)




eyeris_db_disconnect(con)
