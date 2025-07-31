con <- eyeris_db_connect(
  file.path("~/Downloads", "newdbbaseline")
)

tables <- eyeris_db_list_tables(con)
tables

data <- eyeris_db_read(con, data_type = "blinks", subject = "001")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds_events", subject = "001", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds_summary", subject = "001", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "epoch_summary", subject = "001")
data |> View()

data <- eyeris_db_read(con, data_type = "epochs", subject = "001", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "events", subject = "001")
data |> View()

data <- eyeris_db_read(con, data_type = "run_confounds", subject = "001")
data |> View()

data <- eyeris_db_read(con, data_type = "timeseries", subject = "001")
data |> View()

# TODO: test baselining condition
# TODO: prevent duplicate entries (drop all entries for given subid beforehand)




eyeris_db_disconnect(con)
