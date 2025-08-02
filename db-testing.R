con <- eyeris_db_connect(
  file.path("~/Downloads", "debugging")
)

eyeris::eyeris_extract_data(file.path("~/Downloads", "debugging")) -> pop
pop |> View()

tables <- eyeris_db_list_tables(con)
tables

data <- eyeris_db_read(con, data_type = "blinks", subject = "059")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds_events", subject = "059")#, epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds_summary", subject = "059")#, epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "epoch_summary", subject = "059")
data |> View()

data <- eyeris_db_read(con, data_type = "epochs", subject = "059")#, epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "events", subject = "059")
data |> View()

data <- eyeris_db_read(con, data_type = "run_confounds", subject = "059")
data |> View()

data <- eyeris_db_read(con, data_type = "timeseries", subject = "059")
data |> View()




eyeris_db_disconnect(con)
