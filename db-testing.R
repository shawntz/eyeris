con <- eyeris_db_connect(

)

eyeris::eyeris_extract_data(file.path("~/Downloads", "debugging")) -> pop
pop |> View()

tables <- eyeris_db_list_tables(con)
tables

data <- eyeris_db_read(con, data_type = "blinks", subject = "056")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds_events", subject = "056", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "confounds_summary", subject = "056", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "epoch_summary", subject = "056")
data |> View()

data <- eyeris_db_read(con, data_type = "epochs", subject = "056", epoch_label = "prepostprobe")
data |> View()

data <- eyeris_db_read(con, data_type = "events", subject = "056")
data |> View()

data <- eyeris_db_read(con, data_type = "run_confounds", subject = "056")
data |> View()

data <- eyeris_db_read(con, data_type = "timeseries", subject = "056")
data |> View()




eyeris_db_disconnect(con)
