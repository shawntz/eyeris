args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
source(file.path(dirname(script), "load-package.R"))
options(device = function(...) grDevices::pdf(file = NULL))
emit <- function(phase) {
  cat("\n@@EYERIS@@", jsonlite::toJSON(list(phase = phase), auto_unbox = TRUE), "\n", sep = "")
  flush(stdout())
}
tryCatch({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install the R package jsonlite.")
  config <- jsonlite::fromJSON(args[1], simplifyVector = TRUE)
  load_eyeris()
  emit("glassbox")
  x <- do.call(eyeris::glassbox, c(list(file = config$input, interactive_preview = FALSE, verbose = TRUE), config$glassbox))
  if (!is.null(config$epoch)) {
    emit("epoch")
    x <- do.call(eyeris::epoch, c(list(eyeris = x, verbose = TRUE), config$epoch))
  }
  emit("bidsify")
  do.call(eyeris::bidsify, c(list(eyeris = x, bids_dir = config$bids,
    participant_id = config$subject, session_num = config$session, task_name = config$task,
    verbose = TRUE, csv_enabled = TRUE, db_enabled = config$database, db_path = "eyeris",
    html_report = config$report, save_raw = TRUE), list()))
  saveRDS(x, config$output)
  provenance <- list(R = as.character(getRversion()), eyeris = as.character(utils::packageVersion("eyeris")),
    session = capture.output(sessionInfo()), completed = format(Sys.time(), tz = "UTC", usetz = TRUE))
  jsonlite::write_json(provenance, file.path(dirname(config$output), "runtime.json"), auto_unbox = TRUE, pretty = TRUE)
  emit("complete")
}, error = function(e) {
  message("Pipeline failed: ", conditionMessage(e))
  quit(status = 1)
})
