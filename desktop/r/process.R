args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
script <- gsub("~+~", " ", script, fixed=TRUE)
source(file.path(dirname(script), "load-package.R"))
options(device = function(...) grDevices::pdf(file = NULL))
emit <- function(phase, recording = NULL) {
  event <- list(phase = phase)
  event$recording <- recording
  cat("\n@@EYERIS@@", jsonlite::toJSON(event, auto_unbox = TRUE), "\n", sep = "")
  flush(stdout())
}
# Binocular objects keep each processed eye separately.
blocks <- function(x) length(if (inherits(x, "eyeris")) x$timeseries else x$left$timeseries)
tryCatch({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install the R package jsonlite.")
  # Keep each recording as a list, not a data frame row.
  config <- jsonlite::fromJSON(args[1], simplifyVector = TRUE, simplifyDataFrame = FALSE)
  load_eyeris()
  keys <- vapply(config$recordings, function(r) paste(r$subject, r$session, r$task), character(1))
  for (i in seq_along(config$recordings)) {
    recording <- config$recordings[[i]]
    emit("glassbox", i)
    x <- do.call(eyeris::glassbox, c(list(file = recording$input, interactive_preview = FALSE, verbose = TRUE), config$glassbox))
    # bidsify() ignores run_num when a recording has several blocks and numbers
    # each block as a run instead, overwriting the other runs written here.
    if (sum(keys == keys[i]) > 1 && blocks(x) > 1) {
      stop(sprintf(
        "%s contains %d recording blocks. eyeris numbers the blocks in one ASC as runs, so it cannot be processed with other runs of task-%s. Set a numeric load_asc block in Advanced glassbox options, or process this recording on its own.",
        basename(recording$input), blocks(x), recording$task
      ))
    }
    if (!is.null(config$epoch)) {
      emit("epoch", i)
      x <- do.call(eyeris::epoch, c(list(eyeris = x, verbose = TRUE), config$epoch))
    }
    emit("bidsify", i)
    do.call(eyeris::bidsify, list(eyeris = x, bids_dir = config$bids,
      participant_id = recording$subject, session_num = recording$session, task_name = recording$task,
      run_num = recording$run, verbose = TRUE, csv_enabled = TRUE, db_enabled = config$database,
      db_path = "eyeris", html_report = config$report, save_raw = TRUE))
    saveRDS(x, recording$output)
  }
  provenance <- list(R = as.character(getRversion()), eyeris = as.character(utils::packageVersion("eyeris")),
    session = capture.output(sessionInfo()), completed = format(Sys.time(), tz = "UTC", usetz = TRUE))
  jsonlite::write_json(provenance, file.path(dirname(config$bids), "runtime.json"), auto_unbox = TRUE, pretty = TRUE)
  emit("complete")
}, error = function(e) {
  message("Pipeline failed: ", conditionMessage(e))
  quit(status = 1)
})
