# Which eyes each recording has, read by eyeris::load_asc() itself, so the app
# offers only the eye choices the data supports. Reads [{id, path}] as JSON on
# stdin and writes one @@EYES@@ JSON line per recording as it is checked.
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
script <- gsub("~+~", " ", script, fixed = TRUE)
source(file.path(dirname(script), "load-package.R"))
options(device = function(...) grDevices::pdf(file = NULL))
load_eyeris()
recordings <- jsonlite::fromJSON(file("stdin"), simplifyDataFrame = FALSE)
recorded_eyes <- function(path) {
  # "both" keeps a binocular recording's eyes apart; one-eye data is returned
  # as that eye whatever the mode.
  x <- suppressWarnings(suppressMessages(
    eyeris::load_asc(path, binocular_mode = "both", verbose = FALSE)
  ))
  if (!is.null(x$left) && !is.null(x$right)) return("both")
  if (isTRUE(x$info$left[1])) return("left")
  if (isTRUE(x$info$right[1])) return("right")
  stop("eyeris found no left or right eye in this recording.")
}
for (recording in recordings) {
  result <- tryCatch(
    list(id = recording$id, eyes = recorded_eyes(recording$path)),
    error = function(e) list(id = recording$id, eyes = "unknown", error = conditionMessage(e))
  )
  cat("@@EYES@@", jsonlite::toJSON(result, auto_unbox = TRUE), "\n", sep = "")
  flush(stdout())
}
