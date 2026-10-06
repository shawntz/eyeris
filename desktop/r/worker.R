args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
script <- gsub("~+~", " ", script, fixed=TRUE)
source(file.path(dirname(script), "review.R"))
lib <- Sys.getenv("EYERIS_PACKAGE_LIBRARY")
if (nzchar(lib) && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install the R package jsonlite to use eyeris Desktop.")
input <- file("stdin", "r")
cached_path <- NULL
cached_object <- NULL
get_object <- function(path) {
  if (!identical(path, cached_path)) {
    next_object <- readRDS(path)
    review_objects(next_object)
    cached_object <<- next_object
    cached_path <<- path
  }
  cached_object
}
repeat {
  line <- readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  request <- NULL
  response <- tryCatch({
    request <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    p <- request$params
    result <- switch(request$method,
      ping = list(r = as.character(getRversion()), eyeris = if (requireNamespace("eyeris", quietly = TRUE)) as.character(utils::packageVersion("eyeris")) else NULL),
      index = review_index(get_object(p$path)),
      trace = review_trace(get_object(p$path), p$epoch, p$stage, p$range),
      export = review_export_source(get_object(p$path), p$epochs, p$destination),
      stop("Unknown worker method.")
    )
    list(id = request$id, result = result)
  }, error = function(e) list(id = request$id, error = conditionMessage(e)))
  cat(jsonlite::toJSON(response, auto_unbox = TRUE, null = "null", na = "null", digits = NA), "\n", sep = "")
  flush(stdout())
}
