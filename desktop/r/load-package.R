load_eyeris <- function() {
  lib <- Sys.getenv("EYERIS_PACKAGE_LIBRARY")
  repo <- Sys.getenv("EYERIS_SOURCE")
  if (nzchar(lib) && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  if (nzchar(repo) && file.exists(file.path(repo, "DESCRIPTION"))) {
    if (!requireNamespace("pkgload", quietly = TRUE)) stop("Development mode requires the R package pkgload.")
    pkgload::load_all(repo, quiet = TRUE)
  } else {
    if (!requireNamespace("eyeris", quietly = TRUE)) stop("Install eyeris and its R dependencies before processing.")
    suppressPackageStartupMessages(library(eyeris))
  }
}
