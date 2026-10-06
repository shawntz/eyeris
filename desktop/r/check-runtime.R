args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
script <- gsub("~+~", " ", script, fixed=TRUE)
source(file.path(dirname(script), "load-package.R"))
load_eyeris()
required <- c("jsonlite", "eyeris", "duckdb", "rmarkdown")
resources <- Sys.getenv("EYERIS_RESOURCE_DIR")
if (nzchar(resources)) {
  manifest <- jsonlite::fromJSON(file.path(resources, "runtime", "manifest.json"))
  stopifnot(as.character(getRversion()) == manifest$R)
  stopifnot(as.character(packageVersion("eyeris")) == manifest$eyeris)
  root <- paste0(normalizePath(resources, winslash="/", mustWork=TRUE), "/")
  paths <- normalizePath(c(R.home(), .libPaths()), winslash="/", mustWork=TRUE)
  if (any(!startsWith(tolower(paths), tolower(root)))) stop("Runtime isolation failed: an external R library is visible")
  required <- unique(c(required, names(manifest$packages)))
  for (pkg in names(manifest$packages)) {
    if (packageVersion(pkg) != package_version(manifest$packages[[pkg]])) stop("Dependency version mismatch: ",pkg)
  }
}
for (pkg in required) if (!requireNamespace(pkg, quietly=TRUE)) stop("Missing or incompatible dependency: ",pkg)
if (!rmarkdown::pandoc_available()) stop("Pandoc is unavailable")
if (nzchar(resources) && as.character(rmarkdown::pandoc_version()) != manifest$pandoc) stop("Pandoc version mismatch")
cat(jsonlite::toJSON(list(ok=TRUE, R=as.character(getRversion()), home=R.home(), libraries=.libPaths(), pandoc=as.character(rmarkdown::pandoc_version())), auto_unbox=TRUE), "\n")
