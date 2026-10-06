# Explicit maintainer operation; normal builds consume the committed lock.
args <- commandArgs(TRUE)
if (length(args) != 1L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", args[1])) stop("Usage: Rscript scripts/update-runtime-lock.R YYYY-MM-DD (from desktop/)")
lock <- jsonlite::fromJSON("runtime-lock.json")
repo <- paste0("https://packagemanager.posit.co/cran/", args[1])
available <- available.packages(repos=repo)
roots <- trimws(unlist(strsplit(read.dcf("../DESCRIPTION")[1,"Imports"], ",")))
roots <- c(sub("[[:space:]]+[(].*$", "", roots), "duckdb")
deps <- unique(c(roots, unlist(tools::package_dependencies(roots, db=available, recursive=TRUE))))
packages <- available[intersect(deps,rownames(available)), c("Package","Version")]
lock$repository <- repo
lock$packages <- as.list(setNames(packages[,"Version"],packages[,"Package"]))
jsonlite::write_json(lock, "runtime-lock.json", pretty=TRUE, auto_unbox=TRUE)
