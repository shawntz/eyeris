args <- commandArgs(trailingOnly = TRUE)
# Some diagnostic helpers consult a graphics device even without a preview.
# Keep those offscreen and avoid leaving Rplots.pdf in the working directory.
options(device = function(...) grDevices::pdf(file = NULL))
destination <- if (length(args)) args[[1]] else file.path(getwd(), "sub-demo_task-memory.rds")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
script <- gsub("~+~", " ", script, fixed=TRUE)
source(file.path(dirname(script), "load-package.R"))
if (!nzchar(Sys.getenv("EYERIS_PACKAGE_LIBRARY")) && !nzchar(Sys.getenv("EYERIS_SOURCE"))) {
  repo <- normalizePath(file.path(dirname(script), "../.."), mustWork = FALSE)
  if (file.exists(file.path(repo, "DESCRIPTION"))) Sys.setenv(EYERIS_SOURCE = repo)
}
load_eyeris()
x <- eyeris::glassbox(eyeris::eyelink_asc_demo_dataset(), verbose = FALSE)
x <- eyeris::epoch(x, events = "PROBE_START_{trial}", limits = c(-1, 2), label = "probe", verbose = FALSE)
saveRDS(x, destination)
cat("Saved", destination, "\n")
