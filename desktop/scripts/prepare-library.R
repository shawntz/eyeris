args <- commandArgs(TRUE)
lock <- jsonlite::fromJSON(args[1])
lib <- args[2]
dir.create(lib, recursive=TRUE, showWarnings=FALSE)
platform <- Sys.info()[["sysname"]]
options(timeout=600)
if (platform == "Darwin" || .Platform$OS.type == "windows") {
  suffix <- if (platform == "Darwin") ".tgz" else ".zip"
  distribution <- if (platform == "Darwin") {
    if (R.version$arch == "aarch64") "macosx/sonoma-arm64" else "macosx/big-sur-x86_64"
  } else "windows"
  major <- paste(strsplit(lock$R, ".", fixed=TRUE)[[1]][1:2], collapse=".")
  cache <- file.path(dirname(lib), "runtime-cache", basename(lock$repository), distribution, major)
  dir.create(cache, recursive=TRUE, showWarnings=FALSE)
  for (pkg in names(lock$packages)) {
    name <- paste0(pkg,"_",lock$packages[[pkg]],suffix)
    archive <- file.path(cache,name)
    if (!file.exists(archive)) {
      url <- paste0(lock$repository,"/bin/",distribution,"/contrib/",major,"/",name)
      download.file(url,paste0(archive,".part"),mode="wb",quiet=TRUE)
      stopifnot(file.rename(paste0(archive,".part"),archive))
    }
    if (platform == "Darwin") untar(archive,exdir=lib) else unzip(archive,exdir=lib)
  }
} else {
  # CI uses Ubuntu 22.04. Its dated binary snapshot avoids compiling DuckDB.
  repo <- sub("/cran/", "/cran/__linux__/jammy/", lock$repository, fixed=TRUE)
  options(HTTPUserAgent=sprintf("R (%s %s %s %s)",getRversion(),R.version$platform,R.version$arch,R.version$os))
  install.packages(names(lock$packages),lib=lib,repos=repo,dependencies=FALSE,Ncpus=2)
}
for (pkg in names(lock$packages)) {
  if (!file.exists(file.path(lib,pkg,"DESCRIPTION")) || packageVersion(pkg,lib.loc=lib) != package_version(lock$packages[[pkg]])) {
    stop("Pinned dependency unavailable: ",pkg," ",lock$packages[[pkg]])
  }
}
