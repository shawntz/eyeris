args <- commandArgs(TRUE)
lock <- jsonlite::fromJSON(args[1])
lib <- args[2]
dir.create(lib, recursive=TRUE, showWarnings=FALSE)
platform <- Sys.info()[["sysname"]]
options(timeout=600)
major <- paste(strsplit(lock$R, ".", fixed=TRUE)[[1]][1:2], collapse=".")
headers <- NULL
if (platform == "Darwin") {
  suffix <- ".tgz"
  distribution <- if (R.version$arch == "aarch64") "macosx/sonoma-arm64" else "macosx/big-sur-x86_64"
  repository <- paste0(lock$repository,"/bin/",distribution,"/contrib/",major)
} else if (.Platform$OS.type == "windows") {
  suffix <- ".zip"
  distribution <- "windows"
  repository <- paste0(lock$repository,"/bin/windows/contrib/",major)
} else {
  # Request the native Ubuntu 22.04 binaries explicitly. Never silently compile
  # a source archive (especially DuckDB) if binary negotiation fails.
  suffix <- ".tar.gz"
  distribution <- "linux/jammy"
  repository <- paste0(sub("/cran/", "/cran/__linux__/jammy/", lock$repository, fixed=TRUE),"/src/contrib")
  headers <- c("User-Agent"=sprintf("R (%s %s %s %s)",getRversion(),R.version$platform,R.version$arch,R.version$os))
}
cache <- file.path(dirname(lib), "runtime-cache", basename(lock$repository), distribution, major)
dir.create(cache, recursive=TRUE, showWarnings=FALSE)
for (pkg in names(lock$packages)) {
  name <- paste0(pkg,"_",lock$packages[[pkg]],suffix)
  archive <- file.path(cache,name)
  if (!file.exists(archive)) {
    download.file(paste0(repository,"/",name),paste0(archive,".part"),mode="wb",quiet=TRUE,headers=headers)
    stopifnot(file.rename(paste0(archive,".part"),archive))
  }
  if (.Platform$OS.type == "windows") {
    unzip(archive,exdir=lib)
  } else {
    entries <- untar(archive,list=TRUE)
    if (!paste0(pkg,"/Meta/package.rds") %in% sub("^\\./", "", entries)) {
      unlink(archive)
      stop("Expected a prebuilt binary for ",pkg,"; repository returned a source archive")
    }
    untar(archive,exdir=lib)
  }
}
for (pkg in names(lock$packages)) {
  if (!file.exists(file.path(lib,pkg,"DESCRIPTION")) || packageVersion(pkg,lib.loc=lib) != package_version(lock$packages[[pkg]])) {
    stop("Pinned dependency unavailable: ",pkg," ",lock$packages[[pkg]])
  }
}
