#' Run `eyeris` commands with automatic logging of R console's stdout and stderr
#'
#' This utility function evaluates `eyeris` commands while automatically
#' capturing and recording both standard output (`stdout`) and standard error
#' (`stderr`) to timestamped log files in your desired log directory.
#'
#' Each run produces two log files:
#' - `<timestamp>.out`: records all console output
#' - `<timestamp>.err`: records all warnings and errors
#'
#' @param eyeris_cmd An `eyeris` command, wrapped in `{}` if multiline.
#' @param log_dir Character path to the desired log directory. Is set to the
#' temporary directory given by [tempdir()] by default.
#' @param timestamp_format Format string passed to `format(Sys.time())` for
#' naming the log files. Defaults to `"%Y%m%d_%H%M%S"`.
#'
#' @return The result of the evaluated `eyeris` command (invisibly).
#'
#' @examples
#' eyelogger({
#'   message("eyeris `glassbox()` completed successfully.")
#'   warning("eyeris `glassbox()` completed with warnings.")
#'   print("some eyeris-related information.")
#' })
#'
#' eyelogger({
#'   glassbox(eyelink_asc_demo_dataset(), interactive_preview = FALSE)
#' }, log_dir = file.path(tempdir(), "eyeris_logs"))
#'
#' @export
eyelogger <- function(eyeris_cmd,
                      log_dir = file.path(tempdir(), "eyeris_logs"),
                      timestamp_format = "%Y%m%d_%H%M%S") {
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), timestamp_format)

  stdout_file <- file.path(log_dir, paste0(timestamp, ".out"))
  stderr_file <- file.path(log_dir, paste0(timestamp, ".err"))

  cmd_file <- file.path(log_dir, paste0(timestamp, ".cmd"))
  writeLines(deparse(substitute(eyeris_cmd)), con = cmd_file)

  stdout_con <- file(stdout_file, open = "wt")
  stderr_con <- file(stderr_file, open = "wt")
  sink(stdout_con, type = "output")
  sink(stderr_con, type = "message")

  result <- NULL

  tryCatch({
    result <- eval(eyeris_cmd, envir = parent.frame())
  }, finally = {
    sink(type = "message")
    sink(type = "output")
    close(stdout_con)
    close(stderr_con)
  })

  invisible(result)
}
