#' Standardized logging functions for eyeris
#'
#' These functions provide a consistent logging interface with automatic
#' timestamping, glue-style string interpolation, and support for multiple
#' string arguments.
#'
#' @param ... Character strings to be logged. Will be collapsed with spaces.
#'   Supports glue-style interpolation with curly braces.
#' @param verbose Logical. Whether to actually print the log message.
#' @param wrap Logical. Whether to wrap long messages (default TRUE).
#' @param .envir Environment for glue interpolation (default: parent frame).
#'
#' @name logging
#' @keywords internal
NULL

#' Get formatted timestamp for logging
#' @return Character string with current timestamp
#' @keywords internal
get_log_timestamp <- function() {
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
}

#' Core logging function with timestamp and glue support
#' @param level Character string for log level (INFO, OKAY, WARN, EXIT)
#' @param ... Character strings to log
#' @param verbose Logical. Whether to print the message
#' @param wrap Logical. Whether to wrap long messages
#' @param .envir Environment for glue interpolation
#' @keywords internal
log_message <- function(
  level,
  ...,
  verbose = TRUE,
  wrap = TRUE,
  .envir = parent.frame()
) {
  if (!verbose) {
    return(invisible(NULL))
  }

  # Collapse multiple strings with spaces
  message_parts <- list(...)
  message_text <- paste(message_parts, collapse = " ")

  # Apply glue interpolation if there are braces
  if (grepl("\\{.*\\}", message_text)) {
    message_text <- glue::glue(message_text, .envir = .envir)
  }

  # Prepend timestamp and log level
  full_message <- paste(get_log_timestamp(), paste0("[", level, "]"), message_text)

  # Call appropriate cli function based on level
  switch(
    level,
    "INFO" = cli::cli_alert_info(full_message, wrap = wrap),
    "OKAY" = cli::cli_alert_success(full_message, wrap = wrap),
    "WARN" = cli::cli_alert_warning(full_message, wrap = wrap),
    "EXIT" = cli::cli_abort(full_message, wrap = wrap),
    cli::cli_alert_info(full_message, wrap = wrap) # fallback
  )
}

#' Log an informational message
#'
#' @param ... Character strings to log. Supports glue-style interpolation.
#' @param verbose Logical. Whether to print the message (default TRUE).
#' @param wrap Logical. Whether to wrap long messages (default TRUE).
#' @param .envir Environment for glue interpolation (default: parent frame).
#'
#' @examples
#' \dontrun{
#' log_info("Processing file:", "data.csv")
#' subject_id <- "001"
#' log_info("Processing subject {subject_id}")
#' log_info("Found {nrow(data)} rows", "in dataset")
#' }
#'
#' @export
log_info <- function(..., verbose = TRUE, wrap = TRUE, .envir = parent.frame()) {
  log_message("INFO", ..., verbose = verbose, wrap = wrap, .envir = .envir)
}

#' Log a success message
#'
#' @param ... Character strings to log. Supports glue-style interpolation.
#' @param verbose Logical. Whether to print the message (default TRUE).
#' @param wrap Logical. Whether to wrap long messages (default TRUE).
#' @param .envir Environment for glue interpolation (default: parent frame).
#'
#' @examples
#' \dontrun{
#' log_success("Processing completed successfully")
#' n_files <- 5
#' log_success("Processed {n_files} files successfully")
#' }
#'
#' @export
log_success <- function(..., verbose = TRUE, wrap = TRUE, .envir = parent.frame()) {
  log_message("OKAY", ..., verbose = verbose, wrap = wrap, .envir = .envir)
}

#' Log a warning message
#'
#' @param ... Character strings to log. Supports glue-style interpolation.
#' @param verbose Logical. Whether to print the message (default TRUE).
#' @param wrap Logical. Whether to wrap long messages (default TRUE).
#' @param .envir Environment for glue interpolation (default: parent frame).
#'
#' @examples
#' \dontrun{
#' log_warn("Missing data detected")
#' missing_count <- 10
#' log_warn("Found {missing_count} missing values")
#' }
#'
#' @export
log_warn <- function(..., verbose = TRUE, wrap = TRUE, .envir = parent.frame()) {
  log_message("WARN", ..., verbose = verbose, wrap = wrap, .envir = .envir)
}

#' Log an error message and abort
#'
#' @param ... Character strings to log. Supports glue-style interpolation.
#' @param wrap Logical. Whether to wrap long messages (default TRUE).
#' @param .envir Environment for glue interpolation (default: parent frame).
#'
#' @examples
#' \dontrun{
#' log_error("Critical error occurred")
#' file_path <- "missing.csv"
#' log_error("File not found: {file_path}")
#' }
#'
#' @export
log_error <- function(..., wrap = TRUE, .envir = parent.frame()) {
  log_message("EXIT", ..., verbose = TRUE, wrap = wrap, .envir = .envir)
}
