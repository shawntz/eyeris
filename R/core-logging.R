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

  # collapse multiple strings with spaces
  message_parts <- list(...)
  message_text <- paste(message_parts, collapse = " ")

  # apply glue interpolation if there are braces, but handle errors gracefully
  glue_failed <- FALSE
  has_braces <- grepl("\\{", message_text)

  if (has_braces) {
    tryCatch(
      {
        message_text <- glue::glue(message_text, .envir = .envir)
      },
      error = function(e) {
        # if glue fails, completely remove all braces to prevent CLI parsing issues
        # this handles cases where {} contains JSON, structured data, etc.
        # also handles empty braces {} that might be followed by JSON content
        message_text <<- gsub("\\{[^}]*\\}", "[CONTENT]", message_text)
        message_text <<- gsub("\\{\\}", "[EMPTY]", message_text)
        # remove any remaining single braces that might cause issues
        message_text <<- gsub("\\{", "[", message_text)
        message_text <<- gsub("\\}", "]", message_text)
        glue_failed <<- TRUE
      }
    )
  }

  full_message <- paste(
    get_log_timestamp(),
    paste0("[", level, "]"),
    message_text
  )

  # if glue failed, use plain text output to avoid CLI expression parsing
  if (glue_failed) {
    switch(
      level,
      "INFO" = message(paste0("[INFO] ", full_message)),
      "OKAY" = message(paste0("[OKAY] ", full_message)),
      "WARN" = message(paste0("[WARN] ", full_message)),
      "EXIT" = stop(paste0("[EXIT] ", full_message), call. = FALSE),
      message(paste0("[INFO] ", full_message)) # fallback
    )
  } else {
    switch(
      level,
      "INFO" = cli::cli_alert_info(full_message, wrap = wrap),
      "OKAY" = cli::cli_alert_success(full_message, wrap = wrap),
      "WARN" = cli::cli_alert_warning(full_message, wrap = wrap),
      "EXIT" = cli::cli_abort(full_message, wrap = wrap),
      cli::cli_alert_info(full_message, wrap = wrap) # fallback
    )
  }
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
#' @keywords internal
log_info <- function(
  ...,
  verbose = TRUE,
  wrap = TRUE,
  .envir = parent.frame()
) {
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
#' @keywords internal
log_success <- function(
  ...,
  verbose = TRUE,
  wrap = TRUE,
  .envir = parent.frame()
) {
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
#' @keywords internal
log_warn <- function(
  ...,
  verbose = TRUE,
  wrap = TRUE,
  .envir = parent.frame()
) {
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
#' @keywords internal
log_error <- function(..., wrap = TRUE, .envir = parent.frame()) {
  log_message("EXIT", ..., verbose = TRUE, wrap = wrap, .envir = .envir)
}
