#' Display formatted alert messages
#'
#' A utility function to display formatted alert messages using the cli package.
#' Supports warning, info, and success message types.
#'
#' @param type The type of alert to display. Must be one of "warning", "info",
#' or "success"
#' @param msg The message to display. Can include format specifiers for
#' additional arguments
#' @param ... Additional arguments to be formatted into the message string
#'
#' @return No return value; displays the formatted message to the console
#'
#' @keywords internal
alert <- function(type = c("warning", "info", "success"), msg, ...) {
  type <- match.arg(type)

  if (type == "warning") {
    cli::cli_alert_warning(sprintf(msg, ...))
  } else if (type == "info") {
    cli::cli_alert_info(sprintf(msg, ...))
  } else if (type == "success") {
    cli::cli_alert_success(sprintf(msg, ...))
  }
}
