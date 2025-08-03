#' Handle errors with custom error classes
#'
#' A utility function to handle errors with specific error classes and provide
#' appropriate error messages using the cli package.
#'
#' @param e The error object to handle
#' @param e_class The expected error class to check against
#'
#' @return No return value; either displays an error message via cli or stops
#'   execution with the original error
#'
#' @keywords internal
error_handler <- function(e, e_class) {
  if (inherits(e, e_class)) {
    log_error(e$message)
  } else {
    stop(e)
  }
}
