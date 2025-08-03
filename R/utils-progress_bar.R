#' Create a progress bar for tracking operations
#'
#' Creates a progress bar using the progress package with customizable
#' formatting.
#'
#' @param total The total number of items to process
#' @param msg The message to display before the progress bar
#' @param width The width of the progress bar in characters
#' @param show_percent Whether to show percentage completion
#' @param show_eta Whether to show estimated time remaining
#' @param clear Whether to clear the progress bar when done
#'
#' @return A progress bar object from the progress package
#'
#' @keywords internal
progress_bar <- function(
  total,
  msg = "Processing",
  width = 80,
  show_percent = TRUE,
  show_eta = TRUE,
  clear = FALSE
) {
  format_str <- c(msg, "[:bar]")
  if (show_percent) {
    format_str <- c(format_str, ":percent")
  }
  if (show_eta) {
    format_str <- c(format_str, "eta: :eta")
  }
  format_str <- paste(format_str, collapse = " ")

  progress::progress_bar$new(
    format = format_str,
    total = total,
    width = width,
    clear = clear
  )
}

#' Create a counter progress bar
#'
#' Creates a simple counter progress bar that shows current/total progress.
#'
#' @param total The total number of items to process
#' @param msg The message to display before the counter
#' @param width The width of the progress bar in characters
#'
#' @return A progress bar object from the progress package
#'
#' @keywords internal
counter_bar <- function(total, msg = "Progress", width = 80) {
  progress_bar(
    total = total,
    msg = sprintf("%s (:current/:total)", msg),
    width = width,
    show_percent = FALSE,
    show_eta = FALSE,
    clear = FALSE
  )
}

#' Tick a progress bar
#'
#' Advances a progress bar by the specified amount.
#'
#' @param pb The progress bar object to tick
#' @param by The number of steps to advance (default: `1`)
#'
#' @return No return value; advances the progress bar
#'
#' @keywords internal
tick <- function(pb, by = 1) {
  if (!is.null(pb)) pb$tick(by)
}
