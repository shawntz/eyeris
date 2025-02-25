progress_bar <- function(total, msg = "Processing", width = 80,
                         show_percent = TRUE, show_eta = TRUE,
                         clear = FALSE) {
  format_str <- c(msg, "[:bar]")
  if (show_percent) format_str <- c(format_str, ":percent")
  if (show_eta) format_str <- c(format_str, "eta: :eta")
  format_str <- paste(format_str, collapse = " ")

  progress::progress_bar$new(
    format = format_str,
    total = total,
    width = width,
    clear = clear
  )
}

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

tick <- function(pb, by = 1) {
  if (!is.null(pb)) pb$tick(by)
}
