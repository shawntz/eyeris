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
