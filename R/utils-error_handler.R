error_handler <- function(e, e_class) {
  if (inherits(e, e_class)) {
    cli::cli_alert_danger(e$message)
  } else {
    stop(e)
  }
}
