parse_call_stack <- function(call_str) {
  func <- sub("\\(.*", "", call_str)
  return(list(Function = func, Arguments = call_str))
}

format_call_stack <- function(callstack) {
  params_parsed <- do.call(rbind, lapply(names(callstack), function(step) {
    parsed <- parse_call_stack(callstack[[step]])
    args <- deparse(parsed$Arguments)
    args <- paste(args, collapse = "")

    data.frame(
      step = step,
      callstack = args,
      stringsAsFactors = FALSE
    )
  }))

  rownames(params_parsed) <- NULL

  return(params_parsed)
}


filter_epochs <- function(eyeris, epochs) {
  return(names(eyeris)[grepl("^epoch_", names(eyeris))])
}
