parse_call_stack <- function(call_str) {
  func <- sub("\\(.*", "", call_str)
  list(Function = func, Arguments = call_str)
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

  params_parsed
}

get_block_numbers <- function(x) {
  if (is.character(x)) {
    block_nums <- as.numeric(gsub("block_", "", x))
  } else if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
    # extract numbers from names like "block_4"
    block_nums <- as.numeric(gsub("block_", "", names(x$timeseries)))
  } else {
    return(NULL)
  }
  block_nums
}

# keep letters, numbers and spaces
clean_string <- function(str) {
  gsub("[^[:alnum:]\\s]", "", str)
}

convert_nested_dt <- function(nested_dt) {
  lapply(nested_dt, function(outer_list) {
    lapply(outer_list, function(dt) {
      if (data.table::is.data.table(dt)) {
        dplyr::as_tibble(dt)
      } else {
        NULL
      }
    })
  })
}

filter_epochs <- function(eyeris, epochs) {
  names(eyeris)[grepl("^epoch_", names(eyeris))]
}

parse_eyelink_info <- function(version_str, model = NA) {
  # edge case: if model is NA & version string contains model info
  if (is.na(model) && grepl("EyeLink", version_str)) {
    model_match <- regexpr("\\(EyeLink[^\\(\\)]*", version_str)
    if (model_match > 0) {
      model_text <- regmatches(version_str, model_match)[[1]]
      model <- trimws(gsub("^\\(", "", model_text))
      version_str <- trimws(sub("\\(EyeLink[^\\(]*$", "", version_str))
    }
  }

  return(list(
    version = version_str,
    model = model
  ))
}
