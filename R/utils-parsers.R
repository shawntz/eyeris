#' Parse call stack information
#'
#' Extracts function name and arguments from a call string.
#'
#' @param call_str A string representation of a function call
#'
#' @return A list containing the function name and full call string
#'
#' @keywords internal
parse_call_stack <- function(call_str) {
  func <- sub("\\(.*", "", call_str)
  list(Function = func, Arguments = call_str)
}

#' Format call stack information for display
#'
#' Converts call stack information into a formatted data frame for display.
#'
#' @param callstack A list of call stack information
#'
#' @return A data frame with formatted call stack information
#'
#' @keywords internal
format_call_stack <- function(callstack) {
  params_parsed <- do.call(
    rbind,
    lapply(names(callstack), function(step) {
      step_data <- callstack[[step]]

      if (is.list(step_data) && "call" %in% names(step_data)) {
        call_obj <- step_data$call
        params <- step_data$parameters

        call_str <- deparse(call_obj)
        call_str <- paste(call_str, collapse = "")

        if (length(params) > 0) {
          param_strs <- sapply(names(params), function(name) {
            val <- params[[name]]
            if (is.null(val)) {
              paste0(name, " = NULL")
            } else if (is.character(val)) {
              paste0(name, " = '", val, "'")
            } else if (is.logical(val)) {
              paste0(name, " = ", val)
            } else {
              paste0(name, " = ", deparse(val))
            }
          })
          param_str <- paste(param_strs, collapse = ", ")
        } else {
          param_str <- "no parameters"
        }

        data.frame(
          step = step,
          callstack = call_str,
          parameters = param_str,
          stringsAsFactors = FALSE
        )
      } else if (is.list(step_data) && "call_stack" %in% names(step_data)) {
        call_obj <- step_data$call_stack
        params <- step_data$parameters

        call_str <- deparse(call_obj)
        call_str <- paste(call_str, collapse = "")

        if (length(params) > 0) {
          param_strs <- sapply(names(params), function(name) {
            val <- params[[name]]
            if (is.null(val)) {
              paste0(name, " = NULL")
            } else if (is.character(val)) {
              paste0(name, " = '", val, "'")
            } else if (is.logical(val)) {
              paste0(name, " = ", val)
            } else {
              paste0(name, " = ", deparse(val))
            }
          })
          param_str <- paste(param_strs, collapse = ", ")
        } else {
          param_str <- "no parameters"
        }

        data.frame(
          step = step,
          callstack = call_str,
          parameters = param_str,
          stringsAsFactors = FALSE
        )
      } else {
        parsed <- parse_call_stack(step_data)
        args <- deparse(parsed$Arguments)
        args <- paste(args, collapse = "")

        data.frame(
          step = step,
          callstack = args,
          parameters = "not available",
          stringsAsFactors = FALSE
        )
      }
    })
  )

  rownames(params_parsed) <- NULL
  params_parsed
}

#' Extract block numbers from eyeris object or character vector
#'
#' Extracts numeric block numbers from block names or an eyeris object.
#'
#' @param x Either a character vector of block names or an `eyeris` object
#'
#' @return A numeric vector of block numbers, or NULL if no blocks found
#'
#' @keywords internal
get_block_numbers <- function(x) {
  if (is.character(x)) {
    block_nums <- as.numeric(gsub("block_", "", x))
  } else if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
    block_nums <- as.numeric(gsub("block_", "", names(x$timeseries)))
  } else {
    return(NULL)
  }
  block_nums
}

#' Clean string by removing non-alphanumeric characters
#'
#' Removes all non-alphanumeric and non-whitespace characters from a string.
#'
#' @param str The string to clean
#'
#' @return A cleaned string with only alphanumeric characters and spaces
#'
#' @keywords internal
clean_string <- function(str) {
  gsub("[^[:alnum:]\\s]", "", str)
}

#' Convert nested data.table objects to tibbles
#'
#' Recursively converts data.table objects within nested lists to tibbles.
#'
#' @param nested_dt A nested list containing data.table objects
#'
#' @return A nested list with data.table objects converted to tibbles
#'
#' @keywords internal
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

#' Filter epoch names from eyeris object
#'
#' Extracts names of epoch-related elements from an eyeris object.
#'
#' @param eyeris An `eyeris` object
#' @param epochs A vector of epoch names to filter
#'
#' @return A character vector of epoch names that start with "epoch_"
#'
#' @keywords internal
filter_epochs <- function(eyeris, epochs) {
  names(eyeris)[grepl("^epoch_", names(eyeris))]
}

#' Parse EyeLink version and model information
#'
#' Extracts and cleans version and model information from EyeLink metadata.
#'
#' @param version_str The version string from EyeLink metadata
#' @param model The model string from EyeLink metadata (default: NA)
#'
#' @return A list containing cleaned version and model strings
#'
#' @keywords internal
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
