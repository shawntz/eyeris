#' Check if a parameter should be omitted from call stack display
#'
#' Determines if a parameter should be omitted when formatting call stacks
#' to avoid memory issues with large epoch-related data structures.
#'
#' @param name The parameter name
#' @param val The parameter value
#'
#' @return TRUE if the parameter should be omitted, FALSE otherwise
#'
#' @keywords internal
should_omit_parameter <- function(name, val) {
  # Only omit if:
  # 1. Parameter name suggests it's event/epoch data (case-insensitive)
  # 2. AND it's a complex object (list, which includes data.frames)
  name_lower <- tolower(name)
  is_epoch_related <- grepl("epoch", name_lower) ||
    name_lower == "events" ||
    name_lower == "baseline_events"

  is_epoch_related && is.list(val)
}

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
  # Helper function to safely deparse and truncate

  safe_deparse <- function(x, max_chars = 200) {
    if (is.character(x)) {
      # Already a string - just truncate if needed
      result <- paste(x, collapse = " ")
    } else {
      result <- tryCatch(
        paste(deparse(x, width.cutoff = 100L), collapse = " "),
        error = function(e) "<error deparsing>"
      )
    }
    # Truncate if too long
    if (nchar(result) > max_chars) {
      paste0(substr(result, 1, max_chars), "...")
    } else {
      result
    }
  }

  # Helper to format a single parameter value

  format_param_value <- function(name, val) {
    if (should_omit_parameter(name, val)) {
      return(paste0(name, " = <omitted>"))
    }
    if (is.null(val)) {
      paste0(name, " = NULL")
    } else if (is.character(val) && length(val) == 1) {
      # Check if it's already an <omitted> or similar marker
      if (grepl("^<.*>$", val) || nchar(val) < 100) {
        paste0(name, " = '", val, "'")
      } else {
        paste0(name, " = '<truncated>'")
      }
    } else if (is.logical(val) && length(val) == 1) {
      paste0(name, " = ", val)
    } else if (is.numeric(val) && length(val) <= 5) {
      paste0(name, " = ", safe_deparse(val, 50))
    } else {
      # For complex objects, just show type info
      paste0(name, " = <", class(val)[1], ">")
    }
  }

  params_parsed <- do.call(
    rbind,
    lapply(names(callstack), function(step) {
      step_data <- callstack[[step]]

      if (is.list(step_data) && "call" %in% names(step_data)) {
        call_obj <- step_data$call
        params <- step_data$parameters

        call_str <- safe_deparse(call_obj, 300)

        if (length(params) > 0) {
          param_strs <- sapply(names(params), function(name) {
            format_param_value(name, params[[name]])
          })
          param_str <- paste(param_strs, collapse = ", ")
          # Final truncation for param string
          if (nchar(param_str) > 500) {
            param_str <- paste0(substr(param_str, 1, 500), "...")
          }
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

        call_str <- safe_deparse(call_obj, 300)

        if (length(params) > 0) {
          param_strs <- sapply(names(params), function(name) {
            format_param_value(name, params[[name]])
          })
          param_str <- paste(param_strs, collapse = ", ")
          # Final truncation for param string
          if (nchar(param_str) > 500) {
            param_str <- paste0(substr(param_str, 1, 500), "...")
          }
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
        args <- safe_deparse(parsed$Arguments, 300)

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
#' Extracts numeric block numbers from block names or an `eyeris` object.
#'
#' @param x Either a character vector of block names or an `eyeris` object
#'
#' @return A numeric vector of block numbers, defaults to 1 if no blocks found
#'
#' @keywords internal
get_block_numbers <- function(x) {
  if (is.character(x)) {
    # handle both "block_N" format and fallback for other formats
    if (grepl("^block_", x)) {
      block_nums <- as.numeric(gsub("block_", "", x))
    } else {
      # for non-block_ names, try to extract any number or default to 01
      numbers <- regmatches(x, gregexpr("[0-9]+", x))[[1]]
      if (length(numbers) > 0) {
        block_nums <- as.numeric(numbers[1])
      } else {
        block_nums <- 1 # default fallback
      }
    }
  } else if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
    block_nums <- as.numeric(gsub("block_", "", names(x$timeseries)))
  } else {
    return(sprintf("%02d", 1)) # default fallback instead of NULL
  }

  # ensure we always return a valid number (block_nums may be a vector when
  # a multi-block object is passed, so guard element-wise rather than with a
  # length-1 `if`, which errors on R >= 4.2)
  if (all(is.na(block_nums))) {
    return(1) # default fallback instead of NULL
  }
  block_nums[is.na(block_nums)] <- 1

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
#' Extracts names of epoch-related elements from an `eyeris` object.
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

  return(list(version = version_str, model = model))
}
