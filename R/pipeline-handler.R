#' Build a generic operation (extension) for the `eyeris` pipeline
#'
#' `pipeline_handler` enables flexible integration of custom data
#' processing functions into the `eyeris` pipeline. Under the hood,
#' each preprocessing function in `eyeris` is a wrapper around a
#' core operation that gets tracked, versioned, and stored using this
#' `pipeline_handler` method. As such, custom pipeline steps must conform
#' to the `eyeris` protocol for maximum compatibility with the downstream
#' functions we provide.
#'
#' Following the `eyeris` protocol also ensures:
#' \itemize{
#'     \item all operations follow a predictable structure, and
#'     \item that new pupil data columns based on previous operations in the
#'       chain are able to be dynamically constructed within the core
#'       timeseries data frame.
#' }
#' @param eyeris An object of class `eyeris` containing timeseries data
#' in a list of dataframes (one per block), various metadata collected
#' by the tracker, and `eyeris` specific pointers for tracking the
#' preprocessing history for that specific instance of the `eyeris` object
#' @param operation The name of the function to apply to the timeseries data.
#' This custom function should accept a dataframe `x`, a string `prev_op`
#' (i.e., the name of the previous pupil column -- which you DO NOT need to
#' supply as a literal string as this is inferred from the `latest` pointer
#' within the `eyeris` object), and any custom parameters you would like
#' @param new_suffix A character string indicating the suffix you would like
#' to be appended to the name of the previous operation's column, which will
#' be used for the new column name in the updated preprocessed dataframe(s)
#' @param ... Additional (optional) arguments passed to the `operation` method
#'
#' @return An updated `eyeris` object with the new column added to the
#' `timeseries` dataframe and the `latest` pointer updated to the name of the
#' most recently added column plus all previous columns (ie, the history "trace"
#' of preprocessing steps from start-to-present)
#'
#' @examples
#' # first, define your custom data preprocessing function
#' winsorize_pupil <- function(x, prev_op, lower = 0.01, upper = 0.99) {
#'   vec <- x[[prev_op]]
#'   q <- quantile(vec, probs = c(lower, upper), na.rm = TRUE)
#'   vec[vec < q[1]] <- q[1]
#'   vec[vec > q[2]] <- q[2]
#'   vec
#' }
#'
#' # second, construct your `pipeline_handler` method wrapper
#' winsorize <- function(eyeris, lower = 0.01, upper = 0.99, call_info = NULL) {
#'   # create call_info if not provided
#'   call_info <- if (is.null(call_info)) {
#'     list(
#'       call_stack = match.call(),
#'       parameters = list(lower = lower, upper = upper)
#'     )
#'   } else {
#'     call_info
#'   }
#'
#'   # handle binocular objects
#'   if (eyeris:::is_binocular_object(eyeris)) {
#'     # process left and right eyes independently
#'     left_result <- eyeris$left |>
#'       pipeline_handler(
#'         winsorize_pupil,
#'         "winsorize",
#'         lower = lower,
#'         upper = upper,
#'         call_info = call_info
#'       )
#'
#'     right_result <- eyeris$right |>
#'       pipeline_handler(
#'         winsorize_pupil,
#'         "winsorize",
#'         lower = lower,
#'         upper = upper,
#'         call_info = call_info
#'     )
#'
#'     # return combined structure
#'     list_out <- list(
#'       left = left_result,
#'       right = right_result,
#'       original_file = eyeris$original_file,
#'       raw_binocular_object = eyeris$raw_binocular_object
#'     )
#'
#'     class(list_out) <- "eyeris"
#'
#'     return(list_out)
#'   } else {
#'     # regular eyeris object, process normally
#'     eyeris |>
#'       pipeline_handler(
#'         winsorize_pupil,
#'         "winsorize",
#'         lower = lower,
#'         upper = upper,
#'         call_info = call_info
#'       )
#'   }
#' }
#'
#' # and voilà, you can now connect your custom extension
#' # directly into your custom `eyeris` pipeline definition!
#' custom_eye <- system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc(block = "auto") |>
#'   eyeris::deblink(extend = 50) |>
#'   winsorize()
#'
#' plot(custom_eye, seed = 1)
#'
#' @seealso
#' For more details, please check out the following vignettes:
#' - Anatomy of an eyeris Object
#'
#' \code{vignette("anatomy", package = "eyeris")}
#'
#' - Building Your Own Custom Pipeline Extensions
#'
#' \code{vignette("custom-extensions", package = "eyeris")}
#'
#' @export
pipeline_handler <- function(eyeris, operation, new_suffix, ...) {
  # extract call_info from ... if it was passed that way
  dots <- list(...)
  if ("call_info" %in% names(dots)) {
    call_info <- dots$call_info
    dots$call_info <- NULL # Remove call_info from dots
  }
  if (!is.list(eyeris$params)) {
    eyeris$params <- list()
  }
  # ensure call_info is a list with call_stack and parameters
  if (!is.null(call_info) && !is.list(call_info)) {
    call_info <- list(call_stack = call_info, parameters = dots)
  }
  if (is.null(call_info)) {
    call_info <- list(call_stack = sys.calls(), parameters = dots)
  }
  eyeris$params[[new_suffix]] <- call_info

  tryCatch(
    {
      check_data(eyeris, new_suffix)
    },
    error = function(e) {
      error_handler(e, "input_data_type_error")
    }
  )

  # getters
  prev_operation <- eyeris$latest

  # handle per-block pointers for multiblock data
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    is_multiblock <- TRUE
  } else {
    if (
      is.null(prev_operation) ||
        length(prev_operation) == 0 ||
        prev_operation == ""
    ) {
      cli::cli_abort(
        paste0(
          "[EXIT] Latest pointer is empty or NULL.",
          "This indicates a pipeline initialization error."
        )
      )
    }
    if (grepl("_([^_]+)_\\1", prev_operation)) {
      cli::cli_abort(paste(
        "[EXIT] Corrupted latest pointer detected:",
        prev_operation,
        "This indicates a pipeline error. Please restart the pipeline."
      ))
    }
    is_multiblock <- FALSE
  }

  # only create output_col and check for single-block data
  if (!is_multiblock) {
    output_col <- paste0(prev_operation, "_", new_suffix)
    if (grepl("_([^_]+)_\\1", output_col)) {
      cli::cli_abort(paste(
        "[EXIT] Attempting to create corrupted column name:",
        output_col,
        "This indicates a pipeline processing error. Please check your data."
      ))
    }
  }

  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    for (i_block in names(eyeris$timeseries)) {
      data <- eyeris$timeseries[[i_block]]
      if ("time_secs" %in% colnames(data)) {
        check_time_monotonic(data$time_secs, "time_secs")
      }
      if ("time_orig" %in% colnames(data)) {
        check_time_monotonic(data$time_orig, "time_orig")
      }
    }
    if (new_suffix == "epoch") {
      data <- do.call(operation, c(list(eyeris, prev_operation), dots))
      eyeris <- data
    }
    for (i_block in names(eyeris$timeseries)) {
      if (new_suffix != "epoch") {
        data <- eyeris$timeseries[[i_block]]
        block_prev_operation <- eyeris$latest[[i_block]]
        if (
          is.null(block_prev_operation) ||
            length(block_prev_operation) == 0 ||
            block_prev_operation == ""
        ) {
          cli::cli_abort(paste(
            "[EXIT] Latest pointer for block",
            i_block,
            "is empty or NULL."
          ))
        }
        if (grepl("_([^_]+)_\\1", block_prev_operation)) {
          cli::cli_abort(paste(
            "[EXIT] Corrupted latest pointer detected for block",
            i_block,
            ":",
            block_prev_operation,
            "This indicates a pipeline error. Please restart the pipeline."
          ))
        }
        block_output_col <- paste0(block_prev_operation, "_", new_suffix)
        if (grepl("_([^_]+)_\\1", block_output_col)) {
          cli::cli_abort(paste(
            "[EXIT] Attempting to create corrupted column name for block",
            i_block,
            ":",
            block_output_col,
            "This indicates a pipeline error. Please check your data."
          ))
        }
        if (new_suffix == "detrend") {
          list_detrend <- do.call(
            operation,
            c(list(data, block_prev_operation), dots)
          )
          data["detrend_fitted_values"] <- list_detrend$fitted_values
          data[[block_output_col]] <- list_detrend$residuals
          if (!exists("detrend_coefs", eyeris)) {
            eyeris$detrend_coefs <- list()
          }
          eyeris$detrend_coefs[[i_block]] <- list_detrend$coefficients
        } else if (new_suffix == "bin" || new_suffix == "downsample") {
          list_ds_bin <- do.call(
            operation,
            c(list(data, block_prev_operation), dots)
          )
          data <- list_ds_bin$downsampled_df |>
            dplyr::select(
              block,
              time_orig,
              time_secs,
              dplyr::everything(),
              -dplyr::starts_with("pupil_"),
              dplyr::starts_with("pupil_")
            ) |>
            dplyr::relocate(
              dplyr::ends_with("_bin"),
              .after = last_col()
            )
        } else {
          data[[block_output_col]] <- do.call(
            operation,
            c(list(data, block_prev_operation), dots)
          )
        }
        eyeris$timeseries[[i_block]] <- data
        if (new_suffix == "bin" || new_suffix == "downsample") {
          eyeris$decimated.sample.rate <- list_ds_bin$decimated.sample.rate
          eyeris$latest[[i_block]] <- block_output_col
        }
      }
    }
    if (
      new_suffix != "bin" &&
        new_suffix != "downsample" &&
        new_suffix != "epoch"
    ) {
      for (i_block in names(eyeris$timeseries)) {
        block_prev_operation <- eyeris$latest[[i_block]]
        block_output_col <- paste0(block_prev_operation, "_", new_suffix)
        eyeris$latest[[i_block]] <- block_output_col
      }
    }
  } else {
    data <- eyeris$timeseries
    if ("time_secs" %in% colnames(data)) {
      check_time_monotonic(data$time_secs, "time_secs")
    }
    if ("time_orig" %in% colnames(data)) {
      check_time_monotonic(data$time_orig, "time_orig")
    }
    if (new_suffix == "epoch") {
      data <- do.call(operation, c(list(eyeris, prev_operation), dots))
      eyeris <- data
    } else if (new_suffix == "bin" || new_suffix == "downsample") {
      data <- eyeris$timeseries
      result <- do.call(operation, c(list(data, prev_operation), dots))
      eyeris$timeseries <- result
    } else {
      data <- eyeris$timeseries
      if (new_suffix == "detrend") {
        list_detrend <- do.call(operation, c(list(data, prev_operation), dots))
        data["detrend_fitted_values"] <- list_detrend$fitted_values
        data[[output_col]] <- list_detrend$residuals
      } else {
        data[[output_col]] <- do.call(
          operation,
          c(list(data, prev_operation), dots)
        )
      }
      eyeris$timeseries <- data
      if (new_suffix == "detrend") {
        eyeris$detrend_coefs <- list_detrend$coefficients
      }
    }
    eyeris$latest <- output_col
  }

  # guard: if no downsampling or binning, time_scaled should mirror time_secs
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    for (i_block in names(eyeris$timeseries)) {
      data <- eyeris$timeseries[[i_block]]
      if (!"time_scaled" %in% colnames(data)) {
        data$time_scaled <- data$time_secs
        eyeris$timeseries[[i_block]] <- data
      }
    }
  } else {
    if (!"time_scaled" %in% colnames(eyeris$timeseries)) {
      eyeris$timeseries$time_scaled <- eyeris$timeseries$time_secs
    }
  }
  eyeris
}
