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
#' preprocessing history for that specific instance of the `eyeris` object.
#' @param operation The name of the function to apply to the timeseries data.
#' This custom function should accept a dataframe `x`, a string `prev_op`
#' (i.e., the name of the previous pupil column -- which you DO NOT need to
#' supply as a literal string as this is inferred from the `latest` pointer
#' within the `eyeris` object), and any custom parameters you would like.
#' @param new_suffix A chracter string indicating the suffix you would like
#' to be appended to the name of the previous operation's column, which will
#' be used for the new column name in the updated preprocessed dataframe(s).
#' @param ... Additional (optional) arguments passed to the `operation` method.
#'
#' @return An updated `eyeris` object with the new column added to the
#' `timeseries` dataframe and the `latest` pointer updated to the name of the
#' most recently added column plus all previous columns (ie, the history "trace"
#' of preprocessing steps from start-to-present).
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
#'   pipeline_handler(
#'     eyeris,
#'     winsorize_pupil,
#'     "winsorize",
#'     lower = lower,
#'     upper = upper,
#'     call_info = call_info
#'   )
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
  if (!is.list(eyeris$params)) eyeris$params <- list()
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

  # setters
  output_col <- paste0(prev_operation, "_", new_suffix)

  # handle either list of dataframes per block or a single df
  if (is.list(eyeris$timeseries) && !is.data.frame(eyeris$timeseries)) {
    # handle list of dfs (block) default method

    # time monotonicity check for each block
    for (i_block in names(eyeris$timeseries)) {
      data <- eyeris$timeseries[[i_block]]
      if ("time_secs" %in% colnames(data)) {
        check_time_monotonic(data$time_secs, "time_secs")
      }
      if ("time_orig" %in% colnames(data)) {
        check_time_monotonic(data$time_orig, "time_orig")
      }
    }

    # testing:
    if (new_suffix == "epoch") {
      # run op
      data <- operation(eyeris, prev_operation, ...)

      # reset updated S3 eyeris class
      eyeris <- data
    }

    for (i_block in names(eyeris$timeseries)) {
      if (new_suffix != "epoch") {
        data <- eyeris$timeseries[[i_block]]

        # run operation
        if (new_suffix == "detrend") {
          list_detrend <- operation(data, prev_operation, ...)
          data["detrend_fitted_values"] <- list_detrend$fitted_values
          data[[output_col]] <- list_detrend$residuals
          # store detrend coefficients per block
          if (!exists("detrend_coefs", eyeris)) {
            eyeris$detrend_coefs <- list()
          }
          eyeris$detrend_coefs[[i_block]] <- list_detrend$coefficients
        } else if (new_suffix == "bin" || new_suffix == "downsample") {
          list_ds_bin <- operation(data, prev_operation, ...)
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
              dplyr::ends_with("_bin"), .after = last_col()
            )
        } else {
          data[[output_col]] <- operation(data, prev_operation, ...)
        }
        # update block in S3 eyeris object
        eyeris$timeseries[[i_block]] <- data
        if (new_suffix == "bin" || new_suffix == "downsample") {
          eyeris$decimated.sample.rate <- list_ds_bin$decimated.sample.rate
          # update latest pointer for bin/downsample operations
          eyeris$latest <- output_col
        }
      }
    }

    # update latest pointer for non-bin/downsample operations
    if (
      new_suffix != "bin" &&
        new_suffix != "downsample" &&
        new_suffix != "epoch"
    ) {
      eyeris$latest <- output_col
    }
  } else {
    # handle single dfs fallback case
    # global time monotonicity check for single dataframe
    data <- eyeris$timeseries
    if ("time_secs" %in% colnames(data)) {
      check_time_monotonic(data$time_secs, "time_secs")
    }
    if ("time_orig" %in% colnames(data)) {
      check_time_monotonic(data$time_orig, "time_orig")
    }
    if (new_suffix == "epoch") {
      # run op
      data <- operation(eyeris, prev_operation, ...)
      # reset updated S3 eyeris class
      eyeris <- data
    } else if (new_suffix == "bin" || new_suffix == "downsample") {
      data <- eyeris$timeseries
      # run op
      result <- operation(data, prev_operation, ...)
      # update S3 eyeris class
      eyeris$timeseries <- result
    } else {
      data <- eyeris$timeseries
      # run operation
      if (new_suffix == "detrend") {
        list_detrend <- operation(data, prev_operation, ...)
        data["detrend_fitted_values"] <- list_detrend$fitted_values
        data[[output_col]] <- list_detrend$residuals
      } else {
        data[[output_col]] <- operation(data, prev_operation, ...)
      }
      # update S3 eyeris class
      eyeris$timeseries <- data
      # update with detrend coefs if detrended
      if (new_suffix == "detrend") {
        eyeris$detrend_coefs <- list_detrend$coefficients
      }
    }
    # update log var with latest op
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
