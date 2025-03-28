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
#' winsorize <- function(eyeris, lower = 0.01, upper = 0.99) {
#'   pipeline_handler(
#'     eyeris,
#'     winsorize_pupil,
#'     "winsorize",
#'     lower = lower,
#'     upper = upper
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
  call_stack <- sys.calls()[[1]]

  if (!is.list(eyeris$params)) eyeris$params <- list()
  eyeris$params[[new_suffix]] <- call_stack

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
        } else {
          data[[output_col]] <- operation(data, prev_operation, ...)
        }
        # update block in S3 eyeris object
        eyeris$timeseries[[i_block]] <- data
      }
    }
    # update log var with latest op
    eyeris$latest <- output_col
  } else {
    # handle single dfs fallback case
    if (new_suffix == "epoch") {
      # run op
      data <- operation(eyeris, prev_operation, ...)
      # reset updated S3 eyeris class
      eyeris <- data
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
      # update log var with latest op
      eyeris$latest <- output_col
      # update with detrend coefs if detrended
      if (new_suffix == "detrend") {
        eyeris$detrend_coefs <- list_detrend$coefficients
      }
    }
  }

  eyeris
}
