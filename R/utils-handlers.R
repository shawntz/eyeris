error_handler <- function(e, e_class) {
  if (inherits(e, e_class)) {
    cli::cli_alert_danger(e$message)
  } else {
    stop(e)
  }
}

# generic handler/wrapper for eyeris pupil pipeline funcs
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

  return(eyeris)
}
