#' Plot pre-processed pupil data from `eyeris`
#'
#' S3 plotting method for objects of class `eyeris`. Plots a single-panel
#' timeseries for a subset of the pupil timeseries at each preprocessing step.
#' The intended use of this function is to provide a simple method for
#' qualitatively assessing the consequences of the preprocessing recipe and
#' parameters on the raw pupillary signal.
#'
#' @param x An object of class `eyeris` derived from [eyeris::load()].
#' @param ... Additional arguments to be passed to `plot`.
#' @param steps Which steps to plot; defaults to `all` (i.e., plot all steps).
#' Otherwise, pass in a vector containing the index of the step(s) you want to
#' plot, with index `1` being the original raw pupil timeseries.
#' @param num_previews Number of random example "epochs" to generate for
#' previewing the effect of each preprocessing step on the pupil timeseries.
#' @param preview_duration Time in seconds of each randomly selected preview.
#' @param preview_window The start and stop raw timestamps used to subset the
#' preprocessed data from each step of the `eyeris` workflow for visualization.
#' Defaults to NULL, meaning random epochs as defined by `num_examples` and
#' `example_duration` will be plotted. To override the random epochs, set
#' `example_timelim` here to a vector with relative start and stop times
#' (e.g., `c(5000, 6000)` to indicate the raw data from 5-6 seconds on data that
#' were recorded at 1000 Hz). Note, the start/stop time values indicated here
#' relate to the raw index position of each pupil sample from 1 to n (which
#' will need to be specified manually by the user depending on the sampling rate
#' of the recording; i.e., 5000-6000 for the epoch positioned from 5-6 seconds
#' after the start of the timeseries, sampled at 1000 Hz).
#' @param seed Random seed for current plotting session. Leave NULL to select
#' `num_previews` number of random preview "epochs" (of `preview_duration`) each
#' time. Otherwise, choose any seed-integer as you would normally select for
#' [base::set.seed()], and you will be able to continue re-plotting the same
#' random example pupil epochs each time -- which is helpful when adjusting
#' parameters within and across `eyeris` workflow steps.
#' @param block For multi-block recordings, specifies which block to plot.
#' Defaults to 1. When a single `.asc` data file contains multiple
#' recording blocks, this parameter determines which block's timeseries to
#' visualize. Must be a positive integer not exceeding the total number of
#' blocks in the recording.
#' @param plot_distributions Logical flag to indicate whether to plot both
#' diagnostic pupil timeseries *and* accompanying histograms of the pupil
#' samples at each processing step. Defaults to `TRUE`.
#'
#' @return No return value; iteratively plots a subset of the pupil timeseries
#' from each preprocessing step run.
#'
#' @examples
#' # first, generate the preprocessed pupil data
#' my_eyeris_data <- system.file("extdata", "memory.asc", package = "eyeris") |>
#'   eyeris::load_asc() |>
#'   eyeris::deblink(extend = 50) |>
#'   eyeris::detransient() |>
#'   eyeris::interpolate() |>
#'   eyeris::lpfilt(plot_freqz = TRUE) |>
#'   eyeris::zscore()
#'
#' # controlling the timeseries range (i.e., preview window) in your plots:
#'
#' ## example 1: using the default 10000 to 20000 ms time subset
#' plot(my_eyeris_data, seed = 0)
#'
#' ## example 2: using a custom time subset (i.e., 1 to 500 ms)
#' plot(my_eyeris_data, preview_window = c(1, 500), seed = 0)
#'
#' # controlling which block of data you would like to plot:
#'
#' ## example 1: plots first block (default)
#' plot(my_eyeris_data, seed = 0)
#'
#' ## example 2: plots a specific block
#' plot(my_eyeris_data, block = 1, seed = 0)
#'
#' ## example 3: plots a specific block along with a custom preview window
#' plot(
#'   my_eyeris_data,
#'   block = 1,
#'   preview_window = c(1000, 2000),
#'   seed = 0
#' )
#'
#' @rdname plot.eyeris
#'
#' @export
plot.eyeris <- function(x, ..., steps = NULL, num_previews = NULL,
                        preview_duration = NULL, preview_window = NULL,
                        seed = NULL, block = 1, plot_distributions = TRUE) {
  # safely handle user's current options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # tests
  tryCatch(
    {
      check_data(x, "plot")
    },
    error = function(e) {
      error_handler(e, "input_data_type_error")
    }
  )

  tryCatch(
    {
      check_pupil_cols(x, "plot")
    },
    error = function(e) {
      error_handler(e, "missing_pupil_raw_error")
    }
  )

  params <- list(...)
  only_liner_trend <- if ("only_linear_trend" %in% names(params)) {
    params$only_linear_trend <- params$only_linear_trend
  } else {
    params$only_linear_trend <- FALSE
  }

  # set param defaults outside of function declaration
  if (!is.null(preview_window)) {
    if (!is.null(num_previews) || !is.null(preview_duration)) {
      cli::cli_alert_warning(
        paste(
          "num_previews and/or preview_duration will be ignored,",
          "since preview_window was specified here."
        )
      )
    }
  }

  if (is.null(steps)) {
    steps <- "all"
  }

  if (is.null(num_previews)) {
    num_previews <- 3
  }

  if (is.null(preview_duration)) {
    preview_duration <- 5
  }

  # handle random seed for this plotting session
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  # blocks handler
  if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
    available_blocks <- get_block_numbers(x)

    if (block %in% available_blocks) {
      pupil_data <- x$timeseries[[paste0("block_", block)]]
      cli::cli_alert_warning(sprintf(
        "Plotting block %d from possible blocks: %s",
        block,
        toString(available_blocks)
      ))
    } else {
      cli::cli_abort(sprintf(
        "Block %d does not exist. Available blocks: %d",
        block, toString(available_blocks)
      ))
    }
  } else {
    pupil_data <- x$timeseries
  }

  pupil_steps <- grep("^pupil_", names(pupil_data), value = TRUE)
  colors <- c("black", rainbow(length(pupil_steps) - 1))
  transparent_colors <- sapply(colors, function(x) {
    grDevices::adjustcolor(x, alpha.f = 0.5)
  })

  if (length(steps) == 1) {
    if (steps[1] == "all") {
      pupil_steps <- pupil_steps
      colors <- colors
    } else {
      pupil_steps <- pupil_steps[steps]
      colors <- colors[steps]
    }
  } else if (length(steps) > 1 && !is.null(preview_window)) {
    pupil_steps <- pupil_steps[steps]
    colors <- colors[steps]
  } else {
    pupil_steps <- pupil_steps
    colors <- colors
  }

  if (is.null(preview_window)) {
    hz <- x$info$sample.rate

    withr::with_seed(seed, {
      random_epochs <- draw_random_epochs(
        pupil_data, num_previews,
        preview_duration, hz
      )
    })

    par(mfrow = c(1, num_previews), oma = c(0, 0, 3, 0))
    detrend_plotted <- FALSE
    for (i in seq_along(pupil_steps)) {
      for (n in 1:num_previews) {
        st <- min(random_epochs[[n]]$time_orig)
        et <- max(random_epochs[[n]]$time_orig)
        title <- paste0("\n[", st, " - ", et, "]")
        header <- paste0(
          gsub("_", " > ", gsub("pupil_", "", pupil_steps[i])),
          if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
            sprintf(" (Run %d)", block)
          } else {
            ""
          }
        )

        if (grepl("z", pupil_steps[i])) {
          y_units <- "(z)"
        } else {
          y_units <- "(a.u.)"
        }

        if (n == 1) {
          y_label <- paste("pupil size", y_units)
        } else {
          y_label <- ""
        }

        # used when running `plot()` by itself (and thus plotting all steps)
        if (!only_liner_trend) {
          if (grepl("_detrend$", pupil_steps[i]) && !detrend_plotted) {
            par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
            robust_plot(pupil_data$time_orig, pupil_data[[pupil_steps[i - 1]]],
              type = "l", col = "black", lwd = 2,
              main = paste0(
                "detrend:\n",
                gsub(
                  "_", " > ",
                  gsub("pupil_", "", pupil_steps[i - 1])
                )
              ),
              xlab = "raw tracker time (ms)", ylab = "pupil size (a.u.)"
            )
            lines(pupil_data$time_orig, pupil_data$detrend_fitted_values,
              type = "l", col = "blue", lwd = 2, lty = 2
            )
            legend("topleft",
              legend = c("pupil timeseries", "linear trend"),
              col = c("black", "blue"), lwd = 2, lty = c(1, 2)
            )
            par(mfrow = c(1, num_previews), oma = c(0, 0, 3, 0))
            detrend_plotted <- TRUE
          }
        } else {
          if (!detrend_plotted) {
            par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
            title <- paste0(
              "detrend:\n",
              params$next_step[length(params$next_step) - 1]
            )
            robust_plot(pupil_data$time_orig,
              pupil_data[[params$next_step[length(params$next_step) - 1]]],
              type = "l", col = "black", lwd = 2, main = title,
              xlab = "raw tracker time (ms)", ylab = "pupil size (a.u.)"
            )
            lines(pupil_data$time_orig,
              pupil_data$detrend_fitted_values,
              type = "l", col = "blue", lwd = 2, lty = 2
            )
            legend("topleft",
              legend = c("pupil timeseries", "linear trend"),
              col = c("black", "blue"), lwd = 2, lty = c(1, 2)
            )
            par(mfrow = c(1, num_previews), oma = c(0, 0, 3, 0))
            detrend_plotted <- TRUE
            prompt_user()
          }
        }

        # nolint start
        # nolint end

        if (!is.null(params$next_step)) {
          plot_data <- random_epochs[[n]][[
            params$next_step[length(params$next_step)]
          ]]
          robust_plot(
            plot_data,
            type = "l", col = colors[i], lwd = 2,
            main = title, xlab = "time (ms)", ylab = y_label
          )
        } else {
          plot_data <- random_epochs[[n]][[pupil_steps[i]]]
          robust_plot(
            plot_data,
            type = "l", col = colors[i], lwd = 2,
            main = title, xlab = "time (ms)", ylab = y_label
          )
        }
      }

      graphics::mtext(header,
        outer = TRUE, cex = 1.25, font = 2
      )

      if (plot_distributions) {
        plot_pupil_distribution(
          data = pupil_data[[pupil_steps[i]]],
          color = colors[i],
          main = header,
          xlab = y_label
        )

        par(mfrow = c(1, num_previews), oma = c(0, 0, 3, 0))
      }
    }
    par(mfrow = c(1, num_previews), oma = c(0, 0, 3, 0))
  } else {
    start_index <- preview_window[1]
    end_index <- min(preview_window[2], nrow(pupil_data))
    sliced_pupil_data <- pupil_data[start_index:end_index, ]

    for (i in seq_along(pupil_steps)) {
      st <- min(sliced_pupil_data$time_orig)
      et <- max(sliced_pupil_data$time_orig)

      if (grepl("z", pupil_steps[i])) {
        y_units <- "(z)"
      } else {
        y_units <- "(a.u.)"
      }

      y_label <- paste("pupil size", y_units)

      robust_plot(sliced_pupil_data[[pupil_steps[i]]],
        type = "l", col = colors[i], lwd = 2,
        main = paste(paste0(
          gsub("_", " > ", gsub("pupil_", "", pupil_steps[i])),
          if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
            sprintf(" (Run %d)", block)
          } else {
            ""
          },
          "\n[", st, " - ", et, "] | ",
          "[", preview_window[1], " - ", preview_window[2], "]"
        )),
        xlab = "time (s)", ylab = y_label
      )

      if (plot_distributions) {
        plot_pupil_distribution(
          data = pupil_data[[pupil_steps[i]]],
          color = colors[i],
          main = paste(paste0(
            gsub("_", " > ", gsub("pupil_", "", pupil_steps[i])),
            if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
              sprintf(" (Run %d)", block)
            } else {
              ""
            }
          )),
          xlab = y_label
        )
        par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
      }
    }

    par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
  }

  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
}

draw_random_epochs <- function(x, n, d, hz) {
  n_samples <- d * hz
  min_timestamp <- min(x$time_orig)
  max_timestamp <- max(x$time_orig)

  if ((max_timestamp - min_timestamp) < d) {
    cli::cli_abort("Example duration is longer than the duration of data.")
  }

  drawn_epochs <- list()

  for (i in 1:n) {
    rand_start <- sample(min_timestamp:(max_timestamp - n_samples), 1)
    rand_end <- rand_start + n_samples
    drawn_epochs[[i]] <- x |>
      dplyr::filter(time_orig >= rand_start & time_orig < rand_end)
  }

  drawn_epochs
}

robust_plot <- function(x, ...) {
  tryCatch(
    {
      valid <- is.finite(x) # filter out non-finite values

      if (all(!valid)) {
        cli::cli_alert_warning(
          paste0(
            "All values are non-finite in random segment, try running",
            "again with a different seed to avoid empty plots!"
          )
        )
        x <- rep(0, length(x)) # create empty placeholder plot
      } else {
        x <- x[valid]
      }

      plot(x, ...)
    },
    error = function(e) {
      cli::cli_alert_info(
        paste("An error occurred during plotting:", e$message)
      )
    },
    warning = function(w) {
      cli::cli_alert_warning(
        paste("A warning occurred during plotting:", w$message)
      )
    }
  )
}

plot_pupil_distribution <- function(data, color, main, xlab) {
  # safely handle user's current options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

  hist(
    data,
    main = main,
    xlab = xlab,
    ylab = "frequency (count)",
    col = color,
    border = "white",
    breaks = "FD"
  )
}
