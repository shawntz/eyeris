#' Plot pre-processed pupil data from `eyeris`
#'
#' S3 plotting method for objects of class `eyeris`. Plots a single-panel
#' timeseries for a subset of the pupil timeseries at each preprocessing step.
#' The intended use of this function is to provide a simple method for
#' qualitatively assessing the consequences of the preprocessing recipe and
#' parameters on the raw pupillary signal.
#'
#' @param x An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param ... Additional arguments to be passed to `plot`
#' @param steps Which steps to plot; defaults to `all` (i.e., plot all steps).
#' Otherwise, pass in a vector containing the index of the step(s) you want to
#' plot, with index `1` being the original raw pupil timeseries
#' @param preview_n Number of random example "epochs" to generate for
#' previewing the effect of each preprocessing step on the pupil timeseries
#' @param preview_duration Time in seconds of each randomly selected preview
#' @param preview_window The start and stop raw timestamps used to subset the
#' preprocessed data from each step of the `eyeris` workflow for visualization
#' Defaults to NULL, meaning random epochs as defined by `preview_n` and
#' `preview_duration` will be plotted. To override the random epochs, set
#' `preview_window` here to a vector with relative start and stop times (in
#' seconds), for example -- `c(5,6)` -- to indicate the raw data from 5-6 secs
#' on data that were recorded at 1000 Hz). Note, the start/stop time values
#' indicated here are in seconds because `eyeris` automatically computes the
#' indices for the supplied range of seconds using the `$info$sample.rate`
#' metadata in the `eyeris` S3 class object
#' @param seed Random seed for current plotting session. Leave NULL to select
#' `preview_n` number of random preview "epochs" (of `preview_duration`) each
#' time. Otherwise, choose any seed-integer as you would normally select for
#' [base::set.seed()], and you will be able to continue re-plotting the same
#' random example pupil epochs each time -- which is helpful when adjusting
#' parameters within and across `eyeris` workflow steps
#' @param block For multi-block recordings, specifies which block to plot.
#' Defaults to 1. When a single `.asc` data file contains multiple
#' recording blocks, this parameter determines which block's timeseries to
#' visualize. Must be a positive integer not exceeding the total number of
#' blocks in the recording
#' @param plot_distributions Logical flag to indicate whether to plot both
#' diagnostic pupil timeseries *and* accompanying histograms of the pupil
#' samples at each processing step. Defaults to `FALSE`
#' @param suppress_prompt Logical flag to disable interactive confirmation
#' prompts during plotting. Defaults to `TRUE`, which avoids hanging behavior in
#' non-interactive or automated contexts (e.g., RMarkdown, scripts)
#' Set to `FALSE` only when running inside `glassbox()` with
#' `interactive_preview = TRUE`, where prompting after each step is desired, as
#' well as in the generation of interactive HTML reports with [eyeris::bidsify]
#' @param verbose A logical flag to indicate whether to print status messages to
#' the console. Defaults to `TRUE`. Set to `FALSE` to suppress messages about
#' the current processing step and run silently
#' @param add_progressive_summary Logical flag to indicate whether to add a
#' progressive summary plot after plotting. Defaults to `FALSE`. Set to `TRUE`
#' to enable the progressive summary plot (useful for interactive exploration).
#' Set to `FALSE` to disable the progressive summary plot (useful in automated
#' contexts like bidsify reports)
#' @param num_previews **(Deprecated)** Use `preview_n` instead
#'
#' @return No return value; iteratively plots a subset of the pupil timeseries
#' from each preprocessing step run
#'
#' @seealso [lifecycle::deprecate_warn()]
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
#' plot(my_eyeris_data, seed = 0, add_progressive_summary = TRUE)
#'
#' ## example 2: using a custom time subset (i.e., 1 to 500 ms)
#' plot(
#'   my_eyeris_data,
#'   preview_window = c(0.01, 0.5),
#'   seed = 0,
#'   add_progressive_summary = TRUE
#' )
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
#' ##   (i.e., 1000 to 2000 ms)
#' plot(
#'   my_eyeris_data,
#'   block = 1,
#'   preview_window = c(1, 2),
#'   seed = 0
#' )
#'
#' @rdname plot.eyeris
#'
#' @export
plot.eyeris <- function(x, ..., steps = NULL, preview_n = NULL,
                        preview_duration = NULL, preview_window = NULL,
                        seed = NULL, block = 1, plot_distributions = FALSE,
                        suppress_prompt = TRUE, verbose = TRUE,
                        add_progressive_summary = FALSE,
                        num_previews = deprecated()) {
  # handle deprecated parameters
  if (is_present(num_previews)) {
    deprecate_warn(
      "1.2.0",
      "plot(num_previews)",
      "plot(preview_n)"
    )
    preview_n <- num_previews
  }

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

  non_plot_params <- c(
    "preview_window", "seed", "steps", "num_previews",
    "preview_n", "preview_duration", "block",
    "suppress_prompt", "plot_distributions",
    "only_linear_trend", "next_step", "add_progressive_summary"
  )

  plot_params <- params[!(names(params) %in% non_plot_params)]

  # set param defaults outside of function declaration
  if (!is.null(preview_window)) {
    if (!is.null(preview_n) || !is.null(preview_duration)) {
      cli::cli_alert_warning(
        paste(
          "preview_n and/or preview_duration will be ignored,",
          "since preview_window was specified here."
        )
      )
    }
  }

  if (is.null(steps)) {
    steps <- "all"
  }

  if (is.null(preview_n)) {
    preview_n <- 3
  }

  if (is.null(preview_duration)) {
    preview_duration <- 5 # seconds
  }

  hz <- if (!is.na(x$decimated.sample.rate)) {
    x$decimated.sample.rate
  } else {
    x$info$sample.rate
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
      if (verbose) {
        cli::cli_alert_warning(sprintf(
          "[ INFO ] - Plotting block %d from possible blocks: %s",
          block,
          toString(available_blocks)
        ))
      }
    } else {
      cli::cli_abort(sprintf(
        "[ WARN ] - Block %d does not exist. Available blocks: %d",
        block, toString(available_blocks)
      ))
    }
  } else {
    pupil_data <- x$timeseries$block_1
  }

  if (verbose) {
    alert("info", paste("[ INFO ] - Plotting with sampling rate:", hz, "Hz"))
  }

  pupil_steps <- grep("^pupil_", names(pupil_data), value = TRUE)

  colorpal <- eyeris_color_palette()
  colors <- c("black", colorpal)

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
    withr::with_seed(seed, {
      random_epochs <- draw_random_epochs(
        pupil_data, preview_n,
        preview_duration, hz
      )
    })

    par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
    detrend_plotted <- FALSE
    for (i in seq_along(pupil_steps)) {
      for (n in 1:preview_n) {
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

        y_label <- paste("pupil size", y_units)

        # used when running `plot()` by itself (and thus plotting all steps)
        if (!only_liner_trend) {
          if (grepl("_detrend$", pupil_steps[i]) && !detrend_plotted) {
            # only attempt detrend overlay if detrend_fitted_values exists
            if ("detrend_fitted_values" %in% colnames(pupil_data)) {
              detrend_success <- plot_detrend_overlay(
                pupil_data,
                pupil_steps = pupil_steps,
                preview_n = preview_n,
                suppress_prompt = suppress_prompt
              )

              if (detrend_success) {
                detrend_plotted <- TRUE
              }
            } else {
              detrend_plotted <- TRUE
            }
          }
        } else {
          if (!detrend_plotted) {
            if ("detrend_fitted_values" %in% colnames(pupil_data)) {
              detrend_success <- plot_detrend_overlay(
                pupil_data,
                pupil_steps = pupil_steps,
                preview_n = preview_n,
                suppress_prompt = suppress_prompt
              )

              if (detrend_success) {
                detrend_plotted <- TRUE
              }
            } else {
              detrend_plotted <- TRUE
            }
          }
        }

        if (!is.null(params$next_step)) {
          plot_data <- random_epochs[[n]][[
            params$next_step[length(params$next_step)]
          ]]
        } else {
          plot_data <- random_epochs[[n]][[pupil_steps[i]]]
        }

        is_placeholder <- "message" %in% colnames(random_epochs[[n]]) &&
          any(random_epochs[[n]]$message == "NO_VALID_SAMPLES")
        no_valid_data <- is.null(plot_data) || all(is.na(plot_data))

        if (is_placeholder || no_valid_data) {
          plot(NA,
            xlim = c(0, 1), ylim = c(0, 1), type = "n",
            xlab = "", ylab = "", main = title
          )
          text(
            0.5, 0.5,
            "No valid samples\nin this segment.\n
            Please re-run with a different `report_seed`",
            cex = 0.8, col = "red"
          )
        } else {
          do.call(robust_plot, c(
            list(y = plot_data, x = random_epochs[[n]]$time_scaled),
            plot_params,
            list(
              type = "l", col = colors[i], lwd = 2,
              main = title, xlab = "time (ms)", ylab = y_label
            )
          ))
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
          xlab = y_label,
          backuplab = "pupil size"
        )

        par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
      }
    }
    par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
  } else {
    preview_window_indices <- round(preview_window * hz) + 1
    start_index <- preview_window_indices[1]
    end_index <- preview_window_indices[2]

    if (start_index < 1 || start_index > nrow(pupil_data) ||
          end_index < 1 || end_index > nrow(pupil_data) ||
          start_index >= end_index) {
      cli::cli_abort(
        "Invalid preview_window: start/end index out of range or invalid."
      )
    }

    sliced_pupil_data <- pupil_data[start_index:end_index, ]

    # time axis in ms for proper scaling
    time_ms <- (
      sliced_pupil_data$time_scaled - min(sliced_pupil_data$time_scaled)
    )

    for (i in seq_along(pupil_steps)) {
      st <- pupil_data$time_orig[start_index]
      et <- pupil_data$time_orig[end_index]

      if (grepl("z", pupil_steps[i])) {
        y_units <- "(z)"
      } else {
        y_units <- "(a.u.)"
      }

      y_label <- paste("pupil size", y_units)

      do.call(robust_plot, c(
        list(y = sliced_pupil_data[[pupil_steps[i]]],
             x = sliced_pupil_data$time_scaled),
        plot_params,
        list(
          type = "l",
          col = colors[i],
          lwd = 2,
          main = paste0(
            gsub("_", " > ", gsub("pupil_", "", pupil_steps[i])),
            if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
              sprintf(" (Run %d)", block)
            } else {
              ""
            },
            "\n[", st, " - ", et, " ms] | ",
            "[index: ", preview_window_indices[1], " - ",
            preview_window_indices[2], "]"
          ),
          xlab = "time (secs)",
          ylab = y_label
        )
      ))

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
          xlab = y_label,
          backuplab = "pupil size"
        )
        par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
      }
    }

    par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
  }

  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

  # add progressive summary plot at the end (if requested)
  if (add_progressive_summary) {
    if (verbose) {
      cli::cli_alert_info(
        sprintf("[ INFO ] - Creating progressive summary plot for block_%d",
                block)
      )
    }

    tryCatch({
      make_prog_summary_plot(
        pupil_data = pupil_data,
        pupil_steps = pupil_steps,
        preview_n = preview_n,
        plot_params = plot_params,
        run_id = if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
          paste0("run-", sprintf("%02d", block))
        } else {
          "run-01"
        },
        cex = 1.15
      )

      if (verbose) {
        cli::cli_alert_success(
          "[  OK  ] - Progressive summary plot created successfully!"
        )
      }
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(
          paste("[ WARN ] - Could not create progressive summary plot:",
                e$message)
        )
      }
    })
  }

  # reset plotting parameters to prevent downstream issues
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)
}

#' Draw random epochs for plotting
#'
#' Generates random time segments from the timeseries data for preview plotting.
#'
#' @param x A dataframe containing timeseries data
#' @param n Number of random epochs to draw
#' @param d Duration of each epoch in seconds
#' @param hz Sampling rate in Hz
#'
#' @return A list of dataframes, each containing a random epoch segment
#'
#' @keywords internal
draw_random_epochs <- function(x, n, d, hz) {
  # get number of samples needed for specified duration
  n_samples <- ceiling(d * hz)

  min_time_secs <- min(x$time_secs, na.rm = TRUE)
  max_time_secs <- max(x$time_secs, na.rm = TRUE)

  if ((max_time_secs - min_time_secs) < d) {
    cli::cli_abort("Example duration is longer than the duration of data.")
  }

  # get step size and ensure it's valid for the time range
  step_size <- 1 / hz
  time_range <- max_time_secs - d - min_time_secs

  # case: if step size is larger than available time range, adjust it
  if (step_size > time_range) {
    step_size <- time_range / 10  # use 10 steps as a reasonable minimum?
    if (step_size <= 0) {
      step_size <- 0.001  # fallback to 1ms if still invalid?
    }
  }

  drawn_epochs <- list()
  max_attempts <- 100 # prevent looping forever

  for (i in 1:n) {
    attempts <- 0
    valid_epoch_found <- FALSE

    while (attempts < max_attempts && !valid_epoch_found) {
      rand_start_secs <- sample(
        seq(min_time_secs, max_time_secs - d, by = step_size),
        1
      )
      rand_end_secs <- rand_start_secs + d

      epoch_data <- x |>
        dplyr::filter(time_secs >= rand_start_secs & time_secs < rand_end_secs)

      # ensure proper x-axis scaling with the time_orig column in ms
      epoch_data$time_scaled <- (epoch_data$time_secs - rand_start_secs) * 1000

      pupil_cols <- grep("^pupil_", colnames(epoch_data), value = TRUE)
      if (length(pupil_cols) > 0) {
        has_valid_data <- any(sapply(pupil_cols, function(col) {
          any(is.finite(epoch_data[[col]]))
        }))

        if (has_valid_data) {
          drawn_epochs[[i]] <- epoch_data
          valid_epoch_found <- TRUE
        } else {
          attempts <- attempts + 1
        }
      } else {
        drawn_epochs[[i]] <- epoch_data
        valid_epoch_found <- TRUE
      }
    }

    if (!valid_epoch_found) {
      placeholder_data <- data.frame(
        time_secs = c(rand_start_secs, rand_end_secs),
        time_orig = c(rand_start_secs, rand_end_secs),
        time_scaled = c(0, d * 1000),
        message = c("NO_VALID_SAMPLES", "NO_VALID_SAMPLES")
      )
      drawn_epochs[[i]] <- placeholder_data

      cli::cli_alert_warning(
        paste0(
          "Randomly selected plot segment ", i, " had no valid samples. ",
          "Please re-run with a different `report_seed`."
        )
      )
    }
  }

  drawn_epochs
}

#' Robust plotting function with error handling
#'
#' A wrapper around base plotting functions that handles errors and missing
#' data gracefully.
#'
#' @param y The y-axis data to plot
#' @param x The x-axis data (optional, defaults to sequence)
#' @param ... Additional arguments passed to plot()
#'
#' @return No return value; creates a plot or displays warning messages
#'
#' @keywords internal
robust_plot <- function(y, x = NULL, ...) {
  tryCatch(
    {
      if (length(y) == 0 || all(is.na(y))) {
        cli::cli_alert_warning("No finite data to plot.")
        return(invisible(NULL))
      }

      dots <- list(...)
      col_user <- if ("col" %in% names(dots)) dots$col else "blue"

      # store original y for getting NA positions
      y_orig <- y

      # if x is NULL, use 1:length(y)
      if (is.null(x)) {
        x_seq <- seq_along(y_orig)
      } else {
        x_seq <- x
      }

      # init placeholder line
      plot(x_seq, ifelse(is.na(y_orig), NA, y_orig),
        xlim = range(x_seq, na.rm = TRUE),
        ...
      )

      # add vertical lines where there are NAs (using x values if available)
      na_idx <- which(is.na(y_orig))
      if (length(na_idx) > 0) {
        abline(
          v = if (!is.null(x)) x_seq[na_idx] else na_idx, col = "black", lty = 2
        )
      }

      # replace NA with -1 after drawing NA lines for continuity
      y_clean <- y_orig
      y_clean[is.na(y_clean)] <- -1
      lines(x_seq, y_clean, col = col_user)
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

#' Plot pupil distribution histogram
#'
#' Creates a histogram of pupil size distribution with customizable parameters.
#'
#' @param data The pupil data to plot
#' @param color The color for the histogram bars
#' @param main The main title for the plot
#' @param xlab The x-axis label
#' @param backuplab A backup label if xlab is NULL
#'
#' @return No return value; creates a histogram plot
#'
#' @keywords internal
plot_pupil_distribution <- function(data, color, main, xlab, backuplab = NULL) {
  # safely handle user's current options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

  new_xlab <- if (!is.null(xlab)) {
    xlab
  } else if (!is.null(backuplab)) {
    backuplab
  } else {
    "pupil size"
  }

  hist(
    data,
    main = main,
    xlab = new_xlab,
    ylab = "frequency (count)",
    col = color,
    border = "white",
    breaks = "FD"
  )
}

#' Draw vertical lines at NA positions
#'
#' Adds vertical dashed lines at positions where y values are NA.
#'
#' @param x The x-axis values
#' @param y The y-axis values
#' @param ... Additional arguments passed to abline()
#'
#' @return No return value; adds lines to the current plot
#'
#' @keywords internal
draw_na_lines <- function(x, y, ...) {
  na_idx <- which(is.na(y))
  abline(v = x[na_idx], col = "black", lty = 2, ...)
}

#' Internal helper to plot detrending overlay
#'
#' This function replicates the exact detrending visualization from the
#' `glassbox()` interactive preview mode. It uses `robust_plot()` to show the
#' most recent detrended pupil signal overlaid with the fitted linear trend.
#'
#' @param pupil_data A single block of pupil timeseries data
#' (e.g. `eyeris$timeseries$block_1`)
#' @param preview_n Number of columns for `par(mfrow)`. Default = 3.
#' @param plot_params A named list of additional parameters to forward to
#' `robust_plot()`
#' @param suppress_prompt Logical. Whether to skip prompting. Default = TRUE.
#'
#' @return Logical indicating whether detrend overlay was plotted successfully
#'
#' @keywords internal
plot_detrend_overlay <- function(pupil_data,
                                 pupil_steps,
                                 preview_n = preview_n,
                                 plot_params = list(),
                                 suppress_prompt = TRUE) {
  # store current par settings to restore them in case func returns early
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

  detrend_step <- grep("_detrend$", pupil_steps, value = TRUE)

  all_cols <- colnames(pupil_data)
  detrend_fitted_index <- which(all_cols == "detrend_fitted_values")

  # guard if detrend_fitted_values exists and has a valid previous column
  if (length(detrend_fitted_index) == 0) {
    cli::cli_alert_danger(
      "detrend_fitted_values not found in eyeris S3 object."
    )
    par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
    return(FALSE)
  }

  if (detrend_fitted_index <= 1) {
    cli::cli_alert_warning(
      "No previous pupil column found to plot detrend overlay against. ",
      "This can happen when detrend is the only preprocessing step enabled."
    )
    # restore main plotting func layout
    par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
    return(FALSE)
  }

  prev_col <- all_cols[detrend_fitted_index - 1]

  # ensure prev col is a pupil col
  if (!grepl("^pupil_", prev_col)) {
    cli::cli_alert_warning(
      "Previous column is not a pupil column. Cannot plot detrend overlay."
    )
    # restore main plotting func layout
    par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
    return(FALSE)
  }

  ydat <- pupil_data[[prev_col]]
  xdat <- pupil_data$time_secs

  do.call(robust_plot, c(
    list(y = ydat, x = xdat),
    plot_params,
    list(
      type = "l",
      col = "black",
      lwd = 2,
      main = paste0(
        "detrend:\n",
        gsub("_", " > ", gsub("pupil_", "", detrend_step))
      ),
      xlab = "tracker time (s)",
      ylab = "pupil size (a.u.)"
    )
  ))

  lines(pupil_data$time_secs,
    pupil_data$detrend_fitted_values,
    type = "l", col = "blue", lwd = 2, lty = 1
  )

  legend("topleft",
    legend = c("pupil timeseries", "linear trend"),
    col = c("black", "blue"), lwd = 2, lty = c(1, 1)
  )

  par(mfrow = c(1, preview_n), oma = c(0, 0, 3, 0))
  if (!suppress_prompt) prompt_user()

  return(TRUE)
}

#' Create gaze heatmap of eye coordinates
#'
#' Creates a heatmap showing the distribution of eye_x and eye_y coordinates
#' across the entire screen area. The heatmap shows where the participant
#' looked most frequently during the recording period.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param block Block number to plot (default: 1)
#' @param screen_width Screen width in pixels from eyeris$info$screen.x
#' @param screen_height Screen height in pixels from eyeris$info$screen.y
#' @param n_bins Number of bins for the heatmap grid (default: 50)
#' @param col_palette Color palette for the heatmap (default: "viridis")
#' @param main Title for the plot (default: "Fixation Heatmap")
#' @param xlab X-axis label (default: "Screen X (pixels)")
#' @param ylab Y-axis label (default: "Screen Y (pixels)")
#' @param sample_rate Sample rate in Hz (optional)
#'
#' @return No return value; creates a heatmap plot
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#' eyeris_preproc <- glassbox(demo_data)
#' plot_gaze_heatmap(eyeris = eyeris_preproc, block = 1)
#'
#' @export
plot_gaze_heatmap <- function(eyeris, block = 1, screen_width = NULL,
                              screen_height = NULL,
                              n_bins = 50, col_palette = "viridis",
                              main = "Gaze Heatmap",
                              xlab = "Screen X (pixels)",
                              ylab = "Screen Y (pixels)",
                              sample_rate = NULL) {
  if (inherits(eyeris, "eyeris")) {
    block_str <- paste0("block_", block)
    if (is.null(screen_width)) screen_width <- eyeris$info$screen.x
    if (is.null(screen_height)) screen_height <- eyeris$info$screen.y

    df <- eyeris$timeseries[[block_str]]
    if (!is.data.frame(df)) {
      warning("Block not found in eyeris object.")
      return(invisible(NULL))
    }
  } else {
    df <- eyeris
    if (is.null(screen_width) || is.null(screen_height)) {
      stop("screen width and height must be provided with dataframe inputs.")
    }
  }

  if (!all(c("eye_x", "eye_y") %in% colnames(df))) {
    warning("eye_x and/or eye_y columns not found in input data.")
    return(invisible(NULL))
  }

  valid_coords <- !is.na(df$eye_x) & !is.na(df$eye_y)
  if (sum(valid_coords) == 0) {
    warning("No valid eye coordinates found")
    return(invisible(NULL))
  }

  x_coords <- df$eye_x[valid_coords]
  y_coords <- df$eye_y[valid_coords]

  tryCatch({
    dens <- MASS::kde2d(x_coords, y_coords, n = n_bins,
                        lims = c(0, screen_width, 0, screen_height))
    norm_density <- dens$z / max(dens$z, na.rm = TRUE)

    if (col_palette == "viridis") {
      colors <- viridis::viridis(100)
    } else if (col_palette == "plasma") {
      colors <- viridis::plasma(100)
    } else if (col_palette == "inferno") {
      colors <- viridis::inferno(100)
    } else if (col_palette == "magma") {
      colors <- viridis::magma(100)
    } else {
      colors <- grDevices::heat.colors(100)
    }

    fields::image.plot(
      x = dens$x, y = dens$y,
      z = t(norm_density)[, rev(seq_len(nrow(norm_density)))],
      col = colors, main = main, xlab = xlab, ylab = ylab,
      xlim = c(0, screen_width), ylim = c(screen_height, 0),
      legend.lab = "Normalized density", legend.line = 2.5,
      zlim = c(0, 1)
    )
    rect(0, 0, screen_width, screen_height, border = "black", lwd = 2)
    points(screen_width / 2, screen_height / 2, pch = 3, col = "red", cex = 1.5)
  }, error = function(e) {
    plot(x_coords, y_coords,
         pch = 16, cex = 0.5,
         col = grDevices::adjustcolor("blue", alpha.f = 0.6),
         main = main, xlab = xlab, ylab = ylab,
         xlim = c(0, screen_width), ylim = c(screen_height, 0))
    rect(0, 0, screen_width, screen_height, border = "black", lwd = 2)
    points(screen_width / 2, screen_height / 2, pch = 3, col = "red", cex = 1.5)
  })
}
