#' Plot pre-processed pupil data from `eyeris`
#'
#' S3 plotting method for objects of class `eyeris`. Plots a single-panel
#' timeseries for a subset of the pupil time series at each preprocessing step.
#' The intended use of this function is to provide a simple method for
#' qualitatively assessing the consequences of the preprocessing recipe and
#' parameters on the raw pupillary signal.
#'
#' @param x An object of class `eyeris` derived from [eyeris::load_asc()]
#' @param ... Additional arguments to be passed to `plot`
#' @param steps Which steps to plot; defaults to `all` (i.e., plot all steps).
#' Otherwise, pass in a vector containing the index of the step(s) you want to
#' plot, with index `1` being the original raw pupil time series
#' @param preview_n Number of random example "epochs" to generate for
#' previewing the effect of each preprocessing step on the pupil time series
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
#' recording blocks, this parameter determines which block's time series to
#' visualize. Must be a positive integer not exceeding the total number of
#' blocks in the recording
#' @param plot_distributions Logical flag to indicate whether to plot both
#' diagnostic pupil time series *and* accompanying histograms of the pupil
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
#' @param eye For binocular data, specifies which eye to plot: "left", "right",
#' or "both". Defaults to "left". For "both", currently plots left eye data
#' (use eye="right" for right eye data)
#' @param num_previews **(Deprecated)** Use `preview_n` instead
#'
#' @return No return value; iteratively plots a subset of the pupil time series
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
#' # controlling the time series range (i.e., preview window) in your plots:
#' #  (note: omitting `preview_window` uses the default 10000 to 20000 ms
#' #   subset; the examples below pass shorter windows explicitly so that they
#' #   run quickly)
#'
#' ## example 1: a 2-second time subset (i.e., 10000 to 12000 ms)
#' plot(
#'   my_eyeris_data,
#'   preview_window = c(10, 12),
#'   seed = 0,
#'   add_progressive_summary = TRUE
#' )
#'
#' ## example 2: using a custom time subset (i.e., 1 to 500 ms)
#' plot(
#'   my_eyeris_data,
#'   preview_window = c(0.01, 0.5),
#'   seed = 0
#' )
#'
#' # controlling which block of data you would like to plot:
#' #  (`block` defaults to 1, i.e., the first block; here it is combined with a
#' #   custom preview window of 1000 to 2000 ms)
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
plot.eyeris <- function(
  x,
  ...,
  steps = NULL,
  preview_n = NULL,
  preview_duration = NULL,
  preview_window = NULL,
  seed = NULL,
  block = 1,
  plot_distributions = FALSE,
  suppress_prompt = TRUE,
  verbose = TRUE,
  add_progressive_summary = FALSE,
  eye = c("left", "right", "both"),
  num_previews = deprecated()
) {
  # handle deprecated parameters
  if (is_present(num_previews)) {
    deprecate_warn("1.2.0", "plot(num_previews)", "plot(preview_n)")
    preview_n <- num_previews
  }

  eye_suffix <- NULL
  # handle binocular eyeris objects
  eye <- match.arg(eye)
  if (is_binocular_object(x)) {
    if (eye == "left") {
      x <- x$left
      eye_suffix <- "eye-L"
      log_info("Plotting left eye data", verbose = verbose)
    } else if (eye == "right") {
      x <- x$right
      eye_suffix <- "eye-R"
      log_info("Plotting right eye data", verbose = verbose)
    } else if (eye == "both") {
      x <- x$left
      log_info(
        "Plotting left eye data (use eye='right' for right eye)",
        verbose = verbose
      )
    }
  }

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
    "preview_window",
    "seed",
    "steps",
    "num_previews",
    "preview_n",
    "preview_duration",
    "block",
    "suppress_prompt",
    "plot_distributions",
    "only_linear_trend",
    "next_step",
    "add_progressive_summary",
    "eye"
  )

  plot_params <- params[!(names(params) %in% non_plot_params)]

  # set param defaults outside of function declaration
  if (!is.null(preview_window)) {
    if (!is.null(preview_n) || !is.null(preview_duration)) {
      log_warn(
        "preview_n and/or preview_duration will be ignored,",
        "since preview_window was specified here."
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
    available_blocks_sorted <- sort(as.numeric(available_blocks), na.last = NA)

    if (block %in% available_blocks) {
      pupil_data <- x$timeseries[[paste0("block_", block)]]
      log_info(
        "Plotting block {block} with sampling rate {hz} Hz from possible blocks: {toString(available_blocks_sorted)}",
        verbose = verbose
      )
    } else {
      log_error(
        "Block {block} does not exist. Available blocks: {toString(available_blocks_sorted)}"
      )
    }
  } else {
    pupil_data <- x$timeseries$block_1
  }

  pupil_steps <- grep("^pupil_", names(pupil_data), value = TRUE)

  # when a downsample/bin step has run, the stored timeseries only retains the
  # decimated samples. plot earlier (pre-decimation) pipeline steps from the
  # preserved full-resolution data so they are not misleadingly shown at the
  # decimated sampling rate (issue #294)
  full_pupil_data <- get_pre_decimation_block(x, block)
  has_decimation <- !is.null(full_pupil_data)
  full_hz <- x$info$sample.rate

  colorpal <- eyeris_color_palette()
  colors <- c("black", colorpal)

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
        pupil_data,
        preview_n,
        preview_duration,
        hz
      )
    })

    # build matching full-resolution epochs (over the same time windows) so
    # that pre-decimation steps can be plotted at their original sampling rate
    random_epochs_full <- NULL
    if (has_decimation) {
      epoch_starts <- vapply(
        random_epochs,
        function(e) min(e$time_secs, na.rm = TRUE),
        numeric(1)
      )
      random_epochs_full <- lapply(epoch_starts, function(s0) {
        slice_epoch_window(full_pupil_data, s0, preview_duration)
      })
    }

    detrend_plotted <- FALSE
    for (i in seq_along(pupil_steps)) {
      use_full <- has_decimation && !is_decimated_col(pupil_steps[i])
      panels <- vector("list", preview_n)
      for (n in 1:preview_n) {
        epoch_n <- if (use_full) random_epochs_full[[n]] else random_epochs[[n]]
        st <- min(epoch_n$time_orig, na.rm = TRUE)
        et <- max(epoch_n$time_orig, na.rm = TRUE)
        title <- paste0("[", st, " - ", et, "]")
        header <- paste0(
          gsub("_", " > ", gsub("pupil_", "", pupil_steps[i])),
          if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
            paste(
              sprintf(" (Run %d)", block),
              if (!is.null(eye_suffix)) paste0(" (", eye_suffix, ")") else ""
            )
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
          plot_data <- epoch_n[[params$next_step[length(params$next_step)]]]
        } else {
          plot_data <- epoch_n[[pupil_steps[i]]]
        }

        is_placeholder <- "message" %in%
          colnames(epoch_n) &&
          any(epoch_n$message == "NO_VALID_SAMPLES")
        no_valid_data <- is.null(plot_data) || all(is.na(plot_data))

        if (is_placeholder || no_valid_data) {
          panels[[n]] <- rb_blank_panel(
            paste0(
              "No valid samples\nin this segment.\n",
              "Please re-run with a different `report_seed`"
            ),
            title = title
          )
        } else {
          panels[[n]] <- do.call(
            robust_plot,
            c(
              list(y = plot_data, x = epoch_n$time_scaled),
              plot_params,
              list(
                col = colors[i],
                main = title,
                xlab = "time (ms)",
                ylab = y_label
              )
            )
          )
        }
      }

      rb_print(panels, title = header)

      if (plot_distributions) {
        plot_pupil_distribution(
          data = if (use_full) {
            full_pupil_data[[pupil_steps[i]]]
          } else {
            pupil_data[[pupil_steps[i]]]
          },
          color = colors[i],
          main = header,
          xlab = y_label,
          backuplab = "pupil size"
        )
      }
    }
  } else {
    for (i in seq_along(pupil_steps)) {
      # plot pre-decimation steps from the preserved full-resolution data at
      # the original sampling rate; decimated steps stay on the decimated data
      use_full <- has_decimation && !is_decimated_col(pupil_steps[i])
      this_data <- if (use_full) full_pupil_data else pupil_data
      this_hz <- if (use_full) full_hz else hz

      preview_window_indices <- round(preview_window * this_hz) + 1
      start_index <- preview_window_indices[1]
      end_index <- preview_window_indices[2]

      if (use_full) {
        # the preserved full-resolution frame can extend slightly past the
        # decimated frame the preview_window was derived from; clamp rather
        # than error so the same window can be drawn at full resolution
        start_index <- max(1, start_index)
        end_index <- min(nrow(this_data), end_index)
      }

      if (
        start_index < 1 ||
          start_index > nrow(this_data) ||
          end_index < 1 ||
          end_index > nrow(this_data) ||
          start_index >= end_index
      ) {
        log_error(
          "Invalid preview_window: start/end index out of range or invalid."
        )
      }

      sliced_pupil_data <- this_data[start_index:end_index, ]

      st <- this_data$time_orig[start_index]
      et <- this_data$time_orig[end_index]

      if (grepl("z", pupil_steps[i])) {
        y_units <- "(z)"
      } else {
        y_units <- "(a.u.)"
      }

      y_label <- paste("pupil size", y_units)

      rb_print(do.call(
        robust_plot,
        c(
          list(
            y = sliced_pupil_data[[pupil_steps[i]]],
            x = sliced_pupil_data$time_scaled
          ),
          plot_params,
          list(
            col = colors[i],
            main = paste0(
              gsub("_", " > ", gsub("pupil_", "", pupil_steps[i])),
              if (is.list(x$timeseries) && !is.data.frame(x$timeseries)) {
                sprintf(" (Run %d)", block)
              } else {
                ""
              },
              "\n[",
              st,
              " - ",
              et,
              " ms] | ",
              "[index: ",
              preview_window_indices[1],
              " - ",
              preview_window_indices[2],
              "]"
            ),
            xlab = "time (secs)",
            ylab = y_label
          )
        )
      ))

      if (plot_distributions) {
        plot_pupil_distribution(
          data = this_data[[pupil_steps[i]]],
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
      }
    }
  }

  # add progressive summary plot at the end (if requested)
  if (add_progressive_summary) {
    log_info(
      "Creating progressive summary plot for block_{block}",
      verbose = verbose
    )

    tryCatch(
      {
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
          cex = 1.15,
          full_pupil_data = full_pupil_data
        )

        log_success(
          "Progressive summary plot created successfully!",
          verbose = verbose
        )
      },
      error = function(e) {
        log_warn(
          "Could not create progressive summary plot: {e$message}",
          verbose = verbose
        )
      }
    )
  }

  invisible(NULL)
}

#' Identify decimated (downsample/bin) pupil columns
#'
#' A pupil column is considered "decimated" if it was produced by the
#' `downsample()`/`bin()` step or by any step that follows it. Such columns
#' are stored at the decimated sampling rate, whereas earlier columns should
#' be plotted from the preserved full-resolution data (see issue #294).
#'
#' @param col A character vector of pupil column names
#'
#' @return A logical vector that is `TRUE` for decimated columns
#'
#' @keywords internal
is_decimated_col <- function(col) {
  # match the `_downsample`/`_bin` operation suffix whether it is the final
  # column (e.g. `..._downsample`) or has later steps appended (e.g.
  # `..._downsample_detrend`, `..._bin_z`), while avoiding accidental matches
  # mid-token in unrelated names
  grepl("(_downsample|_bin)($|_)", col)
}

#' Retrieve the preserved full-resolution data for a block
#'
#' Returns the full-resolution (pre-decimation) time series for the requested
#' block, if it was preserved when a `downsample()`/`bin()` step ran. Used so
#' that diagnostic plots of earlier pipeline steps can be rendered at their
#' original sampling rate rather than the decimated rate (see issue #294).
#'
#' @param x An object of class `eyeris`
#' @param block The block number to retrieve
#'
#' @return A data frame of full-resolution time series data for the block, or
#' `NULL` if no pre-decimation data was preserved
#'
#' @keywords internal
get_pre_decimation_block <- function(x, block) {
  pre <- x$timeseries_pre_decimation
  if (is.null(pre)) {
    return(NULL)
  }
  if (is.data.frame(pre)) {
    return(pre)
  }
  pre[[paste0("block_", block)]]
}

#' Slice a fixed time window out of a time series for previewing
#'
#' Extracts the rows of `df` whose `time_secs` fall within
#' `[start_secs, start_secs + d)` and (re)computes a `time_scaled` column (in
#' milliseconds, relative to the window start). Used to draw a full-resolution
#' preview epoch over the same time window selected on the decimated data.
#'
#' @param df A data frame containing a `time_secs` column
#' @param start_secs The start of the window in seconds
#' @param d The window duration in seconds
#'
#' @return A data frame containing the rows that fall within the window
#'
#' @keywords internal
slice_epoch_window <- function(df, start_secs, d) {
  in_window <- df$time_secs >= start_secs & df$time_secs < (start_secs + d)
  epoch <- df[in_window, , drop = FALSE]
  epoch$time_scaled <- (epoch$time_secs - start_secs) * 1000
  epoch
}

#' Draw random epochs for plotting
#'
#' Generates random time segments from the time series data for preview plotting.
#'
#' @param x A data frame containing time series data
#' @param n Number of random epochs to draw
#' @param d Duration of each epoch in seconds
#' @param hz Sampling rate in Hz
#'
#' @return A list of data frames, each containing a random epoch segment
#'
#' @keywords internal
draw_random_epochs <- function(x, n, d, hz) {
  # get number of samples needed for specified duration
  n_samples <- ceiling(d * hz)

  min_time_secs <- min(x$time_secs, na.rm = TRUE)
  max_time_secs <- max(x$time_secs, na.rm = TRUE)

  if ((max_time_secs - min_time_secs) < d) {
    log_error("Example duration is longer than the duration of data.")
  }

  # get step size and ensure it's valid for the time range
  step_size <- 1 / hz
  time_range <- max_time_secs - d - min_time_secs

  # case: if step size is larger than available time range, adjust it
  if (step_size > time_range) {
    step_size <- time_range / 10 # use 10 steps as a reasonable minimum?
    if (step_size <= 0) {
      step_size <- 0.001 # fallback to 1ms if still invalid?
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

      log_warn(
        "Randomly selected plot segment {i} had no valid samples. Please re-run with a different `report_seed`."
      )
    }
  }

  drawn_epochs
}

#' Robust plotting function with error handling
#'
#' Builds a single-series pupil time series panel with `reaborn`, handling
#' errors and missing data gracefully. Returns a `ggplot` object (rather than
#' drawing directly) so callers can combine several panels into a single
#' `patchwork` row before printing to the active device.
#'
#' @param y The y-axis data to plot
#' @param x The x-axis data (optional, defaults to sequence)
#' @param ... Additional arguments; `col`, `main`, `xlab`, and `ylab` control
#' the line colour, title, and axis labels (any base-graphics style arguments
#' such as `type`/`lwd` are accepted and ignored)
#'
#' @return A `ggplot` object
#'
#' @keywords internal
robust_plot <- function(y, x = NULL, ...) {
  dots <- list(...)
  col_user <- if ("col" %in% names(dots)) dots$col else "#377EB8"
  title <- if ("main" %in% names(dots)) dots$main else NULL
  xlab <- if ("xlab" %in% names(dots)) dots$xlab else "time (ms)"
  ylab <- if ("ylab" %in% names(dots)) dots$ylab else "pupil size"

  tryCatch(
    {
      if (is.null(x)) {
        x <- seq_along(y)
      }
      rb_timeseries_panel(
        x = x,
        y = y,
        color = col_user,
        title = title,
        xlab = xlab,
        ylab = ylab
      )
    },
    error = function(e) {
      log_warn("An error occurred during plotting: {e$message}")
      rb_blank_panel(
        "An error occurred\nduring plotting",
        title = title,
        xlab = xlab,
        ylab = ylab
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
  p <- rb_histogram(
    data = data,
    color = color,
    title = main,
    xlab = xlab,
    backuplab = backuplab
  )
  # print to the active device; suppress the benign scale/aesthetic chatter
  # that ggplot2 emits so diagnostic plotting stays quiet during reports
  suppressMessages(suppressWarnings(print(p)))
  invisible(NULL)
}

#' Internal helper to plot detrending overlay
#'
#' This function replicates the exact detrending visualization from the
#' `glassbox()` interactive preview mode. It uses `reaborn` to show the most
#' recent pre-detrend pupil signal overlaid with the fitted trend, and
#' prints the resulting `ggplot` to the active device.
#'
#' @param pupil_data A single block of pupil time series data
#' (e.g. `eyeris$timeseries$block_1`)
#' @param pupil_steps Character vector of pupil column names
#' @param preview_n Unused; retained for backwards compatibility.
#' @param plot_params Unused; retained for backwards compatibility.
#' @param suppress_prompt Logical. Whether to skip prompting. Default = TRUE.
#'
#' @return Logical indicating whether detrend overlay was plotted successfully
#'
#' @keywords internal
plot_detrend_overlay <- function(
  pupil_data,
  pupil_steps,
  preview_n = preview_n,
  plot_params = list(),
  suppress_prompt = TRUE
) {
  detrend_step <- grep("_detrend$", pupil_steps, value = TRUE)

  all_cols <- colnames(pupil_data)
  detrend_fitted_index <- which(all_cols == "detrend_fitted_values")

  # guard if detrend_fitted_values exists and has a valid previous column
  if (length(detrend_fitted_index) == 0) {
    log_warn("detrend_fitted_values not found in eyeris S3 object.")
    return(FALSE)
  }

  if (detrend_fitted_index <= 1) {
    log_warn(
      "No previous pupil column found to plot detrend overlay against.",
      "This can happen when detrend is the only preprocessing step enabled."
    )
    return(FALSE)
  }

  prev_col <- all_cols[detrend_fitted_index - 1]

  # ensure prev col is a pupil col
  if (!grepl("^pupil_", prev_col)) {
    log_warn(
      "Previous column is not a pupil column. Cannot plot detrend overlay."
    )
    return(FALSE)
  }

  dstep <- detrend_step[length(detrend_step)]

  # combine the pre-detrend pupil signal and the fitted trend into a
  # single long data frame so `reaborn` draws them as two hue-mapped lines with
  # an automatic legend (replacing the base-graphics overlay + legend())
  line_df <- rbind(
    data.frame(
      .x = pupil_data$time_secs,
      .y = pupil_data[[prev_col]],
      series = "pupil time series",
      stringsAsFactors = FALSE
    ),
    data.frame(
      .x = pupil_data$time_secs,
      .y = pupil_data$detrend_fitted_values,
      series = "fitted trend",
      stringsAsFactors = FALSE
    )
  )
  line_df <- line_df[is.finite(line_df$.y), , drop = FALSE]

  p <- rb_quiet(reaborn::lineplot(
    data = line_df,
    x = ".x",
    y = ".y",
    hue = "series",
    hue_order = c("pupil time series", "fitted trend"),
    palette = c("black", "blue"),
    estimator = NULL
  )) +
    ggplot2::labs(
      title = paste0("detrend:\n", gsub("_", " > ", gsub("pupil_", "", dstep))),
      x = "tracker time (s)",
      y = "pupil size (a.u.)"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  suppressMessages(suppressWarnings(print(p)))

  if (!suppress_prompt) {
    prompt_user()
  }

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
#' @param screen_width Screen width in pixels from `eyeris$info$screen.x`
#' @param screen_height Screen height in pixels from `eyeris$info$screen.y`
#' @param n_bins Number of bins for the heatmap grid (default: 50)
#' @param col_palette Color palette for the heatmap (default: "viridis")
#' @param main Title for the plot (default: "Fixation Heatmap")
#' @param xlab X-axis label (default: "Screen X (pixels)")
#' @param ylab Y-axis label (default: "Screen Y (pixels)")
#' @param sample_rate Sample rate in Hz (optional)
#' @param eye_suffix Eye suffix for binocular data (default: NULL)
#'
#' @return No return value; creates a heatmap plot
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#' eyeris_preproc <- glassbox(demo_data)
#' plot_gaze_heatmap(eyeris = eyeris_preproc, block = 1)
#'
#' @export
plot_gaze_heatmap <- function(
  eyeris,
  block = 1,
  screen_width = NULL,
  screen_height = NULL,
  n_bins = 50,
  col_palette = "viridis",
  main = "Gaze Heatmap",
  xlab = "Screen X (pixels)",
  ylab = "Screen Y (pixels)",
  sample_rate = NULL,
  eye_suffix = NULL
) {
  if (inherits(eyeris, "eyeris")) {
    block_str <- paste0("block_", block)
    if (is.null(screen_width)) {
      screen_width <- eyeris$info$screen.x
    }
    if (is.null(screen_height)) {
      screen_height <- eyeris$info$screen.y
    }

    df <- eyeris$timeseries[[block_str]]
    if (!is.data.frame(df)) {
      log_warn("Block not found in eyeris object.")
      return(invisible(NULL))
    }
  } else {
    df <- eyeris
    if (is.null(screen_width) || is.null(screen_height)) {
      log_error(
        "Screen width and height must be provided with data frame inputs."
      )
    }
  }

  if (!all(c("eye_x", "eye_y") %in% colnames(df))) {
    log_warn("eye_x and/or eye_y columns not found in input data.")
    return(invisible(NULL))
  }

  valid_coords <- !is.na(df$eye_x) & !is.na(df$eye_y)
  if (sum(valid_coords) == 0) {
    log_warn("No valid eye coordinates found")
    return(invisible(NULL))
  }

  x_coords <- df$eye_x[valid_coords]
  y_coords <- df$eye_y[valid_coords]

  if (!is.null(eye_suffix)) {
    main <- paste0(main, " - ", eye_suffix)
  }

  # map the requested palette to a viridis colormap option
  vir_option <- switch(
    col_palette,
    viridis = "viridis",
    plasma = "plasma",
    inferno = "inferno",
    magma = "magma",
    "viridis"
  )

  df_xy <- data.frame(.x = x_coords, .y = y_coords)

  # screen coordinates have their origin at the top-left, so the y axis is
  # reversed (scale_y_reverse actually flips the axis, unlike a reversed
  # coord_cartesian ylim); the centre fixation cross is drawn on top
  screen_frame <- function(p) {
    p +
      ggplot2::annotate(
        "point",
        x = screen_width / 2,
        y = screen_height / 2,
        shape = 3,
        colour = "red",
        size = 3,
        stroke = 1
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, screen_width),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_reverse(limits = c(screen_height, 0), expand = c(0, 0)) +
      ggplot2::labs(title = main, x = xlab, y = ylab) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }

  p <- tryCatch(
    {
      # estimate a smooth 2D kernel density over the full screen and render it
      # as a viridis raster so the colour gradient reflects where gaze samples
      # concentrated. reaborn's kdeplot only draws discrete filled contour
      # bands, which collapse to a single colour on tightly clustered gaze data
      dens <- MASS::kde2d(
        x_coords,
        y_coords,
        n = n_bins,
        lims = c(0, screen_width, 0, screen_height)
      )
      grid <- expand.grid(gx = dens$x, gy = dens$y)
      grid$gz <- as.vector(dens$z) / max(dens$z, na.rm = TRUE)

      suppressMessages(suppressWarnings(screen_frame(
        ggplot2::ggplot(
          grid,
          ggplot2::aes(x = .data$gx, y = .data$gy, fill = .data$gz)
        ) +
          ggplot2::geom_raster(interpolate = TRUE) +
          viridis::scale_fill_viridis(
            option = vir_option,
            name = "normalized\ndensity",
            limits = c(0, 1)
          )
      )))
    },
    error = function(e) {
      # fall back to a simple scatter of the raw gaze coordinates
      suppressMessages(suppressWarnings(screen_frame(rb_quiet(reaborn::scatterplot(
        data = df_xy,
        x = ".x",
        y = ".y",
        color = grDevices::adjustcolor("blue", alpha.f = 0.6)
      )))))
    }
  )

  suppressMessages(suppressWarnings(print(p)))
  invisible(NULL)
}

#' Plot binocular correlation between left and right eye data
#'
#' Creates correlation plots showing the relationship between left and right eye
#' measurements for pupil size, x-coordinates, and y-coordinates. This function
#' is useful for validating binocular data quality and assessing the correlation
#' between the two eyes.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()]
#'   with binocular data, or a list containing `left` and `right` eyeris objects
#'   (from `binocular_mode = "both"`)
#' @param block Block number to plot (default: 1)
#' @param variables Variables to plot correlations for. Defaults to
#'   `c("pupil", "x", "y")` for pupil size, x-coordinates, and y-coordinates
#' @param main Title for the overall plot (default: "Binocular Correlation")
#' @param col_palette Color palette for the plots (default: "viridis")
#' @param sample_rate Sample rate in Hz (optional, for time-based sampling)
#' @param verbose Logical flag to indicate whether to print status messages
#'   (default: TRUE)
#'
#' @return No return value; creates correlation plots
#'
#' @examples
#' # For binocular data loaded with binocular_mode = "both"
#' binocular_data <- load_asc(eyelink_asc_binocular_demo_dataset(), binocular_mode = "both")
#' plot_binocular_correlation(binocular_data)
#'
#' # For binocular data loaded with binocular_mode = "average"
#' # (correlation plot will show original left vs right before averaging)
#' avg_data <- load_asc(eyelink_asc_binocular_demo_dataset(), binocular_mode = "average")
#' plot_binocular_correlation(avg_data$raw_binocular_object)
#'
#' @export
plot_binocular_correlation <- function(
  eyeris,
  block = 1,
  variables = c("pupil", "x", "y"),
  main = "",
  col_palette = "viridis",
  sample_rate = NULL,
  verbose = TRUE
) {
  # check if a binocular object (from binocular_mode = "both")
  if (is_binocular_object(eyeris)) {
    left_data <- eyeris$left
    right_data <- eyeris$right
    has_binocular <- TRUE
  } else {
    # check if a regular eyeris object with binocular columns
    if (all(c("left", "right") %in% names(eyeris))) {
      left_data <- eyeris$left
      right_data <- eyeris$right
      has_binocular <- TRUE
    } else {
      left_data <- eyeris
      right_data <- eyeris
      has_binocular <- isTRUE(eyeris$binocular)
    }
  }

  block_str <- paste0("block_", block)

  if (has_binocular) {
    if (!block_str %in% names(left_data$timeseries)) {
      log_warn("Block {block} not found in left eye data")
    }
    if (!block_str %in% names(right_data$timeseries)) {
      log_warn("Block {block} not found in right eye data")
    }

    left_df <- left_data$timeseries[[block_str]]
    right_df <- right_data$timeseries[[block_str]]

    # require exact match for pupil_raw; if not present, skip plot with message
    if (!"pupil_raw" %in% colnames(left_df)) {
      suppressMessages(suppressWarnings(print(rb_blank_panel(
        "Skipped: No pupil_raw column\nfound in left eye data",
        title = "Binocular Correlation"
      ))))
      log_warn("Skipped: No pupil_raw column found in left eye data")
      return(invisible(NULL))
    }
    pupil_col <- grep("^pupil_", colnames(left_df), value = TRUE)
    if (length(pupil_col) == 0) {
      log_warn("No pupil columns found in left eye data")
    }
    pupil_col <- pupil_col[1] # use the first pupil column

    pupil_col <- "pupil_raw"
    left_pupil <- left_df[[pupil_col]]
    right_pupil <- right_df[[pupil_col]]
    left_x <- left_df$eye_x
    left_y <- left_df$eye_y
    right_x <- right_df$eye_x
    right_y <- right_df$eye_y
  } else {
    # for regular eyeris objects, check for binocular columns
    if (!block_str %in% names(left_data$timeseries)) {
      log_warn("Block {block} not found in eyeris data")
    }

    df <- left_data$timeseries[[block_str]]

    if (!has_binocular) {
      log_warn(
        "No binocular columns (psl, psr, xpl, xpr, ypl, ypr) found in data.",
        "Use binocular_mode = 'both' when loading data to enable this function."
      )
    }

    left_pupil <- df$psl
    right_pupil <- df$psr
    left_x <- df$xpl
    left_y <- df$ypl
    right_x <- df$xpr
    right_y <- df$ypr
  }

  # create one correlation scatter panel per variable, then draw them as a
  # single patchwork row (replacing the base-graphics par(mfrow) layout)
  panels <- list()
  for (var in variables) {
    if (var == "pupil") {
      left_var <- left_pupil
      right_var <- right_pupil
      xlab <- "Left Eye Pupil Size"
      ylab <- "Right Eye Pupil Size"
    } else if (var == "x") {
      left_var <- left_x
      right_var <- right_x
      xlab <- "Left Eye X-Coordinate"
      ylab <- "Right Eye X-Coordinate"
    } else if (var == "y") {
      left_var <- left_y
      right_var <- right_y
      xlab <- "Left Eye Y-Coordinate"
      ylab <- "Right Eye Y-Coordinate"
    } else {
      log_warn("Unknown variable '{var}', skipping")
      next
    }

    # remove NA values for correlation calculation
    valid_data <- !is.na(left_var) & !is.na(right_var)
    if (sum(valid_data) == 0) {
      log_warn("No valid data for {var} correlation")
      next
    }

    left_clean <- left_var[valid_data]
    right_clean <- right_var[valid_data]

    cor_value <- cor(left_clean, right_clean, use = "complete.obs")
    lim <- range(c(left_clean, right_clean), na.rm = TRUE)
    df_lr <- data.frame(.l = left_clean, .r = right_clean)

    panel <- tryCatch(
      rb_quiet(reaborn::scatterplot(
        data = df_lr,
        x = ".l",
        y = ".r",
        color = grDevices::adjustcolor("blue", alpha.f = 0.6)
      )) +
        ggplot2::geom_abline(
          slope = 1,
          intercept = 0,
          colour = "red",
          linewidth = 0.8,
          linetype = "dashed"
        ) +
        ggplot2::coord_cartesian(xlim = lim, ylim = lim) +
        ggplot2::labs(
          title = sprintf("r = %.3f", cor_value),
          x = xlab,
          y = ylab
        ) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)),
      error = function(e) {
        log_warn("Error creating correlation plot for {var}: {e$message}")
        NULL
      }
    )

    if (!is.null(panel)) {
      panels[[length(panels) + 1]] <- panel
    }
  }

  if (length(panels) > 0) {
    suppressMessages(suppressWarnings(rb_print(panels, title = main)))
  }

  log_success(
    "Created binocular correlation plots for block {block}",
    verbose = verbose
  )
}
