#' Downsample time series data with anti-aliasing filtering
#'
#' This function downsamples pupillometry data by applying an anti-aliasing
#' filter before decimation. Unlike binning, downsampling preserves the
#' original temporal dynamics without averaging within bins.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Provide parameters via
#' `downsample = list(...)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' Downsampling reduces the sampling frequency by decimating data points.
#' The function automatically designs an anti-aliasing filter using the
#' `lpfilt()` function with carefully chosen parameters:
#'
#' - `ws` (stopband frequency) = Fs_new / 2 (Nyquist freq of new sampling rate)
#' - `wp` (passband frequency) = ws - max(5, Fs_nq * 0.2)
#' - An error is raised if `wp < 4` to prevent loss of pupillary responses
#'
#' The resulting time points will be: 0, 1/X, 2/X, 3/X, ..., etc. where X is
#' the new sampling frequency.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load_asc()].
#' @param target_fs The target sampling frequency in Hz after downsampling.
#' @param plot_freqz Boolean flag for displaying filter frequency response.
#'
#' @return An `eyeris` object with downsampled data and updated sampling rate.
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline.
#' [eyeris::bin()] for binning functionality.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # downsample pupil data recorded at 1000 Hz to 100 Hz
#' demo_data |>
#'   eyeris::glassbox(downsample = list(target_fs = 100)) |>
#'   plot(seed = 0)
#'
#' @export
downsample <- function(eyeris, target_fs, plot_freqz = FALSE) {
  current_fs <- eyeris$info$sample.rate
  decimation_factor <- current_fs / target_fs

  if (decimation_factor < 1) {
    cli::cli_abort(
      paste(
        "Target sampling frequency (", target_fs, " Hz) must be less than",
        "current sampling frequency (", current_fs, " Hz)"
      )
    )
  }

  if (decimation_factor != round(decimation_factor)) {
    cli::cli_abort(
      paste(
        "Decimation factor must be an integer. Current: ", decimation_factor,
        ". Consider using a different target_fs."
      )
    )
  }

  # filter parameters
  fs_nq <- target_fs / 2  # Nyquist freq of new sampling rate
  ws <- fs_nq  # stopband freq

  # passband freq with safety margin
  wt <- max(5, fs_nq * 0.2)
  wp <- ws - wt

  if (wp < 4) {
    cli::cli_abort(
      paste(
        "Passband frequency (", round(wp, 2), " Hz) is too low.",
        "This would likely cause loss of actual pupillary responses.",
        "Consider using a higher target_fs or the binning function instead."
      )
    )
  }

  eyeris <- eyeris |>
    pipeline_handler(
      downsample_pupil,
      "downsample",
      target_fs,
      decimation_factor,
      wp,
      ws,
      current_fs,
      plot_freqz
    )

  eyeris$info$decimated.sample.rate <- target_fs

  return(eyeris)
}

downsample_pupil <- function(x, prev_op, target_fs, decimation_factor, wp, ws,
                             current_fs, plot_freqz) {
  if (any(is.na(x[[prev_op]]))) {
    cli::cli_abort("NAs detected in pupil data. Need to interpolate first.")
  } else {
    prev_pupil <- x[[prev_op]]
  }

  # design anti-aliasing filter
  fs_nq <- current_fs / 2
  foo <- gsignal::buttord(wp / fs_nq, ws / fs_nq, 1, 35)  # rp = 1, rs = 35
  filt <- gsignal::butter(foo, output = "Sos")

  # plot frequency response if requested
  if (plot_freqz) {
    par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
    freq_response <- gsignal::freqz(filt, fs = current_fs)
    xlim_sel <- freq_response$w <= min((ws + 10), fs_nq)
    gsignal::freqz_plot(
      freq_response$w[xlim_sel],
      freq_response$h[xlim_sel]
    )
    subtitle <- paste0(
      "*freq response for anti-aliasing filter* - ",
      "passband (", wp, "Hz), stopband (", ws, "Hz), ",
      "target_fs (", target_fs, "Hz)\n"
    )

    # calculate cex
    plot_width <- par("pin")[1]
    scaling_factor <- 7
    cex_val <- plot_width / scaling_factor
    graphics::mtext(
      side = 2, line = 2, at = 0, adj = 0.95,
      cex = cex_val, subtitle
    )
  }

  # apply anti-aliasing filter
  filtered_data <- gsignal::filtfilt(filt, prev_pupil)

  # decimate (downsample) the data
  # use every nth sample where n is the decimation factor
  indices <- seq(1, length(filtered_data), by = decimation_factor)
  downsampled_data <- filtered_data[indices]

  downsampled_df <- data.frame(stringsAsFactors = FALSE)
  downsampled_df[[paste0(prev_op, "_downsample")]] <- downsampled_data

  # downsample timestamps and other columns
  for (col in names(x)) {
    if (col != prev_op && is.numeric(x[[col]])) {
      downsampled_df[[col]] <- x[[col]][indices]
    }
  }

  downsampled_df
}
