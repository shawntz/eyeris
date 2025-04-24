#' Lowpass filtering of time series data
#'
#' The intended use of this method is for smoothing, although by specifying
#' `wp` and `ws` differently one can achieve highpass or bandpass filtering
#' as well. However, only lowpass filtering should be done on pupillometry data.
#'
#' @note
#' This function is part of the `glassbox()` preprocessing pipeline and is not
#' intended for direct use in most cases. Provide parameters via
#' `lpfilt = list(...)`.
#'
#' Advanced users may call it directly if needed.
#'
#' @details
#' This function is automatically called by `glassbox()` by default. If needed,
#' customize the parameters for `lpfilt` by providing a parameter list. Use
#' `glassbox(lpfilt = FALSE)` to disable this step as needed.
#'
#' Users should prefer using `glassbox()` rather than invoking this function
#' directly unless they have a specific reason to customize the pipeline
#' manually.
#'
#' @param eyeris An object of class `eyeris` derived from [eyeris::load()].
#' @param wp The end of passband frequency in Hz (desired lowpass cutoff).
#' @param ws The start of stopband frequency in Hz (required lowpass cutoff).
#' @param rp Required maximal ripple within passband in dB.
#' @param rs Required minimal attenuation within stopband in dB.
#' @param plot_freqz Boolean flag for displaying filter frequency response.
#'
#' @return An `eyeris` object with a new column in `timeseries`:
#' `pupil_raw_{...}_lpfilt`.
#'
#' @seealso [eyeris::glassbox()] for the recommended way to run this step as
#' part of the full eyeris glassbox preprocessing pipeline.
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' demo_data |>
#'   # set lpfilt to FALSE (instead of a list of params) to skip step
#'   eyeris::glassbox(lpfilt = list(plot_freqz = TRUE)) |>
#'   plot(seed = 0)
#'
#' @export
lpfilt <- function(eyeris, wp = 4, ws = 8,
                   rp = 1, rs = 35, plot_freqz = FALSE) {
  # safely handle user's current options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  fs <- eyeris$info$sample.rate

  eyeris |>
    pipeline_handler(lpfilt_pupil, "lpfilt", wp, ws, rp, rs, fs, plot_freqz)
}

lpfilt_pupil <- function(x, prev_op, wp, ws, rp, rs, fs, plot_freqz) {
  if (any(is.na(x[[prev_op]]))) {
    cli::cli_abort("NAs detected in pupil data. Need to interpolate first.")
  } else {
    prev_pupil <- x[[prev_op]]
  }

  # design a Butterworth filter with minimum order to meet requirements
  fs_nq <- fs / 2
  foo <- gsignal::buttord(wp / fs_nq, ws / fs_nq, rp, rs)
  filt <- gsignal::butter(foo, output = "Sos")

  # plot frequency response of the filter
  if (plot_freqz) {
    par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
    freq_response <- gsignal::freqz(filt, fs = fs)
    xlim_sel <- freq_response$w <= min((ws + 10), fs_nq)
    gsignal::freqz_plot(
      freq_response$w[xlim_sel],
      freq_response$h[xlim_sel]
    )
    subtitle <- paste0(
      "*freq response for the low-pass filter* - ",
      "cutoff (", wp, "Hz), stopping (", ws, "Hz)\n"
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

  # filter twice (forward and backward) to preserve phase information
  gsignal::filtfilt(filt, prev_pupil)
}
