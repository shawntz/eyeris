#' Internal `reaborn`-based plotting helpers
#'
#' `eyeris` renders its diagnostic figures with `reaborn` (an R port of the
#' 'Python' 'seaborn' library that builds on 'ggplot2'). Because every
#' `reaborn` plot is a `ggplot` object, these helpers construct the individual
#' panels and hand back `ggplot` objects that the exported plotting functions
#' [print()] to the active graphics device. Printing keeps the existing
#' device-capture flow used by the interactive HTML reports intact
#' (`png()`/`jpeg()` -> draw -> `dev.off()`), where the final page drawn on a
#' static-filename device is the one written to disk.
#'
#' @keywords internal
#' @name eyeris-reaborn
NULL

#' Muffle `reaborn`'s benign duplicate-colour aesthetic warning
#'
#' `reaborn` (like `seaborn`) accepts a fixed single-series `color`, which
#' 'ggplot2' reports as a duplicated `colour` aesthetic. The warning is
#' harmless (the colour is applied correctly), so it is muffled to keep the
#' console and report generation quiet.
#'
#' @param expr An expression that builds a `reaborn`/`ggplot` object
#'
#' @return The value of `expr`
#'
#' @keywords internal
rb_quiet <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("Duplicated aesthetics", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Build an informative empty panel
#'
#' Returns a blank `ggplot` panel with a centered message, used in place of a
#' real plot when there are too few valid samples to draw one.
#'
#' @param message The message to display in the middle of the panel
#' @param title Optional panel title
#' @param msg_color Colour of the message text (default `"red"`)
#' @param xlab,ylab Axis labels (default empty)
#'
#' @return A `ggplot` object
#'
#' @keywords internal
rb_blank_panel <- function(
  message,
  title = NULL,
  msg_color = "red",
  xlab = "",
  ylab = ""
) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = message,
      colour = msg_color,
      size = 3.5
    ) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Collapse missing-sample positions into contiguous gap regions
#'
#' Rather than drawing one vertical rule per `NA` sample (which can be many
#' thousands of overlapping lines on a full-resolution time series), the
#' missing segments are collapsed into contiguous runs so they can be shaded as
#' a handful of rectangles.
#'
#' @param x Numeric x positions
#' @param y Numeric y values (gaps are where `y` is `NA`)
#'
#' @return A data frame with `xmin`/`xmax` columns (one row per gap run), or
#' `NULL` when there are no gaps
#'
#' @keywords internal
rb_na_gaps <- function(x, y) {
  na <- is.na(y)
  if (!any(na) || all(na)) {
    return(NULL)
  }
  runs <- rle(na)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  keep <- runs$values
  # pad by half a sample so single-sample gaps remain visible
  dx <- suppressWarnings(stats::median(diff(x), na.rm = TRUE))
  if (!is.finite(dx) || dx <= 0) {
    dx <- 0
  }
  data.frame(xmin = x[starts[keep]] - dx / 2, xmax = x[ends[keep]] + dx / 2)
}

#' Build a single-series pupil time series panel
#'
#' Draws `y` against `x` as a `reaborn` line plot in `color`, shading any
#' missing-sample gaps, and applies the supplied titles/labels.
#'
#' @param x Numeric x positions (e.g. time)
#' @param y Numeric y values (e.g. pupil size); `NA` marks missing samples
#' @param color Line colour
#' @param title Panel title
#' @param xlab,ylab Axis labels
#'
#' @return A `ggplot` object
#'
#' @keywords internal
rb_timeseries_panel <- function(
  x,
  y,
  color = "#377EB8",
  title = NULL,
  xlab = "time (ms)",
  ylab = "pupil size"
) {
  if (length(y) == 0 || all(is.na(y))) {
    return(rb_blank_panel(
      "No finite data to plot",
      title = title,
      xlab = xlab,
      ylab = ylab
    ))
  }

  gaps <- rb_na_gaps(x, y)
  fin <- is.finite(y)
  df <- data.frame(.x = x[fin], .y = y[fin])

  # estimator = NULL draws the raw samples as a plain line (no mean
  # aggregation / bootstrap CI), which is both correct for a raw pupil signal
  # and dramatically faster on long, full-resolution time series
  p <- rb_quiet(reaborn::lineplot(
    data = df,
    x = ".x",
    y = ".y",
    color = color,
    estimator = NULL
  ))

  if (!is.null(gaps)) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = gaps$xmin,
        xmax = gaps$xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = "black",
        alpha = 0.08
      )
  }

  p +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

#' Build a pupil-size distribution histogram
#'
#' @param data Numeric vector of pupil samples
#' @param color Fill colour for the bars
#' @param title Panel title
#' @param xlab X-axis label
#' @param backuplab Fallback x-axis label when `xlab` is `NULL`
#'
#' @return A `ggplot` object
#'
#' @keywords internal
rb_histogram <- function(data, color, title, xlab, backuplab = NULL) {
  new_xlab <- if (!is.null(xlab)) {
    xlab
  } else if (!is.null(backuplab)) {
    backuplab
  } else {
    "pupil size"
  }

  finite_data <- data[is.finite(data)]

  if (length(finite_data) < 2) {
    return(rb_blank_panel(
      "Not enough data\nto plot distribution",
      title = title,
      xlab = new_xlab,
      ylab = "frequency (count)"
    ))
  }

  df <- data.frame(.v = finite_data)

  rb_quiet(reaborn::histplot(data = df, x = ".v", color = color)) +
    ggplot2::labs(title = title, x = new_xlab, y = "frequency (count)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

#' Print one or more `ggplot` panels to the active device
#'
#' A single `ggplot` is printed as-is; a list of panels is combined into a
#' single row with `patchwork` and printed, optionally with a bold, centered
#' overall title.
#'
#' @param plots A `ggplot` object, or a list of `ggplot` objects
#' @param title Optional overall title (used only for a list of panels)
#' @param nrow Number of rows when combining a list (default 1)
#'
#' @return Invisibly `NULL`; called for the side effect of drawing
#'
#' @keywords internal
rb_print <- function(plots, title = NULL, nrow = 1) {
  if (is.null(plots)) {
    return(invisible(NULL))
  }

  if (inherits(plots, "gg") || inherits(plots, "ggplot")) {
    print(plots)
    return(invisible(NULL))
  }

  # drop any NULL/non-ggplot entries (e.g. panels skipped or produced by a
  # mocked drawing primitive during testing) before combining
  plots <- Filter(function(p) inherits(p, "gg") || inherits(p, "ggplot"), plots)
  if (length(plots) == 0) {
    return(invisible(NULL))
  }

  combined <- patchwork::wrap_plots(plots, nrow = nrow)

  if (!is.null(title) && nzchar(title)) {
    combined <- combined +
      patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            hjust = 0.5,
            size = 14
          )
        )
      )
  }

  print(combined)
  invisible(NULL)
}
