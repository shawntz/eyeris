#' Access example EyeLink .asc demo dataset file provided by the eyeris package.
#'
#' @description Returns the file path to the demo `.asc` EyeLink pupil data file
#' included in the `eyeris` package.
#'
#' @return A character string giving the full file path to the demo `.asc`
#' EyeLink pupil data file
#'
#' @examples
#' path_to_demo_dataset <- eyelink_asc_demo_dataset()
#' print(path_to_demo_dataset)
#'
#' @export
eyelink_asc_demo_dataset <- function() {
  system.file("extdata", "memory.asc", package = "eyeris")
}

#' Default color palette for eyeris plotting functions
#'
#' @description A custom color palette designed for visualizing pupil data
#' preprocessing steps. This palette is based on the RColorBrewer Set1 palette
#' and provides distinct, visually appealing colors for different preprocessing
#' stages.
#'
#' @details The palette includes 7 colors optimized for:
#' \itemize{
#'   \item High contrast and visibility
#'   \item Colorblind-friendly design
#'   \item Consistent visual hierarchy across preprocessing steps
#'   \item Professional appearance in reports and publications
#' }
#'
#' Colors are designed to work well with both light and dark backgrounds
#' and maintain readability when overlaid in time series plots.
#'
#' @return A character vector of 7 hex color codes representing the default
#'   eyeris color palette
#'
#' @examples
#' # get the default color palette
#' colors <- eyeris_color_palette()
#' print(colors)
#'
#' # use in a plot
#' plot(1:7, 1:7, col = colors, pch = 19, cex = 3)
#'
#' @export
eyeris_color_palette <- function() {
  c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#F781BF",
    "#A65628"
  )
}
