#' Access example EyeLink .asc demo dataset file provided by the eyeris package.
#'
#' @description Returns the file path to the demo `.asc` EyeLink pupil data file
#' included in the `eyeris` package.
#'
#' @return A character string giving the full file path to the demo `.asc`
#' EyeLink pupil data file.
#'
#' @examples
#' path_to_demo_dataset <- eyelink_asc_demo_dataset()
#' print(path_to_demo_dataset)
#'
#' @export
eyelink_asc_demo_dataset <- function() {
  system.file("extdata", "memory.asc", package = "eyeris")
}
