#' Create interactive epoch gallery report
#'
#' Generates an interactive HTML gallery report for epoch data with lightbox
#' functionality.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param epochs Vector of epoch plot file paths
#' @param out Output directory for the report
#' @param epoch_name Name of the epoch for the report
#' @param ... Additional parameters passed from bidsify
#'
#' @return No return value; creates and renders an HTML gallery report
#'
#' @keywords internal
make_gallery <- function(eyeris, epochs, out, epoch_name, ...) {
  params <- list(...)

  epoch_name_corrected <- sub("^epoch_", "epoch-", epoch_name)

  # include eye_suffix in filename if provided
  report_filename <- paste0("sub-", params$sub, "_", epoch_name_corrected)
  if (!is.null(params$eye_suffix)) {
    report_filename <- paste0(report_filename, "_", params$eye_suffix)
  }
  report_filename <- paste0(report_filename, ".Rmd")

  rmd_f <- file.path(out, report_filename)

  report_date <- format(Sys.time(), "%B %d, %Y | %H:%M:%OS3")
  package_version <- as.character(
    utils::packageVersion("eyeris")
  )

  css <- system.file(
    file.path("rmarkdown", "css", "report.css"),
    package = "eyeris"
  )

  sticker_path <- system.file("figures", "sticker.png", package = "eyeris")

  epoch_lightbox_html <- print_lightbox_img_html(epochs)

  title <- "`eyeris` interactive epoch previewer"
  if (!is.null(params$eye_suffix)) {
    title <- paste0(title, " - ", params$eye_suffix)
  }

  content <- paste0(
    "---\n",
    "title: '",
    title,
    "'\n",
    "date: '",
    report_date,
    "'\n",
    "output:\n",
    "  html_document:\n",
    "    df_print: paged\n",
    "    css: '",
    css,
    "'\n",
    "---\n\n",
    "\n\n<img src='",
    sticker_path,
    "' class='top-right-image'>",
    "\n\n---\n\n## Summary\n",
    " - Subject ID: ",
    params$sub,
    "\n",
    " - Session: ",
    params$ses,
    "\n",
    " - Task: ",
    params$task,
    "\n",
    " - Run: ",
    params$run,
    "\n",
    if (!is.null(params$eye_suffix)) paste0(" - Eye: ", params$eye_suffix, "\n") else "",
    " - BIDS Directory: ",
    out,
    "\n",
    " - Source `.asc` file: ",
    eyeris$file,
    "\n",
    " - [`eyeris` version](https://github.com/shawntz/eyeris): ",
    package_version,
    "\n",
    "\n\n<style type='text/css'>\n",
    "@import url('http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/",
    "bootstrap.min.css');\n",
    "@import url('https://cdn.jsdelivr.net/npm/lightbox2/dist/css/",
    "lightbox.min.css');\n</style>\n",
    "<script src='https://cdn.jsdelivr.net/npm/lightbox2/dist/js/",
    "lightbox.min.js'></script>\n<script>document.addEventListener(",
    "'DOMContentLoaded', function() {lightbox.option({'imageFadeDuration' : 0,",
    "'resizeDuration': 25,'wrapAround': false});});</script>\n\n\n",
    "\n## Preprocessed Data Preview\n\n",
    "\n## ",
    epoch_name,
    "\n\n",
    epoch_lightbox_html,
    "\n",
    "\n\n---\n\n### Citation\n\n",
    "```{r citation, echo=FALSE, comment=NA}\n",
    "citation('eyeris')\n",
    "```\n\n"
  )

  writeLines(content, con = rmd_f)

  rmarkdown::render(rmd_f, output_format = "html_document")

  unlink(rmd_f)
}

#' Print lightbox image HTML
#'
#' Generates HTML code for lightbox image gallery functionality.
#'
#' @param images Vector of image file paths
#'
#' @return A character string containing HTML code for the lightbox gallery
#'
#' @keywords internal
print_lightbox_img_html <- function(images) {
  html_out <- ""

  for (i in images) {
    html_out <- paste0(
      html_out,
      '<a href="',
      i,
      '" data-lightbox="gallery" data-title="Image 1">',
      '<img src="',
      i,
      '" alt="Thumbnail 1" style="margin: 5px; width: 150px;"></a>'
    )
  }

  html_out
}
