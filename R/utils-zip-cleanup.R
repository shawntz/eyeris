#' Zip and cleanup source figure files
#'
#' Creates zip files for all PNG and JPG files in each run directory under
#' source/figures/, then deletes the individual image files. This reduces
#' file count while preserving all figure data in compressed format.
#'
#' @param report_path Path to the report directory containing source/figures/
#' @param eye_suffix Optional eye suffix for binocular data
#' @param verbose Whether to print verbose output
#'
#' @return List of created zip file paths
#'
#' @keywords internal
zip_and_cleanup_source_figures <- function(
  report_path,
  eye_suffix = NULL,
  verbose = FALSE
) {
  figures_dir <- file.path(report_path, "source", "figures")

  if (!dir.exists(figures_dir)) {
    log_warn("Source figures directory not found: {figures_dir}", verbose = verbose)
    return(NULL)
  }

  # find all run dirs (run-01, run-02, etc.)
  run_dirs <- list.dirs(figures_dir, full.names = TRUE, recursive = FALSE)
  run_dirs <- run_dirs[grepl("run-\\d+$", basename(run_dirs))]

  if (length(run_dirs) == 0) {
    log_info("No run directories found in: {figures_dir}", verbose = verbose)
    return(NULL)
  }

  created_zips <- c()

  for (run_dir in run_dirs) {
    run_name <- basename(run_dir)

    # remove existing zip files if they exist (for reprocessing)
    zip_filename <- run_name
    if (!is.null(eye_suffix)) {
      zip_filename <- paste0(zip_filename, "_", eye_suffix)
    }
    zip_filename <- paste0(zip_filename, ".zip")
    existing_zip_path <- file.path(figures_dir, zip_filename)
    if (file.exists(existing_zip_path)) {
      unlink(existing_zip_path)
      log_info("Removed existing zip file: {zip_filename}", verbose = verbose)
    }

    image_files <- list.files(
      run_dir,
      pattern = "\\.(png|jpg|jpeg)$",
      ignore.case = TRUE,
      full.names = TRUE,
      recursive = FALSE
    )

    # filter by eye_suffix if provided
    if (!is.null(eye_suffix)) {
      image_files <- image_files[grepl(eye_suffix, image_files)]
    }

    if (length(image_files) == 0) {
      log_info("No image files found in: {run_dir}", verbose = verbose)
      next
    }

    zip_filename <- run_name
    if (!is.null(eye_suffix)) {
      zip_filename <- paste0(zip_filename, "_", eye_suffix)
    }
    zip_filename <- paste0(zip_filename, ".zip")
    zip_path <- file.path(figures_dir, zip_filename)

    tryCatch(
      {
        # change to run dir to create relative paths in zip
        current_dir <- getwd()
        setwd(run_dir)

        relative_files <- c()
        for (file in image_files) {
          rel_path <- gsub(paste0("^", run_dir, "/"), "", file)
          if (file.exists(rel_path)) {
            relative_files <- c(relative_files, rel_path)
          }
        }

        if (length(relative_files) > 0) {
          utils::zip(zip_filename, files = relative_files, flags = "-r9X")

          if (file.exists(zip_filename)) {
            # move zip file to parent figures directory
            final_zip_path <- file.path(figures_dir, zip_filename)
            file.rename(zip_filename, final_zip_path)
            created_zips <- c(created_zips, final_zip_path)

            log_success(
              "Created {zip_filename} with {length(relative_files)} images",
              verbose = verbose
            )
          }
        }

        setwd(current_dir)
        # remove the entire run directory after successful zip creation
        if (file.exists(final_zip_path)) {
          unlink(run_dir, recursive = TRUE)
          log_success("Removed run directory: {run_name}", verbose = verbose)
        }
      },
      error = function(e) {
        tryCatch(
          setwd(current_dir), # finally return to original directory
          error = function(setwd_error) {
            log_warn(
              "Failed to return to original directory: {setwd_error$message}",
              verbose = verbose
            )
          }
        )
        log_warn("Failed to create zip for {run_name}: {e$message}", verbose = verbose)
      }
    )
  }

  return(created_zips)
}

#' Clean up source figures after report generation
#'
#' Wrapper function to zip and cleanup source figure files after the main
#' HTML report has been generated and the R Markdown source has been cleaned up.
#'
#' @param report_path Path to the report directory
#' @param eye_suffix Optional eye suffix for binocular data
#' @param verbose Whether to print verbose output
#'
#' @return Invisibly returns list of created zip files
#'
#' @keywords internal
cleanup_source_figures_post_render <- function(
  report_path,
  eye_suffix = NULL,
  verbose = FALSE
) {
  log_info("Starting post-render cleanup of source figure files...", verbose = verbose)

  zip_files <- zip_and_cleanup_source_figures(
    report_path = report_path,
    eye_suffix = eye_suffix,
    verbose = verbose
  )

  if (!is.null(zip_files) && length(zip_files) > 0) {
    log_success(
      "Post-render cleanup complete. Created {length(zip_files)} zip files.",
      verbose = verbose
    )
  } else {
    log_info("No figure files found to cleanup.", verbose = verbose)
  }

  invisible(zip_files)
}
