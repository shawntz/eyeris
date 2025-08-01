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
zip_and_cleanup_source_figures <- function(report_path, eye_suffix = NULL, verbose = FALSE) {
  figures_dir <- file.path(report_path, "source", "figures")

  if (!dir.exists(figures_dir)) {
    if (verbose) {
      cli::cli_alert_warning("[WARN] Source figures directory not found: %s", figures_dir)
    }
    return(NULL)
  }

  # find all run dirs (run-01, run-02, etc.)
  run_dirs <- list.dirs(figures_dir, full.names = TRUE, recursive = FALSE)
  run_dirs <- run_dirs[grepl("run-\\d+$", basename(run_dirs))]

  if (length(run_dirs) == 0) {
    if (verbose) {
      cli::cli_alert_info("[INFO] No run directories found in: %s", figures_dir)
    }
    return(NULL)
  }

  created_zips <- c()

  for (run_dir in run_dirs) {
    run_name <- basename(run_dir)

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
      if (verbose) {
        cli::cli_alert_info("[INFO] No image files found in: %s", run_dir)
      }
      next
    }

    zip_filename <- paste0(run_name, "_figures")
    if (!is.null(eye_suffix)) {
      zip_filename <- paste0(zip_filename, "_", eye_suffix)
    }
    zip_filename <- paste0(zip_filename, ".zip")
    zip_path <- file.path(run_dir, zip_filename)

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
            # delete individual image files after successful zip creation
            for (file in image_files) {
              if (file.exists(file)) {
                unlink(file)
              }
            }

            empty_dirs <- list.dirs(run_dir, full.names = TRUE, recursive = TRUE)
            empty_dirs <- empty_dirs[empty_dirs != run_dir] # don't remove the run dir itself

            for (dir in rev(empty_dirs)) {
              # remove in reverse order (deepest first)
              if (length(list.files(dir, all.files = TRUE, no.. = TRUE)) == 0) {
                unlink(dir, recursive = TRUE)
              }
            }

            created_zips <- c(created_zips, file.path(run_dir, zip_filename))

            if (verbose) {
              cli::cli_alert_success(
                sprintf(
                  "[OKAY] Created %s with %d images, removed individual files",
                  zip_filename,
                  length(relative_files)
                )
              )
            }
          }
        }

        setwd(current_dir)
      },
      error = function(e) {
        tryCatch(
          setwd(current_dir), # finally return to original directory
          error = function(setwd_error) {
            if (verbose) {
              cli::cli_alert_warning(
                sprintf("[WARN] Failed to return to original directory: %s", setwd_error$message)
              )
            }
          }
        )
        if (verbose) {
          cli::cli_alert_warning(
            sprintf("[WARN] Failed to create zip for %s: %s", run_name, e$message)
          )
        }
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
cleanup_source_figures_post_render <- function(report_path, eye_suffix = NULL, verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("[INFO] Starting post-render cleanup of source figure files...")
  }

  zip_files <- zip_and_cleanup_source_figures(
    report_path = report_path,
    eye_suffix = eye_suffix,
    verbose = verbose
  )

  if (!is.null(zip_files) && length(zip_files) > 0) {
    if (verbose) {
      cli::cli_alert_success(
        sprintf("[OKAY] Post-render cleanup complete. Created %d zip files.", length(zip_files))
      )
    }
  } else {
    if (verbose) {
      cli::cli_alert_info("[INFO] No figure files found to cleanup.")
    }
  }

  invisible(zip_files)
}
