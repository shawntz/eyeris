#' Create zip file from epoch images
#'
#' Creates a zip file containing epoch images instead of saving individual files.
#' This function collects all the image data in memory and then creates a single
#' zip file, which can be more efficient for the HTML gallery display.
#'
#' @param epochs_to_save List of epoch data to save
#' @param epoch_index Index of the current epoch being processed
#' @param block_name Name of the current block being processed
#' @param run_dir_num Run directory number
#' @param epochs_out Output directory for the epoch files
#' @param pupil_steps Vector of pupil processing steps
#' @param eyeris_object The full `eyeris` object (needed for screen dimensions)
#' @param eye_suffix Optional eye suffix for binocular data
#' @param report_epoch_grouping_var_col Column name for grouping epochs
#' @param verbose Whether to print verbose output
#'
#' @return Path to the created zip file
#'
#' @keywords internal
create_epoch_images_zip <- function(
  epochs_to_save,
  epoch_index,
  block_name,
  run_dir_num,
  epochs_out,
  pupil_steps,
  eyeris_object,
  eye_suffix = NULL,
  report_epoch_grouping_var_col = "matched_event",
  verbose = FALSE
) {
  zip_filename <- sprintf("run-%02d", run_dir_num)
  if (!is.null(eye_suffix)) {
    zip_filename <- paste0(zip_filename, "_", eye_suffix)
  }
  zip_filename <- paste0(zip_filename, ".zip")
  zip_path <- file.path(epochs_out, zip_filename)

  temp_dir <- tempfile("epoch_images_")
  dir.create(temp_dir, recursive = TRUE)

  epoch_groups <- as.vector(unique(epochs_to_save[[epoch_index]][[block_name]][
    report_epoch_grouping_var_col
  ])[[1]])

  created_files <- c()

  tryCatch(
    {
      for (group in epoch_groups) {
        group_df <- epochs_to_save[[epoch_index]][[block_name]]
        group_df <- group_df[
          group_df[[report_epoch_grouping_var_col]] == group,
        ]

        for (pstep in seq_along(pupil_steps)) {
          if (grepl("z", pupil_steps[pstep])) {
            y_units <- "(z)"
          } else {
            y_units <- "(a.u.)"
          }

          colorpal <- eyeris_color_palette()
          colors <- c("black", colorpal)
          y_label <- paste("pupil size", y_units)

          file_out <- file.path(
            temp_dir,
            sprintf("run-%02d_%s_%d", run_dir_num, group, pstep)
          )
          if (!is.null(eye_suffix)) {
            file_out <- paste0(file_out, "_", eye_suffix)
          }
          file_out <- paste0(file_out, ".png")

          png(
            file_out,
            width = 3.25,
            height = 2.5,
            units = "in",
            res = 600,
            pointsize = 6
          )

          y_values <- group_df[[pupil_steps[pstep]]]
          if (any(is.finite(y_values))) {
            plot(
              group_df$timebin,
              y_values,
              type = "l",
              xlab = "time (s)",
              ylab = y_label,
              col = colors[pstep],
              main = paste0(
                group,
                "\n",
                pupil_steps[pstep],
                sprintf(" (Run %d)", run_dir_num)
              )
            )
          } else {
            # handle case where timebin has no finite values
            timebin_range <- range(
              group_df$timebin,
              na.rm = TRUE,
              finite = TRUE
            )
            if (any(!is.finite(timebin_range))) {
              # fallback to default range if no finite values
              timebin_range <- c(0, 1)
            }

            plot(
              NA,
              xlim = timebin_range,
              ylim = c(0, 1),
              type = "n",
              xlab = "time (s)",
              ylab = y_label,
              main = paste0(group, "\n", pupil_steps[pstep], "\nNO DATA")
            )
            log_warn(
              "eyeris: no finite pupillometry data to plot for",
              "current epoch...",
              "plotting empty epoch plot.",
              verbose = verbose
            )
            text(0.5, 0.5, "No valid data", cex = 0.8, col = "red")
          }

          dev.off()
          created_files <- c(created_files, file_out)
        }
      }

      for (group in epoch_groups) {
        group_df <- epochs_to_save[[epoch_index]][[block_name]]
        group_df <- group_df[
          group_df[[report_epoch_grouping_var_col]] == group,
        ]

        if (all(c("eye_x", "eye_y") %in% colnames(group_df))) {
          heatmap_filename <- file.path(
            temp_dir,
            sprintf("run-%02d_%s_gaze_heatmap", run_dir_num, group)
          )
          if (!is.null(eye_suffix)) {
            heatmap_filename <- paste0(heatmap_filename, "_", eye_suffix)
          }
          heatmap_filename <- paste0(heatmap_filename, ".png")

          png(
            heatmap_filename,
            width = 6,
            height = 4,
            units = "in",
            res = 300,
            pointsize = 10
          )

          tryCatch(
            {
              plot_gaze_heatmap(
                eyeris = group_df,
                block = run_dir_num,
                screen_width = eyeris_object$info$screen.x,
                screen_height = eyeris_object$info$screen.y,
                n_bins = 30,
                col_palette = "viridis",
                main = sprintf(
                  "%s\nGaze Heatmap (run-%02d)",
                  group,
                  run_dir_num
                ),
                eye_suffix = eye_suffix
              )
            },
            error = function(e) {
              plot(
                NA,
                xlim = c(0, 1),
                ylim = c(0, 1),
                type = "n",
                xlab = "",
                ylab = "",
                main = paste("Error creating gaze heatmap for epoch", group)
              )
              text(0.5, 0.5, paste("Error:", e$message), cex = 0.8, col = "red")
            }
          )

          dev.off()
          created_files <- c(created_files, heatmap_filename)
        }
      }

      if (length(created_files) > 0) {
        current_dir <- getwd()
        setwd(temp_dir)

        relative_files <- basename(created_files)

        utils::zip(zip_path, files = relative_files, flags = "-r9X")

        setwd(current_dir)

        log_success(
          "Created epoch images zip: {zip_path} ({length(created_files)} images)",
          verbose = verbose
        )
      }
    },
    finally = {
      unlink(temp_dir, recursive = TRUE)
    }
  )

  return(zip_path)
}
