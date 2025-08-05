#' Create interactive epoch gallery report
#'
#' Generates an interactive HTML gallery report for epoch data with lightbox
#' functionality.
#'
#' @param eyeris An `eyeris` object containing preprocessing results
#' @param epochs Vector of epoch plot file paths or path to zip file
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

  report_dir <- file.path(dirname(rmd_f), "source")
  www_source <- system.file("www", package = "eyeris")

  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE)
  }

  file.copy(www_source, report_dir, recursive = TRUE)

  # copy zip file to report structure if it's a zip file path
  if (length(epochs) == 1 && grepl("\\.zip$", epochs) && file.exists(epochs)) {
    zip_basename <- basename(epochs)

    # create figures directory structure in report
    figures_dir <- file.path(report_dir, "figures")

    # extract run number from zip filename (e.g., "run-01.zip")
    run_match <- regmatches(zip_basename, regexpr("run-[0-9]+", zip_basename))
    if (length(run_match) > 0) {
      run_dir <- file.path(figures_dir, run_match)
      epoch_dir <- file.path(run_dir, epoch_name)

      if (!dir.exists(epoch_dir)) {
        dir.create(epoch_dir, recursive = TRUE)
      }

      # copy the zip file to the expected location with the simplified name
      zip_dest <- file.path(epoch_dir, zip_basename)

      # only copy if source and destination are different
      if (
        normalizePath(epochs, mustWork = FALSE) !=
          normalizePath(zip_dest, mustWork = FALSE)
      ) {
        file.copy(epochs, zip_dest, overwrite = TRUE)
      }
    }
  }

  report_date <- format(Sys.time(), "%B %d, %Y | %H:%M:%OS3")
  package_version <- as.character(utils::packageVersion("eyeris"))

  html_deps <- paste0(
    "<link rel='stylesheet' href='./source/www/css/bootstrap.min.css' ",
    "onerror=\"this.onerror=null;this.href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css';\" />\n",

    "<link rel='stylesheet' href='./source/www/css/lightbox.min.css' ",
    "onerror=\"this.onerror=null;this.href='https://cdn.jsdelivr.net/npm/lightbox2/dist/css/lightbox.min.css';\" />\n",

    "<script src='./source/www/js/lightbox.min.js' ",
    "onerror=\"this.onerror=null;this.src='https://cdn.jsdelivr.net/npm/lightbox2/dist/js/lightbox.min.js';\"></script>\n",

    "<script src='./source/www/js/zip.min.js' ",
    "onerror=\"this.onerror=null;this.src='https://unpkg.com/@zip.js/zip.js/dist/zip.min.js';\"></script>\n",

    "<script>document.addEventListener('DOMContentLoaded', function() {",
    "lightbox.option({'imageFadeDuration' : 0, 'resizeDuration': 25, 'wrapAround': false});",
    "});</script>\n"
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
    if (!is.null(params$eye_suffix)) {
      paste0(" - Eye: ", params$eye_suffix, "\n")
    } else {
      ""
    },
    " - BIDS Directory: ",
    out,
    "\n",
    " - Source `.asc` file: ",
    eyeris$file,
    "\n",
    " - [`eyeris` version](https://github.com/shawntz/eyeris): ",
    package_version,
    "\n",
    html_deps,
    "\n## Preprocessed Data Preview\n\n",
    "\n## ",
    epoch_name,
    "\n\n",
    epoch_lightbox_html,
    "\n",
    "\n\n---\n\n### Citation\n\n",
    "```{r citation, echo=FALSE, comment=NA}\n",
    "citation('eyeris')\n",
    "```\n\n",
    "\n\n---\n\n## Session Information\n\n",
    "```{r session-info, echo=FALSE, comment=NA}\n",
    "sessionInfo()\n",
    "```\n\n\n\n\n\n"
  )

  writeLines(content, con = rmd_f)

  rmarkdown::render(rmd_f, output_format = "html_document")

  unlink(rmd_f)
}

#' Print lightbox image HTML for zip-based gallery
#'
#' Generates HTML code for lightbox image gallery functionality that loads
#' images from zip files using zip.js.
#'
#' @param zip_path Path to the zip file containing images (can be absolute or relative)
#' @param image_filenames Vector of image filenames within the zip
#' @param verbose Logical. Whether to print verbose output (default TRUE).
#'
#' @return A character string containing HTML code for the lightbox gallery
#'
#' @keywords internal
print_lightbox_img_html <- function(
  zip_path,
  image_filenames = NULL,
  verbose = TRUE
) {
  # use legacy mode if zip_path is actually a vector of individual image paths
  if (length(zip_path) > 1 || !grepl("\\.zip$", zip_path)) {
    return(print_lightbox_img_html_legacy(zip_path))
  }

  # check if zip file exists and convert to data URL for embedding
  is_absolute <- grepl("^(/|[A-Za-z]:)", zip_path)

  if (is_absolute && file.exists(zip_path)) {
    full_zip_path <- zip_path
    log_info("Using absolute zip file path: {zip_path}", verbose = verbose)

    # create relative path for HTML display
    if (grepl("source/figures", zip_path)) {
      html_zip_path <- sub(".*?(source/figures/.*)", "\\1", zip_path)
    } else {
      html_zip_path <- basename(zip_path)
    }
  } else {
    # try multiple possible paths to find the zip file
    possible_paths <- c(
      zip_path, # exact path as provided
      file.path(getwd(), zip_path), # relative to current working directory
      file.path(dirname(getwd()), zip_path), # relative to parent directory
      normalizePath(zip_path, mustWork = FALSE), # normalized path
      file.path(".", zip_path) # relative to current location
    )

    full_zip_path <- NULL
    for (path in possible_paths) {
      if (file.exists(path)) {
        full_zip_path <- path
        log_info("Found zip file at: {path}", verbose = verbose)
        break
      }
    }

    if (is.null(full_zip_path)) {
      log_warn(
        "Zip file not found. Tried paths: {paste(possible_paths, collapse = ', ')}",
        verbose = TRUE
      )
    }

    # then use original path for HTML display
    html_zip_path <- zip_path
  }

  if (!exists("html_zip_path")) {
    html_zip_path <- zip_path
  }

  # generate unique gallery ID based on zip path
  gallery_id <- gsub("[^a-zA-Z0-9]", "_", basename(zip_path))

  # first try to embed zip file as data URL to avoid CORS issues
  zip_data_url <- NULL
  if (!is.null(full_zip_path) && file.exists(full_zip_path)) {
    tryCatch(
      {
        file_size <- file.info(full_zip_path)$size
        # only embed if file size is reasonable (< 1GB)
        if (file_size < 1024 * 1024 * 1024) {
          zip_bytes <- readBin(full_zip_path, "raw", file_size)
          zip_b64 <- base64enc::base64encode(zip_bytes)
          zip_data_url <- paste0("data:application/zip;base64,", zip_b64)
          log_success(
            "Embedded zip file as data URL ({file_size} bytes)",
            verbose = TRUE
          )
        } else {
          log_warn(
            "Zip file too large for data URL embedding ({file_size} bytes, limit: 1GB)",
            verbose = TRUE
          )
        }
      },
      error = function(e) {
        log_warn(
          "Could not embed zip file as data URL: {e$message}",
          verbose = TRUE
        )
      }
    )
  }

  html_out <- paste0(
    "<div id='gallery_",
    gallery_id,
    "' class='zip-gallery'>\n",
    "  <div class='loading'>Loading images...</div>\n",
    "</div>\n",
    "<input type='file' id='zipInput_",
    gallery_id,
    "' style='display: none;' accept='.zip' />\n",
    "<script>\n",
    "(async function() {\n",
    "  const galleryDiv = document.getElementById('gallery_",
    gallery_id,
    "');\n",
    "  const zipPath = '",
    html_zip_path,
    "';\n",
    "  const zipDataURL = ",
    if (is.null(zip_data_url)) "null" else paste0("'", zip_data_url, "'"),
    ";\n",
    "  \n",
    "  async function loadZipFromURL(url) {\n",
    "    // Try multiple methods for loading local files\n",
    "    const methods = [\n",
    "      // Method 1: XMLHttpRequest with overrideMimeType\n",
    "      async () => {\n",
    "        const xhr = new XMLHttpRequest();\n",
    "        xhr.open('GET', url, true);\n",
    "        xhr.responseType = 'blob';\n",
    "        xhr.overrideMimeType('application/zip');\n",
    "        \n",
    "        return new Promise((resolve, reject) => {\n",
    "          xhr.onload = function() {\n",
    "            if (xhr.status === 200 || xhr.status === 0) {\n",
    "              resolve(xhr.response);\n",
    "            } else {\n",
    "              reject(new Error('XHR failed: ' + xhr.status));\n",
    "            }\n",
    "          };\n",
    "          xhr.onerror = () => reject(new Error('XHR network error'));\n",
    "          xhr.send();\n",
    "        });\n",
    "      },\n",
    "      \n",
    "      // Method 2: fetch with no-cors\n",
    "      async () => {\n",
    "        const response = await fetch(url, { mode: 'no-cors' });\n",
    "        return await response.blob();\n",
    "      },\n",
    "      \n",
    "      // Method 3: Regular fetch\n",
    "      async () => {\n",
    "        const response = await fetch(url);\n",
    "        if (!response.ok) throw new Error('Fetch failed');\n",
    "        return await response.blob();\n",
    "      }\n",
    "    ];\n",
    "    \n",
    "    let lastError;\n",
    "    for (const method of methods) {\n",
    "      try {\n",
    "        const result = await method();\n",
    "        if (result && result.size > 0) {\n",
    "          return result;\n",
    "        }\n",
    "      } catch (error) {\n",
    "        lastError = error;\n",
    "        console.warn('Method failed:', error.message);\n",
    "      }\n",
    "    }\n",
    "    \n",
    "    throw lastError || new Error('All methods failed');\n",
    "  }\n",
    "  \n",
    "  async function processZipBlob(blob) {\n",
    "    const zipReader = new zip.ZipReader(new zip.BlobReader(blob));\n",
    "    const entries = await zipReader.getEntries();\n",
    "    \n",
    "    galleryDiv.innerHTML = '';\n",
    "    \n",
    "    for (const entry of entries) {\n",
    "      if (entry.filename.match(/\\.(jpg|jpeg|png|gif)$/i)) {\n",
    "        const imageBlob = await entry.getData(new zip.BlobWriter());\n",
    "        const url = URL.createObjectURL(imageBlob);\n",
    "        \n",
    "        const link = document.createElement('a');\n",
    "        link.href = url;\n",
    "        link.setAttribute('data-lightbox', 'gallery');\n",
    "        link.setAttribute('data-title', entry.filename);\n",
    "        \n",
    "        const img = document.createElement('img');\n",
    "        img.src = url;\n",
    "        img.alt = entry.filename;\n",
    "        img.style.margin = '5px';\n",
    "        img.style.width = '150px';\n",
    "        \n",
    "        link.appendChild(img);\n",
    "        galleryDiv.appendChild(link);\n",
    "      }\n",
    "    }\n",
    "    \n",
    "    await zipReader.close();\n",
    "  }\n",
    "  \n",
    "  try {\n",
    "    let blob;\n",
    "    if (zipDataURL) {\n",
    "      // Use embedded data URL (avoids CORS issues)\n",
    "      console.log('Loading from embedded data URL (', zipDataURL.length, 'characters)');\n",
    "      const response = await fetch(zipDataURL);\n",
    "      if (!response.ok) {\n",
    "        throw new Error('Failed to fetch from data URL: ' + response.status);\n",
    "      }\n",
    "      blob = await response.blob();\n",
    "      console.log('Successfully loaded blob from data URL (', blob.size, 'bytes)');\n",
    "    } else {\n",
    "      // Fallback to loading from file path\n",
    "      console.log('No data URL available, trying file path methods');\n",
    "      blob = await loadZipFromURL(zipPath);\n",
    "    }\n",
    "    await processZipBlob(blob);\n",
    "  } catch (error) {\n",
    "    console.error('Error loading zip gallery:', error);\n",
    "    console.error('Zip path was:', zipPath);\n",
    "    console.error('Data URL available:', zipDataURL ? 'Yes' : 'No');\n",
    "    \n",
    "    // Extract epoch name for display\n",
    "    const epochMatch = zipPath.match(/epoch_([^/]+)/); \n",
    "    const epochName = epochMatch ? epochMatch[1] : 'epoch images';\n",
    "    \n",
    "    galleryDiv.innerHTML = '<div style=\"padding: 20px; border: 2px solid #e74c3c; background-color: #fdf2f2; margin: 15px 0; border-radius: 8px;\"><div style=\"display: flex; align-items: center; margin-bottom: 15px;\"><span style=\"font-size: 24px; margin-right: 10px;\"></span><h3 style=\"margin: 0; color: #c0392b;\">eyeris Epoch Gallery Loader</h3></div><p style=\"margin-bottom: 15px;\"><strong>Unable to automatically load zip file due to browser security restrictions.</strong></p><div style=\"background-color: #fff; padding: 15px; border-radius: 5px; margin-bottom: 15px;\"><p style=\"margin: 0 0 10px 0; font-weight: bold;\">Expected file location:</p><code style=\"background-color: #f8f9fa; padding: 8px; border-radius: 3px; display: block; word-break: break-all;\">' + zipPath + '</code></div><div style=\"text-align: center;\"><button onclick=\"document.getElementById(\\'zipInput_",
    gallery_id,
    "\\').click();\" style=\"padding: 12px 24px; background-color: #2c3e50; color: white; border: none; border-radius: 6px; cursor: pointer; font-size: 16px; font-weight: bold; box-shadow: 0 2px 4px rgba(0,0,0,0.2);\">Load eyeris Epoch Gallery (' + epochName + ')</button></div><p style=\"margin-top: 15px; font-size: 14px; color: #7f8c8d; text-align: center;\">Navigate to and select the zip file shown above to view your epoch images.</p></div>';\n",
    "    \n",
    "    document.getElementById('zipInput_",
    gallery_id,
    "').addEventListener('change', async function(event) {\n",
    "      const file = event.target.files[0];\n",
    "      if (file && (file.type === 'application/zip' || file.name.endsWith('.zip'))) {\n",
    "        try {\n",
    "          galleryDiv.innerHTML = '<div style=\"text-align: center; padding: 20px;\"><p>Loading epoch images from: <strong>' + file.name + '</strong></p><div style=\"display: inline-block; width: 20px; height: 20px; border: 3px solid #f3f3f3; border-top: 3px solid #3498db; border-radius: 50%; animation: spin 1s linear infinite;\"></div></div><style>@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }</style>';\n",
    "          await processZipBlob(file);\n",
    "        } catch (err) {\n",
    "          galleryDiv.innerHTML = '<div style=\"padding: 15px; border: 2px solid #e74c3c; background-color: #fdf2f2; margin: 10px 0; border-radius: 5px;\"><p style=\"color: #c0392b; font-weight: bold;\">Error processing zip file:</p><p>' + err.message + '</p></div>';\n",
    "        }\n",
    "      } else {\n",
    "        galleryDiv.innerHTML = '<div style=\"padding: 15px; border: 2px solid #f39c12; background-color: #fdf6e3; margin: 10px 0; border-radius: 5px;\"><p style=\"color: #d68910; font-weight: bold;\">Please select a valid ZIP file</p></div>';\n",
    "      }\n",
    "    });\n",
    "  }\n",
    "})();\n",
    "</script>\n"
  )

  html_out
}

#' Print lightbox image HTML (legacy)
#'
#' Generates HTML code for lightbox image gallery functionality using
#' individual image files (legacy behavior).
#'
#' @param images Vector of image file paths
#'
#' @return A character string containing HTML code for the lightbox gallery
#'
#' @keywords internal
print_lightbox_img_html_legacy <- function(images) {
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
