# eyeris 3.0.0 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This major release delivers multiple performance enhancements and feature improvements, reducing processing time and improving overall efficiency across the pipeline.

## 🚨 **Breaking changes & deprecations**

- **DEPRECATED: `merge_runs` and `merge_epochs` parameters in `bidsify()`**: Both `merge_runs` and `merge_epochs` parameters are now deprecated. All runs and epochs are now saved as separate files following proper BIDS conventions, which is the recommended approach for neuroimaging data standards. Users relying on these features should update their analysis workflows to handle separate run and epoch files. Deprecation warnings will be shown when these parameters are used.

## 🚀 New features

- **NF**: **Zip-based epoch gallery system**: Replaced individual epoch image files with zip-based storage and loading system. The `make_gallery()` function now creates epoch images in zip files instead of individual PNG files, and uses `zip.js` to dynamically load images in the HTML gallery. This reduces file count, improves organization, and provides more efficient loading while maintaining full backward compatibility with existing individual image files. This is particularly beneficial for high-throughput compute environments with limited inode quotas and simplifies file transfers to cloud storage providers like GitHub LFS, by @shawntz in #254.

- **NF**: **Automated source figure cleanup**: Added post-render cleanup functionality that automatically zips all `png` and `jpg` files in each `source/figures/run-xx/` directory after the main HTML report is generated before deleting the individual image files, further reducing file count burden while preserving all figure data in compressed format, creating only one zip file per run per subject instead of hundreds of individual image files, by @shawntz in #255.

- **NF - DuckDB database integration**: Added optional `DuckDB` database functionality to `bidsify()` as an alternative to CSV files for large-scale analyses. When `db_enabled = TRUE`, all `eyeris` data (timeseries, epochs, events, blinks, confounds) are written to a centralized database for efficient querying and analysis. Features include seamless out-of-the-box configuration, user-friendly database functions (`eyeris_db_collect()`, `eyeris_db_connect()`, `eyeris_db_read()`, `eyeris_db_list_tables()`), and `dplyr`-style data access. CSV file generation can be optionally disabled with `csv_enabled = FALSE` for cloud compute environments, by @shawntz in #256.

- **NF**: **Added parallel processing support for DuckDB database operations** to prevent concurrency issues during batch processing. Implemented temporary database creation with automatic merging and cleanup mechanisms. Added environment variable detection for common HPC schedulers (SLURM, PBS, SGE, LSF) and manual `parallel_processing` parameter override. Includes comprehensive file locking, process/job ID logging, and full test coverage, by @shawntz in #262.

 > This enhancement enables seamless parallel compute and batch processing when using `db_enabled = TRUE`. Each parallel job now writes to a unique temporary database, preventing the crashes that occurred when multiple processes attempted concurrent writes to the same DuckDB file.
  >
  > Key features:
  > - **Automatic Detection**: Detects HPC environments (SLURM_JOB_ID, PBS_JOBID, etc.)
  > - **Temporary Databases**: Each job uses PID + timestamp for unique temp database names
  > - **Safe Merging**: File-based locking prevents concurrent access during merge operations
  > - **Rich Logging**: Job ID and process ID included in all parallel processing messages
  > - **Zero Breaking Changes**: All existing functionality preserved
  >
  > Usage examples:
  > ```r
  > # Automatic detection in HPC environments
  > data |> bidsify(db_enabled = TRUE)
  > 
  > # Manual enable for testing/development
  > data |> bidsify(db_enabled = TRUE, parallel_processing = TRUE)
  > 
  > # Environment variable override
  > Sys.setenv(PARALLEL_PROCESSING = "1")
  > data |> bidsify(db_enabled = TRUE)
  > ```

- **NF**: **Large-scale database export functionality**. Added `eyeris_db_to_chunked_files()` and `process_chunked_query()` functions to handle really large `eyerisdb` databases by processing data in configurable chunks (default `1M rows`) with automatic file size limits (default `500MB`) and numbered file splitting (`_01-of-N` pattern). Supports both `CSV` and `Parquet` output formats with memory-efficient streaming processing, by @shawntz in #266.

- **NF**: **Robust parquet export/read for `eyerisdb` mixed schemas**. `eyeris_db_to_parquet()` and `read_eyeris_parquet()` now combine tables/files using schema-aligned binding (by column name; fill missing) via `data.table::rbindlist(use.names = TRUE, fill = TRUE)`. Fixes "numbers of columns of arguments do not match" when different `epochs_*` tables have slightly different columns, by @shawntz in #266.

  > **Key Features:**
  > - **Chunked Processing**: Handles databases of any size without memory issues using configurable chunk sizes
  > - **Automatic File Splitting**: Creates numbered files when size limits exceeded (e.g., `data_01-of-03.csv`)
  > - **Smart Schema Grouping**: Dynamically groups tables by column structure to prevent SQL UNION errors
  > - **Database Safety**: Comprehensive temp table cleanup and contamination prevention
  > - **Comprehensive Documentation**: Detailed vignette with real-world examples and troubleshooting

- **NF**: **Database sharing and distribution functionality**. Added `eyeris_db_split_for_sharing()` and `eyeris_db_reconstruct_from_chunks()` functions to facilitate sharing of large eyeris databases via platforms with file size limits (GitHub, OSF, data repositories). Supports chunking strategies by data type, count, or size limits with epoch label grouping for efficient organization. Includes comprehensive metadata for reliable database reconstruction, by @shawntz in #269.

## 🔧 Major code improvements

- **Simplified `bidsify()` function**: Significantly refactored and streamlined the `pipeline-bidsify.R` file to improve maintainability and reduce code complexity:
  - **Removed merge_runs and merge_epochs logic**: Eliminated ~250+ lines of complex conditional logic for merging runs and epochs, simplifying the codebase while ensuring BIDS compliance
  - **Consolidated baseline data access**: Updated helper functions to prioritize baseline information stored within `epoch_xx$block_1$baseline` structure, providing consistent data access patterns while maintaining backward compatibility with legacy storage locations
  - **Simplified epoch summary generation**: Replaced 257 lines of repetitive `sapply()` calls with a clean 77-line implementation using `unlist()` approach for generating `*_desc-epoch_summary.csv` files, improving code readability and maintainability
  - **Added helper functions**: Created centralized functions (`get_epoch_info()`, `get_baseline_events()`, `get_baseline_type()`, `has_baseline()`) to eliminate code duplication and provide consistent data access across the pipeline

- **ENH**: Added post-render cleanup of figures directories in `pipeline-bidsify.R`. Enhanced `zip_and_cleanup_source_figures` to remove existing zip files before reprocessing, move new zip files to the parent figures directory, and delete run directories after successful zipping. This streamlines figures management and prevents leftover files from previous runs, by @shawntz in #261.

  > Further cleans up number of derived files in `eyeris` BIDS directories (which is especially useful for cloud compute deployments).
  >
  > The new behavior will be:
  > 
  > 1. Create zip files in the `figures/` directory (i.e., one level up from the run directories)
  > 2. Remove existing zip files when reprocessing
  > 3. Remove the entire `run-XX/` directories after successful zip creation
  >
  > As such, the final file structure will be:
  
  ```bash
    sub-01/
    └── ses-enc/
        ├── sub-01.html
        └── source/
            ├── figures/
            │   ├── run-01.zip  # now contains all images from run-01/
            │   ├── run-02.zip  # now contains all images from run-02/
            │   └── run-03.zip  # now contains all images from run-03/
            └── logs/
                ├── run-01_metadata.json
                ├── run-02_metadata.json
                └── run-03_metadata.json
  ```

- **FF**: Handle non-finite values in plotting xlim ranges to prevent `plot.window()` crashes. Added `finite=TRUE` parameter to `range()` calls and fallback logic when no finite values exist in timebin or x_seq data. Resolves "need finite 'xlim' values" error during epoch visualization, by @shawntz in #263.

- **FF**: Standardize column structure before rbind in epoch summaries to prevent column mismatch errors. Different epochs can have varying metadata structures (9 vs 10 fields), causing `rbind()` to fail. Added logic to collect all unique column names, standardize structure with NA values for missing columns, and ensure consistent column ordering before combining data frames, by @shawntz in #264.

- **ENH**: **Improved database summary performance**. `eyeris_db_summary()` now extracts subject/session/task information directly from table names instead of sampling database contents, providing complete coverage of all subjects and much faster execution, by @shawntz in #266.

- **ENH**: **Enhanced temp table safety**. All database export functions now automatically detect, warn about, and exclude temporary tables from processing. Added safe temporary table operations with guaranteed cleanup even on process crashes, by @shawntz in #266.

- **ENH**: **Optimized database export performance and reliability**. `eyeris_db_to_chunked_files()` now uses a hybrid approach: database-level export via DuckDB's `COPY` command for large file size limits (≥500MB) for maximum performance, and chunked processing for smaller limits (<500MB) to ensure proper file size splitting for git-lfs workflows. Additionally, parquet file handling has been improved to eliminate unreliable appending that caused `_chunk_` files, now using a cleaner numbered file approach with 80% size thresholds to prevent append failures, by @shawntz in #268.

- **ENH**: **Enhanced chunked database export with epoch label grouping**. `eyeris_db_to_chunked_files()` now supports `group_by_epoch_label` parameter (default: TRUE) that processes epoch-related data types separately by epoch label, reducing memory footprint and creating label-specific output files. Added helper functions for epoch label extraction and improved table grouping logic for better organization of exported data, by @shawntz in #270.

## 🔧 Minor improvements and fixes

- **BF (#250)**: Updated functions in `pipeline-confounds.R` and `pipeline-epoch.R` to handle cases where 'start_matched_event' is used instead of 'matched_event' in epoched data. Resolved an issue with inconsistent variable naming introduced in #a8df0c0, and fixed time and duration calculations in `epoch_start_end_msg()` to use correct units and sample counts, by @shawntz in #251.

- **RF**: **Added Bootstrap and Lightbox assets** (CSS, JS, fonts, images) to `inst/www` and updated `.Rbuildignore` to exclude them from builds. Refactored make_gallery to use local copies of these dependencies with CDN fallbacks, improving offline support and reliability of gallery reports, by @shawntz in #252.

- **DOC**: Added a comprehensive 'Internal API Reference' vignette documenting all internal functions for advanced users and developers and updated the README to link to the new vignette/included it in the pkgdown docs site configuration, by @shawntz in #257.

- **RF**: Updated documentation and comments across multiple files to improve clarity and align terminology throughout the codebase, especially in function descriptions, parameter names, and return value documentation, by @shawntz in #257.

- **FF**: Updated the `log_message()` function to use `tryCatch` when applying `glue` interpolation, ensuring that errors (e.g., from malformed braces or embedded `JSON`) do not interrupt logging; now, the original message is used if interpolation fails, by @shawntz in #258.

- **CHORE**: Add a GitHub Actions workflow to auto-render vignettes and publish them to the `eyeris` GitHub repo wiki, by @shawntz in #259.

- **RF - post-render cleanup to remove figures directory**: The `cleanup_source_figures_post_render()` function now removes the entire `source/figures` directory after report generation, as images are embedded in the `HTML`. Documentation and comments updated to reflect this change, and unused parameters are noted for compatibility, by @shawntz in #261.

- **RF - increase zip file embed size limit to 1GB**: Raised the maximum allowed zip file size for data `URL` embedding from `10MB` to `1GB` in `print_lightbox_img_html()`. Updated warning message to reflect the new limit, by @shawntz in #261.

- **RF - update report title in `make_report` function**: Changed the report title from 'preprocessing summary report' to 'preprocessing report' for consistency and clarity, by @shawntz in #261.

- **CHORE - update logo image URL**: Changed the `logo` image source in `README` files to use a `GitHub raw URL` for better compatibility, by @shawntz in #261.

- **FF**: Detect grouping column for epoch diagnostic plots. For start/end epochs (e.g., `"PROBE_S {STIM}"` to `"PROBE_E {STIM}"`), the epoched data contains `start_matched_event` instead of `matched_event`. Added automatic detection logic with priority: requested column → `start_matched_event` → `end_matched_event`. Includes informative logging and graceful fallback when no suitable column found, by @shawntz in #265.

- **FF**: Improved handling of mismatched start/end events in `epoch()`. When using start/end event pairs (e.g., `c("PROBE_S {STIM}", "PROBE_E {STIM}")`), the function now automatically matches events by extracting identifiers from event messages instead of failing with "Start and end timestamps must have the same number of rows". Unmatched events are filtered out and the process continues with matched pairs only, with informative logging about the filtering process, by @shawntz in #267.

- **DOC**: Updated package documentation to include newly exported database functions in `_pkgdown.yml`, core function reference table in `README.Rmd`, and internal API vignette. Enhanced documentation coverage for database export and management functionality, by @shawntz in #270.

# eyeris 2.1.1 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This release patches a few bugs that emerged as a result of the `v2.1.0` minor release.

## 🔧 Minor improvements and fixes

- BF (#237): Ensure full raw timeseries `.csv` file is written by `bidsify()` for single-run (monocular) data, including cases with or without epoching and with run number override. Previously, the file was only written for multi-run data by @shawntz in #240.

- FF (#236): Certain `sprintf`-formatted log messages (e.g., those with '%s') for blinks/events writing in `pipeline-bidsify.R` now parse and display correctly in the logger by using the internal `alert()` wrapper by @shawntz in #241.

- FF (#238): Only log `[INFO] Filtered epochs: ...` when data is epoched by @shawntz in #242.

- CHORE (#239): Remove duplicate logging events for "Created gaze heatmap" in `bidsify()` pipeline. The log message now appears only once per run and uses the `[OKAY]` log level for both run-level and epoch-level heatmap creation, improving clarity and consistency in logs, by @shawntz in #243.

- ENH DOC: Add GitHub Actions workflow for automated spellchecking of documentation and code using [r-spellcheck-action](https://github.com/marketplace/actions/r-spellcheck-action). The workflow runs on pushes and pull requests to `dev` and `release/**` branches by @shawntz in #244

- CHORE: Adjust `pkgdown` CI deployment trigger conditions for PRs to be more specific about which branches should trigger the workflow by @shawntz in #245.

- CHORE: Update the GitHub Actions workflow configuration to fix CI triggers for the R CMD build/check action. The changes modify the workflow name and expand branch pattern matching to include release branches with additional path segments, by @shawntz in #246.

- CHORE: Standardize the GitHub Actions workflow configurations for air formatting operations. The changes update workflow names and branch targeting patterns to ensure consistent formatting checks and suggestions across the development workflow, by @shawntz in #247.

- CHORE: Resolve spelling errors throughout the package by correcting typos in documentation, comments, and code, while also creating a WORDLIST file for the spellchecker to recognize domain-specific terms by @shawntz in #248.

# eyeris 2.1.0 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This minor release introduces significant enhancements, new features, and robust improvements focusing on binocular data support, standardized logging, and improved reporting and development workflows, substantially elevating `eyeris'` functionality, robustness, and user experience.

## ✨ New features & enhancements

- **Comprehensive Binocular Recording Support**: `eyeris` now provides full binocular recording support, resolving a critical data loading bug previously encountered with binocular EyeLink recording data files (#216 reported by @anomalosepia). This enhancement includes nesting `eyeris` class lists with `left` and `right` parent lists to explicitly separate left eye (`L`) and right eye (`R`) data. All downstream operations are now designed to treat `L` and/or `R` as separate entities, which ensures no cross-contamination between data from the two eyes during processing. Additionally, the `bidsify()` function now appends `_eye-L` and/or `_eye-R` to all derivatives and output HTML reports for binocular data for clear naming conventions. Pipeline functions such as `deblink()`, `interpolate()`, `lpfilt()`, `detransient()`, `zscore()`, `epoch()`, `downsample()`, `detrend()`, and `bin()` have been updated to handle binocular objects. A new `plot_binocular_correlation()` function has also been added, and vignettes were updated to explain binocular data structures and usage with `glassbox()`; by @shawntz in #228.

- **Standardized Logging and Error Handling (using `cli` package)**: The `eyeris` package has undergone comprehensive refactoring to standardize and enhance the clarity of logging, warning, and error messages by transitioning fully to the `cli` R package. This replaces base R functions like `message()`, `stop()`, and `warning()` with `cli::cli_alert_*`, `cli::cli_abort()`, and `cli::cli_alert_warning()` respectively. Consistent log level tags such as `[INFO]`, `[OKAY]`, `[WARN]`, and `[EXIT]` are now implemented within `cli` alert messages for clearer categorization and improved message clarity and uniformity across various pipelines and functions. These changes have been applied across key components including `run_bidsify`, `detransient_pupil`, `pipeline-epoch.R`, `interpolate_pupil`, `make_epoch_label`, `check_and_create_dir`, `compute_baseline`, `load_asc`, `process_eyeris_data`, `pipeline-glassbox.R`, and `plot.eyeris.R`; by @shawntz in #229.

- **Enhanced HTML Reports for Multi-Run Data:** HTML reports now accurately reflect all detected runs, including their metadata and call stack. This addresses a previous issue where only the latest run was shown, by modifying `make_report()` to detect all `run-xx` folders in `source/figures/` and updating `save_progressive_summary_plots()` to use the folder structure rather than `eyeris$timeseries` names. The metadata section now shows one line per run for the `.asc` file source and a formatted call stack for each run; by @shawntz in #224.

- **Integration of Air R Formatter:** The project's R code formatting has transitioned to `Air`, an R formatter and language server written in Rust, replacing the lintr-based formatting system. This change involved deleting the `.github/workflows/linter.yml` file, introducing a new `.air.toml` configuration file, and adding two new GitHub workflows for format checking and suggestions. Furthermore, comprehensive code reformatting has been applied to all R source files to match Air's standards, ensuring better line breaks, consistent spacing, and improved argument alignment throughout the entire codebase; by @shawntz in #234.

## 🔧 Minor improvements and fixes

- **Improved HTML Report Navigation:** The table of contents depth in HTML reports has been increased from 3 to 6 levels for better navigation by @shawntz in #229.

- **Refined Makefile Targets:** New Makefile targets have been added for CRAN presubmission checks, CRAN submission, GitHub releases, and code formatting with `Air`. This also includes expanded dependency installation and improved output formatting and redirection for cleaner logs by @shawntz in #230.

- **Event Placeholder Fix:** The event placeholder in example code and documentation has been updated from `{type}` to `{startstop}` to resolve a name conflict with an existing `type` column name in the derived `.csv` data files by @shawntz in #232.

## 📚 Documentation & Development Workflow Updates

- **Revamped README:** The `README` has been significantly expanded with detailed feature highlights, a comprehensive function reference table, and new sections outlining the BIDS-like file structure for both monocular and binocular data. It also includes quick links to eyeris tutorials (R CRAN package vignettes), improved example output, and updated section headings for clarity and visual appeal; by @shawntz in #231.

- **Enhanced Pull Request Template:** The GitHub pull request template has been updated to provide a more structured and detailed format for contributors. This update includes structured sections for problem description, key changes, and acknowledgments, providing detailed examples and expanding the breaking changes section into a more comprehensive checklist; by @shawntz in #227.

- **Makefile Maintenance:** Comments and section headers in the Makefile have been updated and clarified for better maintainability by @shawntz in #230.

# eyeris 2.0.0 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This is the largest update yet for `eyeris`, introducing a wealth of new features and addressing numerous small issues to significantly enhance functionality, robustness, and user experience (#215).

## ✨ New features

### Enhanced reporting and visualization:

-   **Progressive preprocessing summary plots** can now be generated and saved, visualizing the effects of each preprocessing step on pupil data and updating report structures to include these visualizations. The `plot.eyeris()` function now includes an `add_progressive_summary` parameter to optionally generate these plots by @shawntz in #212.

-   **Gaze heatmap generation** is added for both runs and epoch groups within `bidsify()`, enabling visualizations of eye coordinate distributions, data quality, and participant attention when eye tracking and screen dimension data are available. A new `plot_gaze_heatmap()` function is introduced for this purpose by @gustxsr and @shawntz in #213.

-   **Interactive HTML reports** now include a floating table of contents, enhancing navigation for longer reports by @shawntz in #182.

-   The `html_report` parameter in `bidsify()` now **defaults to `TRUE`** by @shawntz in #212.

### Core data processing functions:

-   New `bin()` and `downsample()` functions are introduced for **pupil time series data processing**, including anti-aliasing filtering for downsampling and averaging for binning. Both functions are integrated into the `eyeris::glassbox()` pipeline by @shawntz and @mh105 in #204.

-   **Unique identifiers (`text_unique`)** are now added to event messages in `eyeris::load_asc()` to prevent duplicate event merges, and the `merge_events_with_timeseries()` function is updated to utilize these for correct event matching and merging by @shawntz in #181.

-   **Confounds calculation and export** are integrated into the processing pipelines, with `eyeris::summarize_confounds()` now included in `eyeris::glassbox()` and `eyeris::epoch()` pipelines by @shawntz in #182.

### Pipeline robustness and reproducibility:

-   **Tracking of pipeline step provenance** is improved, adding the original function call and parameters for each step via a new `call_info` argument. This metadata is passed to `eyeris` functions to enhance reproducibility and debugging by @shawntz in #209.

-   The `eyelogger()` utility documentation has been **updated with a new section in the README**, detailing its purpose, usage examples, parameters, and generated log files for improved reproducibility and debugging by @shawntz in #214.

## 🔧 Minor improvements and fixes

### Robustness and error handling:

-   **Enhanced plotting robustness** includes `tryCatch` blocks to handle errors and display informative messages in plots, and time series plotting now iterates over all intermediate steps to ensure plots are generated even with missing or incomplete data by @shawntz in #181, #183.

-   **Handling of missing valid samples** in random epoch plotting has been improved in `plot.eyeris()`, adding warning messages and placeholder plots when no valid samples are found by @shawntz in #181, #183.

-   **Stricter validation checks** are added for the `prev_op` argument in the `eyeris::zscore_pupil()` internal function to catch missing, non-existent, or corrupted column names early, improving error handling by @shawntz in #207.

-   **Validation for pupil data** in the `eyeris::lpfilt_pupil()` internal function ensures data is numeric, non-empty, and contains only finite values before filtering, preventing errors related to invalid matrix extents by @shawntz in #210.

-   **Stricter checks for corrupted or empty `latest` pointers and output column names** are added in `eyeris::pipeline_handler()`, improving error handling and transitioning operation calls to use `do.call` for flexible argument passing by @shawntz in #211.

-   The `eyeris::load_asc()` function now **correctly sets the `latest` pointer** as a named list for _multi-block structures_ and as a single value for _single block data_, enhancing multi-block support by @shawntz in #211.

-   Fixes an **edge case** where `mad_val` is `NA` in the `eyeris::detransient_pupil()` internal function (occurring when all pupil data is `NA`), ensuring the original pupil data is returned unchanged and preventing comparison to `zero` when `mad_val` is `NA` by @shawntz in #193.

-   **Baseline handling in `eyeris::epoch()`** is simplified by **_deprecating_** `calc_baseline` and `apply_baseline` in favor of a single `baseline` parameter, also resolving bugs related to baseline computation and event mismatches by @shawntz in #177.

### Pipeline and data logic:

-   The `eyeris::glassbox()` function has been **refactored to process each block** in the time series _individually_ (except `load_asc`), improving modularity and ensuring correct error handling of multi-block data by @shawntz in #189.

-   The calculation of `mean_gaze_distance_from_center_px` now **correctly uses the screen center coordinates (`cx`, `cy`)** instead of defaulting to the origin, ensuring the metric reflects distance from the actual screen center by @shawntz in #199.

-   The `eyeris::bidsify()` function is **refactored to handle cases where no epochs are present**, preventing errors and unnecessary processing for users who want summary reports of the entire pupil time series without prior epoching by @shawntz in #201.

-   The `eyeris::bidsify()` function now **properly allows manual specification of the `run_num`** for single-block data, while still auto-numbering multi-block files for improved naming consistency by @shawntz in #203.

-   The **recalculation of epoched confounding variables** is now performed _when new epochs are created_ by @shawntz in #182.

### Documentation and internal clean-up:

-   **Extensive documentation cleanup** has been performed, including fixing various spelling errors/typos in multiple function documentations by @shawntz in #179, #214.

-   The `pdf_report` parameter is `eyeris::bidsify()` has been **_deprecated_** in favor of `html_report = TRUE`, with associated removal of PDF rendering logic from the `render_report()` internal function by @shawntz in #197.

-   Updates to `_pkgdown.yml`, `README`, `NAMESPACE`, and `R/zzz.R` to support new features and functionality, including exposing the `eyeris_color_palette()` and other global variables by @shawntz in #214.

-   The structure of the `latest` field in mock data for unit tests was fixed to be a named list to ensure tests do not fail due to incorrectly specified data structures by @shawntz in #208.

-   Added `MASS`, `viridis`, and `fields` package dependencies to `Imports` to support new gaze heatmaps functionality by @shawntz in #213.

------------------------------------------------------------------------

# eyeris 1.2.1 "Tree Trunks" ![Tree Trunks](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/tree-trunks.png){width="50"}

## ✨ New features

-   `eyelogger()`: a new utility function to automatically capture and record R console output, errors, and the evaluated `eyeris` command into timestamped log files (`.out`, `.err`, and `.cmd`) for improved reproducibility, record keeping, and debugging by @shawntz in #171

    ### `eyelogger()` usage example

    Logging your `eyeris` commands with `eyelogger()` is as simple as wrapping your command like this:

    ``` r
    eyelogger({
      glassbox(eyelink_asc_demo_dataset(), interactive_preview = FALSE)
    }, log_dir = file.path("~/Documents", "eyeris_logs"))
    ```

## 🐛 Bugs fixed

-   Fixed edge case related to non-finite samples in `bidsify` epoch plotting function by @gustxsr in #166
-   Fixed multi-block epoch bug by extracting data from blocks using their names rather than their indices by @hyang336 in #168
-   Fixed plotting bug (#165) by @shawntz in #169

## 🔧 Other minor improvements and fixes

-   ENH: make plot color scheme more accessible/easier to read by @shawntz in #169
-   FF: missing x-axis labels on histograms in rendered reports by @shawntz in #169
-   NF: add detrend fitted values diagnostic plot to rendered reports by @shawntz in #169

# eyeris 1.2.0 "Tree Trunks" ![Tree Trunks](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/tree-trunks.png){alt="Tree Trunks" width="50"}

## ✨ New features

-   NF: Add vertical lines to plots to indicate where missing data (such as blinks and/or removed artifacts) are located in the time series
-   ENH: Plotting now takes time ranges in seconds directly and does the conversion to row index using tracker Hz on the backend, making plotting in `eyeris` more intuitive. To demonstrate:

```         
plot(eyeris_preproc,
  steps = c(1, 5),
  preview_window = c(0, max(eyeris_preproc$timeseries$block_1$time_secs))
)
```

## 🐛 Bug fixes

-   BF: `NA` slot offset in diagnostic plotting (#161)
-   BF: normalize physical machine time bins and convert to seconds / start at 0 seconds (#162)
-   BF: unit displayed on the x-axis doesn't match the unit listed on the x-axis text label in plots (#162)
-   BF: minor issue where a manually specified block number in `load_asc()` wasn't being translated to the column `block` in the resulting list of time series data frames
-   BF: minor issue where setting `block = NULL` in `load_asc()` didn't actually omit the block column values from the resulting data frames within the returned `eyeris` list object

## 🔧 Minor improvements and fixes

-   RF: update package title to match that of the published bioRxiv preprint
-   RF: Deprecated the `num_previews` parameter in `plot()`.
    -   Please use `preview_n` instead.
-   DOC: manually update citation file to include all authors + bioRxiv preprint DOI (#152)
-   DOC: incorrect URIs in `eyeris` documentation for `load_asc()` function (#160)
-   DOC: standardize default values for `deblink()` standalone [previously `40ms`] vs. in `glassbox()` [now all `50ms`] (#163)
-   DOC: updates to `pkgdown` documentation website:
    -   Matching accent color theme with the `eyeris` hex logo
    -   New nav bar items (buttons/links to access the bioRxiv preprint and socials)
    -   Update funders and contributors list
    -   Add funders disclaimer statement to the footer

# eyeris 1.1.0 "Princess Bubblegum" ![Princess Bubblegum](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/princess-bubblegum.png){alt="Princess Bubblegum" width="25"}

## ✨ New features

-   NF: Simplify `glassbox()` caller with redesigned parameters that enhance continuity across all pipeline steps (#148)

## 📚 Documentation

-   DOC: Improved function documentation across the package and added more usage notes for clarity.
-   DOC: Updated styling of documentation website.

## 🔧 Minor improvements and fixes

-   ENH: Make histograms disabled by default when plotting an `eyeris` object (#156).
-   RF: Deprecated the `confirm` parameter in `glassbox()`.
    -   Please use `interactive_preview` instead.
-   RF: Deprecated the `num_previews` parameter in `glassbox()`.
    -   Please use `preview_n` instead.
-   BF: Random seed assignment was not behaving as expected within the `glassbox()` pipeline.
-   RF: Modify paths to documentation assets to fix broken links at build.

# eyeris 1.0.1 "Ice King" ![Ice King](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/ice-king.png){alt="Ice King" width="50"}

This non-CRAN release patches a small handful of documentation-related chores that have no direct impact on the functionality of `eyeris` for the end user. The minor improvements and fixes contained within this release will soon be bundled with a more substantial feature upgrade when submitted to CRAN to reduce burden on the CRAN reviewers at this time. Stay tuned!

## 🔧 Minor improvements and fixes (#159)

-   CHORE: fix duplicate `LICENSE` file issue in `release/**` branches (#145)
-   CHORE: update funders list on `DESCRIPTION` (#149)
-   CHORE: add DOI badge to `README` (#150)
-   CHORE: fix citation years in `DESCRIPTION` (i.e., put them in parentheses, per request of `CRAN` reviewer) (#151)
-   DOC: update `README` to include `CRAN` install code + option for @latest `dev` branch via download with devtools/GitHub (#153)
-   DOC: fix version titles + urls on changelog webpage (#154)
-   CHORE: change pkgdown docs website deployment rules so that public webpage only updates on pushes to official release branches, and not the `dev` branch (#155)
-   DOC: add `CONTRIBUTING.md` guidelines file for GitHub (#157)
-   DOC: fix `/man/figures/...` image ref issues which is leading to broken links on the `R CRAN read-only` [GitHub mirror repo](https://github.com/cran/eyeris) (#158)

# eyeris 1.0.0 "Ice King" ![Ice King](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/ice-king.png){alt="Ice King" width="50"}

## 🎉 **First CRAN release!** (#144)

This version marks the official launch of the `eyeris` package on CRAN.

## ✨ New features (#125)

-   Added example vignettes to demonstrate core functionality:
    -   Preprocessing pipelines with `glassbox()`
    -   Event-based epoching with `epoch()`
    -   BIDS-style export with `bidsify()`
    -   Custom pipeline extensions using `pipeline_handler()`

## 📚 Documentation (#125)

-   Improved function documentation across the package
-   Added citation guidance and reproducibility tips

Thanks for checking out `eyeris`! 🧠

------------------------------------------------------------------------

**Pre-CRAN `dev` GitHub releases:** 

# eyeris 0.1.1.9000 "Jake the Dog" ![Jake the Dog](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/jake.png){alt="Jake the Dog" width="40"}

## 🔧 Minor improvements and fixes

-   FF (#115): add more aggressive handling of edge cases in `eyeris::detransient()` (#121)
    -   Specifically, situations where pupil data appear to have already undergone some type of online filtering directly from the EyeLink Host PC machine.
    -   There is now detailed instructions on what to do if this exception is raised.
    -   Furthermore, a new `mad_thresh` override parameter has been added to `eyeris::detransient()` for advanced users to override the `mad_thresh` computed property. *Note:* this new `mad_thresh` parameter defaults to `NULL` (and should pretty much always stay as such).
-   FF (#122): fixed issue with incompatible unicode character in plot titles (#123)

# eyeris 0.1.0.9000 "Jake the Dog" ![Jake the Dog](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/jake.png){width="40"}

## 💥 Breaking changes

-   NF (#10): add support for `.asc` files containing multiple recording segments within the same file (#120)
    -   There is a new `block` argument added to the `load_asc()` function
    -   The default setting is "auto", which aims to automatically handle multiple recording segments within the same `.asc` file. We recommend using this default as this is likely the *safer choice* rather than assuming a single-block recording. **Furthermore, add downstream functions are intentionally designed to support any *N* number of blocks; using the "auto" setting automatically enables this support for `.asc` files containing single recording blocks by labeling the single recording session as `block_1`**
    -   You can also manually specify a different block value (numeric) instead of "auto", which can be helpful for multi-block experiments where each block/run was recorded to a separate `.asc` file. This is especially important to consider when running the downstream `epoch()` and `bidsify()` functions so that derived files and summary reports are properly labeled with the correction block/run number
    -   Currently, there is also a `NULL` option; however, this is likely going to just be a part of `beta` testing and will probably be removed in a future version given the foregoing 2 options should cover most (if not all) use cases

## ✨ New features

-   NF: robust HTML and PDF output summary sidecar reports within `bidsify()` (#120)
    -   Here, reports are well-organized both by block/run and any specific event message epochs that have been processed using the `epoch()` function
    -   For epoched data, we now provide a useful *interactive utility* within the epoch-specific HTML reports: you can now use your mouse and/or keyboard to swiftly navigate through an interactive gallery of pupil plot segments from every single trial from any given subject, right out-of-the-box! We hope this alleviates some of the complexities/roadblocks users might face when needing to perform manual inspections of their data for quality assurance and/or diagnostic purposes.

## 🔧 Minor improvements and fixes

-   FF (#118): resolved a minor bug in the EyeLink EDF header `model` and `version` fields for data collected on newer EyeLink hardware/software (#120)
-   ENH: event epoching is now both **more robust** and **super fast** (#120)
    -   We have implemented more efficient data structures to swiftly handle large sets of pupil samples in rapid time
    -   We have also added in better visual feedback within the console regarding epoching progress
    -   Similarly, these added benefits coincide nicely with the new multi-block support (#10)
-   General bug fixes and enhancements to codebase and front-end UX (#120)

# eyeris 0.0.0.9000 "Finn the Human" ![Finn the Human](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/finn.png){width="35"}

-   Initial beta release
