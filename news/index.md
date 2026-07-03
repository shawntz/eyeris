# Changelog

## eyeris 3.2.0.9000 (development version)

### 🐛 Bugs fixed

- **FF**: Fixed multi-run epoch CSV files
  (`*_desc-preproc_pupil_epoch-<label>.csv`) not being written for each
  run. When a subject’s data contained multiple blocks (runs),
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  built the per-epoch preprocessed output filename from the `run_num`
  argument — which is intentionally ignored (`NULL`) for multi-block
  objects — and packed the epoch label directly into the `desc` field
  without the `run-` and `epoch-` BIDS entities. As a result, every run
  wrote to the *same* filename and silently overwrote the previous one
  (last-run-wins), so the expected
  `..._run-NN_desc-preproc_pupil_epoch-<label>.csv` files never
  materialized for multi-run inputs. The multi-run writer now derives
  the run number from each block and routes the epoch label through the
  BIDS filename builder, exactly mirroring the single-run path. The
  DuckDB/parquet outputs were already keyed by the correct per-run
  number and are unchanged — only the CSV filenames were affected. As
  part of the same fix, the multi-run raw-timeseries writers now key the
  `run-NN` token off each block’s own block number (rather than a
  positional index, which mislabeled runs when blocks were not numbered
  sequentially), and several internal epoch/baseline metadata helpers no
  longer receive `verbose` in their `block_name` argument position
  (which crashed
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  under `verbose = FALSE` whenever epochs were present), by
  [@alicexue](https://github.com/alicexue) and
  [@shawntz](https://github.com/shawntz).

### 🔧 Under the hood

- **ENH**: Migrated all diagnostic plotting from base graphics to
  [`reaborn`](https://reaborn.org) — an R port of the ‘Python’ ‘seaborn’
  library built on ‘ggplot2’.
  [`plot.eyeris()`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md),
  the gaze heatmap
  ([`plot_gaze_heatmap()`](https://eyeris.shawnschwartz.com/reference/plot_gaze_heatmap.md)),
  the binocular-correlation panels
  ([`plot_binocular_correlation()`](https://eyeris.shawnschwartz.com/reference/plot_binocular_correlation.md)),
  the detrend overlay, the pupil-size distribution histograms, the
  progressive-summary report plot, and the per-epoch gallery figures are
  now rendered with `reaborn` and composited with `patchwork`, giving
  the interactive HTML reports a consistent, publication-quality seaborn
  aesthetic while preserving eyeris’s colour palette and axis labels.
  Each plotting function now builds `ggplot` objects and prints them to
  the active graphics device, so the existing report/gallery capture
  flow
  ([`png()`](https://rdrr.io/r/grDevices/png.html)/[`jpeg()`](https://rdrr.io/r/grDevices/png.html)
  → draw → [`dev.off()`](https://rdrr.io/r/grDevices/dev.html)) is
  unchanged and every public plotting API keeps its signature.
  Multi-panel time series previews are composited with `patchwork`
  (replacing base `par(mfrow)` layouts), missing-sample gaps are shaded
  as contiguous regions, and the binocular scatter panels overlay a
  dashed identity line. Signal-processing frequency-response (Bode)
  plots for
  [`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md)/[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md)
  intentionally remain on `gsignal`, as they fall outside the
  statistical-visualization scope of `reaborn`. Adds `reaborn`,
  `ggplot2`, and `patchwork` to `Imports` and drops the now-unused
  `fields`, by [@shawntz](https://github.com/shawntz).

## eyeris 3.2.0 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2026-06-19

This release fixes several correctness and data-integrity bugs and adds
new transparency and reproducibility tooling. Bug fixes resolve a
report/figure collision when different task names shared the same run
number within a subject/session, misleading diagnostic plots for
pipeline steps that precede downsampling/binning, and silent data loss
in
[`eyeris_db_read()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_read.md)/[`eyeris_db_collect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_collect.md)
when table schemas diverged across `eyeris` versions. New features add a
“percent data lost” annotation to the HTML report, expose a
`prop_missing`/`n_missing` missing-data column at the block and trial
levels for user-defined filtering, and introduce
[`boilerplate()`](https://eyeris.shawnschwartz.com/reference/boilerplate.md),
an fMRIPrep-style generator that auto-writes copy-and-paste-ready
methods text from the parameters captured in your pipeline.
Documentation now cross-references every modular preprocessing function
to a complete end-to-end reference pipeline.

### 🐛 Bugs fixed

- **FF ([\#293](https://github.com/shawntz/eyeris/issues/293))**: Fixed
  a conflict where running
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  for two different tasks that share the same run number (e.g.,
  `task-study_run-01` and `task-test_run-01`) under the same
  subject/session caused the second task to silently overwrite the first
  task’s HTML report and figures. BIDS allows different task names to
  pair with the same run number within a block, but the report
  (`sub-xyz.html`), the `source/figures/run-XX/` directories, and the
  per-run `source/logs/run-XX_metadata.json` were keyed by run number
  alone (and the report file by subject alone), so they collided across
  tasks. The combination of `task` + `run` is now treated as the unique
  key: figure directories and their figures are named
  `task-{task}_run-XX[...]`, the preprocessing report is named
  `sub-{sub}_task-{task}[...].html`, and the epoch gallery report and
  zip files are likewise task-namespaced. A new internal
  [`make_run_dir_name()`](https://eyeris.shawnschwartz.com/reference/make_run_dir_name.md)
  helper is the single source of truth shared by every writer and
  reader. The underlying data files (CSV/parquet/database) already
  included the task entity and are unchanged. As a side effect this also
  corrects a latent mismatch where the gaze-heatmap filename and
  binocular-correlation plots ignored the `run_num` override. **Note:**
  for single-task workflows this changes the on-disk report filename and
  figure directory names (now task-namespaced); regenerate reports to
  pick up the new layout, by [@shawntz](https://github.com/shawntz) and
  [@alicexue](https://github.com/alicexue) in
  [\#293](https://github.com/shawntz/eyeris/issues/293).

- **FF ([\#294](https://github.com/shawntz/eyeris/issues/294))**: Fixed
  misleading diagnostic plots for pipeline steps that precede
  downsampling/binning. When `downsample` (or `bin`) was enabled, the
  working time series retained only the decimated samples, so diagnostic
  plots for earlier steps (e.g., `deblink`) were rendered at the
  decimated rate — making intact data appear largely absent. The
  full-resolution (pre-decimation) time series is now preserved in
  `eyeris$timeseries_pre_decimation` when a
  [`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md)/[`bin()`](https://eyeris.shawnschwartz.com/reference/bin.md)
  step runs, and
  [`plot.eyeris()`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md)
  (plus the progressive-summary report plot) now renders each step at
  the appropriate resolution: steps preceding decimation use the
  original full-resolution data, while the decimation step and any
  subsequent steps use the decimated data, by
  [@shawntz](https://github.com/shawntz) and
  [@alicexue](https://github.com/alicexue) in
  [\#294](https://github.com/shawntz/eyeris/issues/294).

- **FF**: Fixed blank “raw” pupil-size histograms in multi-run
  diagnostic reports. When `plot_distributions = TRUE`,
  [`plot_pupil_distribution()`](https://eyeris.shawnschwartz.com/reference/plot_pupil_distribution.md)
  outlined every histogram bar in white (`border = "white"`). For the
  raw step — whose wide, outlier-laden spread yields the most
  Freedman-Diaconis bins — the white outline completely covered the (now
  very thin) bar fills, so the histogram rendered as a blank white
  panel. Whether a given run crossed that threshold depended on its data
  spread (i.e., its number of bins), which is why some runs in a
  multi-run report showed a normal raw histogram while others appeared
  empty. The bar outline is now dropped once there are too many bars so
  the distribution always stays visible, and the helper additionally
  guards against empty/all-`NA`/near-constant inputs (drawing an
  informative panel or falling back to default breaks) instead of
  raising an error, by [@shawntz](https://github.com/shawntz) and
  [@alicexue](https://github.com/alicexue) in
  [\#319](https://github.com/shawntz/eyeris/issues/319).

- **FF**: Fixed a crash that broke *all* multi-block (multi-run)
  diagnostic plotting and HTML report generation on R `>= 4.2`.
  [`get_block_numbers()`](https://eyeris.shawnschwartz.com/reference/get_block_numbers.md)
  guarded its return value with `if (is.na(block_nums))`, but for a
  multi-block object `block_nums` is a vector (one entry per block), so
  the condition had length `> 1` — a hard error on modern R
  (`"the condition has length > 1"`). The fallback is now applied
  element-wise, so multi-block objects return one number per block; this
  path is exercised by both
  [`plot.eyeris()`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md)
  and
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md),
  by [@shawntz](https://github.com/shawntz) and
  [@alicexue](https://github.com/alicexue) in
  [\#319](https://github.com/shawntz/eyeris/issues/319).

- **FF ([\#310](https://github.com/shawntz/eyeris/issues/310))**: Made
  [`eyeris_db_read()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_read.md)
  (and, by extension,
  [`eyeris_db_collect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_collect.md))
  tolerant of tables whose column schemas diverge across `eyeris`
  versions. Previously, the function built a naive
  `SELECT * FROM t1 UNION ALL SELECT * FROM t2 ...` across every table
  matching a `data_type`, which requires every matched table to share an
  identical column schema. In an incremental study that spans an
  `eyeris` upgrade — e.g., older `run_confounds_*` /
  `confounds_events_*` tables written before the `n_missing` /
  `prop_missing` columns were added, collected alongside data from a
  newer version — DuckDB raised
  `Binder Error: Set operations can only apply to expressions with the same number of result columns`.
  Because the read was wrapped in
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html), this surfaced
  as a warning and an empty
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html),
  i.e. **silent data loss**.
  [`eyeris_db_read()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_read.md)
  now computes the union of columns across all matching tables and
  projects each table onto a consistent, ordered column list (filling
  absent columns with `NULL`/`NA`) before the union, and
  [`eyeris_db_collect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_collect.md)
  reconciles epoch-label results with a fill-aware bind. Identifiers are
  SQL-escaped before quoting so column or table names are handled
  safely, and an informational message is logged whenever divergent
  schemas are detected and aligned, by
  [@shawntz](https://github.com/shawntz) in
  [\#310](https://github.com/shawntz/eyeris/issues/310).

### ✨ New features

- **ENH ([\#296](https://github.com/shawntz/eyeris/issues/296))**: Added
  a “percent data lost” annotation to the timeseries visualizations in
  the HTML report. Each run in the *Preprocessed Data Previews* section
  now displays the percent of samples in the raw pupil timeseries that
  are invalid (missing/during a blink, or off-screen), surfacing data
  loss directly in the report to reinforce workflow transparency. The
  metric reuses the canonical `prop_invalid` value from
  [`summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md)
  when available and falls back to computing missingness directly from
  the raw timeseries otherwise, by
  [@shawntz](https://github.com/shawntz) in
  [\#296](https://github.com/shawntz/eyeris/issues/296).

- **NF ([\#297](https://github.com/shawntz/eyeris/issues/297))**:
  Exposed a missing-data column for user-defined filtering.
  [`summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md)
  (and the
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  pipeline that calls it) now reports the proportion of missing (`NA`)
  pupil samples as `prop_missing` (ranging `0`–`1`; multiply by `100`
  for a percentage), alongside its raw count `n_missing`. These are
  computed at two levels so you can choose the granularity appropriate
  to your design: per recording **block**
  (`confounds$unepoched_timeseries`, exported as `run_confounds`) and
  per epoched event/**trial** (`confounds$epoched_timeseries`, exported
  as `confounds_events`). `eyeris` intentionally does not enforce a
  fixed missing-data exclusion cutoff; instead, `prop_missing` is
  surfaced so users can define their own exclusion thresholds at
  whichever level (trial, epoch, or block) suits their study. Unlike
  `prop_invalid`, `prop_missing` reflects only `NA`/dropout samples and
  does not fold in blink or off-screen flags. The database guide
  vignette was updated with block- and trial-level filtering examples,
  by [@shawntz](https://github.com/shawntz) in
  [\#297](https://github.com/shawntz/eyeris/issues/297).

  > **Upgrade note**: because this adds columns to the `run_confounds`
  > and `confounds_events` tables, a project `DuckDB` that already
  > contains confounds tables written by eyeris `<= 3.1.0` should be
  > regenerated before collecting it (e.g., via
  > [`eyeris_db_collect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_collect.md))
  > alongside data written by this version. Mixing the old and new
  > confounds schemas in the same database is not currently supported by
  > [`eyeris_db_read()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_read.md).

- **NEW ([\#302](https://github.com/shawntz/eyeris/issues/302))**: Added
  [`boilerplate()`](https://eyeris.shawnschwartz.com/reference/boilerplate.md),
  an fMRIPrep-style methods-text generator that auto-writes a
  reproducible, copy-and-paste-ready Markdown description of the exact
  preprocessing workflow that was run, generated directly from the
  parameters captured in `eyeris$params`. The generator walks the
  pipeline steps in canonical order (`load_asc` → `deblink` →
  `detransient` → `interpolate` → `lpfilt` → `downsample`/`bin` →
  `detrend` → `zscore` → `epoch`), substitutes in the actual parameter
  values used, and handles multi-block (multi-run), multi-step,
  custom-extension, and binocular pipelines. The boilerplate is now
  embedded in every diagnostic HTML report (new “Reproducible Methods
  Boilerplate” section) and written to
  `derivatives/.../source/logs/methods_boilerplate.md` alongside the
  per-run `.json` metadata sidecars it references. The generated text is
  licensed under [Creative Commons Attribution 4.0 International (CC BY
  4.0)](https://creativecommons.org/licenses/by/4.0/), with an explicit
  note letting users know it is safe to paste the Markdown content
  directly into their manuscript’s methods section as long as they cite
  `eyeris` to provide the required attribution, by
  [@shawntz](https://github.com/shawntz) in
  [\#302](https://github.com/shawntz/eyeris/issues/302).

### 📚 Documentation

- **DOC ([\#298](https://github.com/shawntz/eyeris/issues/298))**: Added
  cross-references from every modular preprocessing function
  ([`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md),
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md),
  [`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md),
  [`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md),
  [`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md),
  [`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md),
  [`bin()`](https://eyeris.shawnschwartz.com/reference/bin.md),
  [`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md),
  and
  [`zscore()`](https://eyeris.shawnschwartz.com/reference/zscore.md)) to
  a complete, end-to-end reference pipeline that demonstrates how all
  functions are chained together in practice. Each function’s help page
  now points readers to the “Building Blocks Under the Hood” section of
  the *Anatomy of an `eyeris` Object* vignette, which was expanded into
  an explicit, fully-annotated reference that maps each step one-to-one
  to the default
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  recipe. The *Complete Pupillometry Pipeline Walkthrough* vignette now
  links to that reference from a new “Advanced: Building the Pipeline
  Manually” section, by [@shawntz](https://github.com/shawntz).

## eyeris 3.1.0 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2026-06-05

This minor release delivers several robustness and stability
improvements, fixing memory issues during HTML report rendering,
correcting epoch plot compression after downsampling, and improving
documentation accuracy.

### 🐛 Bugs fixed

- **FF ([\#275](https://github.com/shawntz/eyeris/issues/275))**: Fixed
  HTML rendering failure on headless Linux systems with older pandoc
  versions (e.g., v2.7.3) caused by a missing sticker path. Previously,
  the blanket exclusion of `^inst/figures$` in `.Rbuildignore` caused
  `system.file("figures", "sticker.png", package = "eyeris")` to return
  `""`, producing `<img src=''>` markup that older pandoc versions
  cannot render. Replaced the blanket exclusion with targeted file-level
  exclusions for large demo assets (GIFs, character images, annotated
  example screenshots) while keeping only the essential `sticker.png`
  (21KB) in the installed package. Net package size increase is +21KB,
  well within CRAN’s 5MB limit, by
  [@shawntz](https://github.com/shawntz) in
  [\#286](https://github.com/shawntz/eyeris/issues/286).

- **FF ([\#278](https://github.com/shawntz/eyeris/issues/278))**: Fixed
  pandoc out-of-memory (`exit 137`) errors when rendering HTML reports
  with large epoch event data.
  [`format_call_stack()`](https://eyeris.shawnschwartz.com/reference/format_call_stack.md)
  was calling [`deparse()`](https://rdrr.io/r/base/deparse.html) on all
  parameters including large epoch event lists (thousands of rows),
  generating strings that consumed gigabytes of memory. Added
  [`should_omit_parameter()`](https://eyeris.shawnschwartz.com/reference/should_omit_parameter.md)
  helper to detect and filter epoch-related parameters containing
  complex objects (lists/data.frames) before deparsing. Large parameters
  are now displayed as `<omitted>` in the call stack while scalar values
  (e.g., `epoch_length = 100`) are preserved. Added comprehensive test
  coverage for parameter filtering, case-insensitive matching, and
  scalar preservation, by [@shawntz](https://github.com/shawntz) in
  [\#280](https://github.com/shawntz/eyeris/issues/280).

- **FF ([\#291](https://github.com/shawntz/eyeris/issues/291))**: Fixed
  downsampled epoch data being visually compressed in HTML plots. When
  downsampling was applied in the Glassbox pipeline, epoch plots showed
  5-second windows compressed to ~0.5 seconds (a 10x compression). The
  root cause was that
  [`epoch_pupil()`](https://eyeris.shawnschwartz.com/reference/epoch_pupil.md)
  used the original sampling rate (`info$sample.rate`) instead of the
  decimated rate (`decimated.sample.rate`) when calculating epoch
  timebins, causing sample counts to be divided by the wrong Hz value.
  Updated
  [`epoch_pupil()`](https://eyeris.shawnschwartz.com/reference/epoch_pupil.md)
  to check for `decimated.sample.rate` first before falling back to the
  original rate. X-axis labels and CSV output were already correct; this
  fix applies the same logic to the plot rendering, by
  [@shawntz](https://github.com/shawntz) and
  [@alicexue](https://github.com/alicexue) in
  [\#292](https://github.com/shawntz/eyeris/issues/292).

### 🔧 Minor improvements and fixes

- **FF ([\#287](https://github.com/shawntz/eyeris/issues/287))**: Fixed
  misleading
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  usage pattern in the database guide documentation, where examples
  incorrectly instructed users to call
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  and pipe the result into
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md).
  This caused errors because `eyeris` expects a file path string to be
  passed directly into the pipeline;
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  is handled internally. Updated examples to show the correct usage
  pattern and added a runtime error in
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  that detects when a user passes an `eyeris` object instead of a file
  path, with a helpful message explaining the correct approach, by
  [@shawntz](https://github.com/shawntz) and
  [@alicexue](https://github.com/alicexue) in
  [\#288](https://github.com/shawntz/eyeris/issues/288).

### 📚 Documentation

- **DOC**: Added a system requirements section to the README with
  platform-specific guidance for installing runtime dependencies, by
  [@shawntz](https://github.com/shawntz) in
  [\#282](https://github.com/shawntz/eyeris/issues/282).

## eyeris 3.0.1 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2025-10-07

This patch release improves dependency management for Arrow and DuckDB
to prevent installation issues on macOS and other platforms.

### 🔧 Dependency management improvements

- **ENH**: Moved `arrow` from Imports to Suggests to prevent hanging
  installation issues on macOS. The arrow package requires system
  dependencies (pkg-config, cmake, apache-arrow via Homebrew on macOS)
  that could cause installation to hang indefinitely when building from
  source. eyeris now gracefully falls back to DuckDB for parquet
  operations when arrow is not available, with informative installation
  instructions provided via
  [`check_arrow()`](https://eyeris.shawnschwartz.com/reference/check_arrow.md)
  helper function, by [@shawntz](https://github.com/shawntz) in
  [\#273](https://github.com/shawntz/eyeris/issues/273).

- **ENH**: Added comprehensive installation guidance for Arrow and
  DuckDB dependencies. New
  [`check_arrow()`](https://eyeris.shawnschwartz.com/reference/check_arrow.md)
  helper function provides platform-specific installation instructions
  (macOS, Linux, Windows) with detailed steps for installing required
  system dependencies. Startup messages now inform users about missing
  optional dependencies and point to installation documentation, by
  [@shawntz](https://github.com/shawntz) in
  [\#273](https://github.com/shawntz/eyeris/issues/273).

### 📚 Documentation

- **DOC**: Updated README with detailed instructions for installing
  Arrow and DuckDB dependencies across different platforms, including
  Homebrew setup for macOS, system package installation for Linux
  distributions, and notes about when these packages are required, by
  [@shawntz](https://github.com/shawntz) in
  [\#273](https://github.com/shawntz/eyeris/issues/273).

## eyeris 3.0.0 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2025-09-17

This major release delivers multiple performance enhancements and
feature improvements, reducing processing time and improving overall
efficiency across the pipeline.

### 🚨 **Breaking changes & deprecations**

- **DEPRECATED: `merge_runs` and `merge_epochs` parameters in
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)**:
  Both `merge_runs` and `merge_epochs` parameters are now deprecated.
  All runs and epochs are now saved as separate files following proper
  BIDS conventions, which is the recommended approach for neuroimaging
  data standards. Users relying on these features should update their
  analysis workflows to handle separate run and epoch files. Deprecation
  warnings will be shown when these parameters are used.

### 🚀 New features

- **NF**: **Zip-based epoch gallery system**: Replaced individual epoch
  image files with zip-based storage and loading system. The
  [`make_gallery()`](https://eyeris.shawnschwartz.com/reference/make_gallery.md)
  function now creates epoch images in zip files instead of individual
  PNG files, and uses `zip.js` to dynamically load images in the HTML
  gallery. This reduces file count, improves organization, and provides
  more efficient loading while maintaining full backward compatibility
  with existing individual image files. This is particularly beneficial
  for high-throughput compute environments with limited inode quotas and
  simplifies file transfers to cloud storage providers like GitHub LFS,
  by [@shawntz](https://github.com/shawntz) in
  [\#254](https://github.com/shawntz/eyeris/issues/254).

- **NF**: **Automated source figure cleanup**: Added post-render cleanup
  functionality that automatically zips all `png` and `jpg` files in
  each `source/figures/run-xx/` directory after the main HTML report is
  generated before deleting the individual image files, further reducing
  file count burden while preserving all figure data in compressed
  format, creating only one zip file per run per subject instead of
  hundreds of individual image files, by
  [@shawntz](https://github.com/shawntz) in
  [\#255](https://github.com/shawntz/eyeris/issues/255).

- **NF - DuckDB database integration**: Added optional `DuckDB` database
  functionality to
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  as an alternative to CSV files for large-scale analyses. When
  `db_enabled = TRUE`, all `eyeris` data (timeseries, epochs, events,
  blinks, confounds) are written to a centralized database for efficient
  querying and analysis. Features include seamless out-of-the-box
  configuration, user-friendly database functions
  ([`eyeris_db_collect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_collect.md),
  [`eyeris_db_connect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_connect.md),
  [`eyeris_db_read()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_read.md),
  [`eyeris_db_list_tables()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_list_tables.md)),
  and `dplyr`-style data access. CSV file generation can be optionally
  disabled with `csv_enabled = FALSE` for cloud compute environments, by
  [@shawntz](https://github.com/shawntz) in
  [\#256](https://github.com/shawntz/eyeris/issues/256).

- **NF**: **Added parallel processing support for DuckDB database
  operations** to prevent concurrency issues during batch processing.
  Implemented temporary database creation with automatic merging and
  cleanup mechanisms. Added environment variable detection for common
  HPC schedulers (SLURM, PBS, SGE, LSF) and manual `parallel_processing`
  parameter override. Includes comprehensive file locking, process/job
  ID logging, and full test coverage, by
  [@shawntz](https://github.com/shawntz) in
  [\#262](https://github.com/shawntz/eyeris/issues/262).

> This enhancement enables seamless parallel compute and batch
> processing when using `db_enabled = TRUE`. Each parallel job now
> writes to a unique temporary database, preventing the crashes that
> occurred when multiple processes attempted concurrent writes to the
> same DuckDB file.
>
> Key features: - **Automatic Detection**: Detects HPC environments
> (SLURM_JOB_ID, PBS_JOBID, etc.) - **Temporary Databases**: Each job
> uses PID + timestamp for unique temp database names - **Safe
> Merging**: File-based locking prevents concurrent access during merge
> operations - **Rich Logging**: Job ID and process ID included in all
> parallel processing messages - **Zero Breaking Changes**: All existing
> functionality preserved
>
> Usage examples:
>
> ``` r
>
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

- **NF**: **Large-scale database export functionality**. Added
  [`eyeris_db_to_chunked_files()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_to_chunked_files.md)
  and
  [`process_chunked_query()`](https://eyeris.shawnschwartz.com/reference/process_chunked_query.md)
  functions to handle really large `eyerisdb` databases by processing
  data in configurable chunks (default `1M rows`) with automatic file
  size limits (default `500MB`) and numbered file splitting (`_01-of-N`
  pattern). Supports both `CSV` and `Parquet` output formats with
  memory-efficient streaming processing, by
  [@shawntz](https://github.com/shawntz) in
  [\#266](https://github.com/shawntz/eyeris/issues/266).

- **NF**: **Robust parquet export/read for `eyerisdb` mixed schemas**.
  [`eyeris_db_to_parquet()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_to_parquet.md)
  and
  [`read_eyeris_parquet()`](https://eyeris.shawnschwartz.com/reference/read_eyeris_parquet.md)
  now combine tables/files using schema-aligned binding (by column name;
  fill missing) via
  `data.table::rbindlist(use.names = TRUE, fill = TRUE)`. Fixes “numbers
  of columns of arguments do not match” when different `epochs_*` tables
  have slightly different columns, by
  [@shawntz](https://github.com/shawntz) in
  [\#266](https://github.com/shawntz/eyeris/issues/266).

  > **Key Features:**
  >
  > - **Chunked Processing**: Handles databases of any size without
  >   memory issues using configurable chunk sizes
  > - **Automatic File Splitting**: Creates numbered files when size
  >   limits exceeded (e.g., `data_01-of-03.csv`)
  > - **Smart Schema Grouping**: Dynamically groups tables by column
  >   structure to prevent SQL UNION errors
  > - **Database Safety**: Comprehensive temp table cleanup and
  >   contamination prevention
  > - **Comprehensive Documentation**: Detailed vignette with real-world
  >   examples and troubleshooting

- **NF**: **Database sharing and distribution functionality**. Added
  [`eyeris_db_split_for_sharing()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_split_for_sharing.md)
  and
  [`eyeris_db_reconstruct_from_chunks()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_reconstruct_from_chunks.md)
  functions to facilitate sharing of large eyeris databases via
  platforms with file size limits (GitHub, OSF, data repositories).
  Supports chunking strategies by data type, count, or size limits with
  epoch label grouping for efficient organization. Includes
  comprehensive metadata for reliable database reconstruction, by
  [@shawntz](https://github.com/shawntz) in
  [\#269](https://github.com/shawntz/eyeris/issues/269).

### 🔧 Major code improvements

- **Simplified
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  function**: Significantly refactored and streamlined the
  `pipeline-bidsify.R` file to improve maintainability and reduce code
  complexity:

  - **Removed merge_runs and merge_epochs logic**: Eliminated ~250+
    lines of complex conditional logic for merging runs and epochs,
    simplifying the codebase while ensuring BIDS compliance
  - **Consolidated baseline data access**: Updated helper functions to
    prioritize baseline information stored within
    `epoch_xx$block_1$baseline` structure, providing consistent data
    access patterns while maintaining backward compatibility with legacy
    storage locations
  - **Simplified epoch summary generation**: Replaced 257 lines of
    repetitive [`sapply()`](https://rdrr.io/r/base/lapply.html) calls
    with a clean 77-line implementation using
    [`unlist()`](https://rdrr.io/r/base/unlist.html) approach for
    generating `*_desc-epoch_summary.csv` files, improving code
    readability and maintainability
  - **Added helper functions**: Created centralized functions
    (`get_epoch_info()`, `get_baseline_events()`, `get_baseline_type()`,
    `has_baseline()`) to eliminate code duplication and provide
    consistent data access across the pipeline

- **ENH**: Added post-render cleanup of figures directories in
  `pipeline-bidsify.R`. Enhanced `zip_and_cleanup_source_figures` to
  remove existing zip files before reprocessing, move new zip files to
  the parent figures directory, and delete run directories after
  successful zipping. This streamlines figures management and prevents
  leftover files from previous runs, by
  [@shawntz](https://github.com/shawntz) in
  [\#261](https://github.com/shawntz/eyeris/issues/261).

  > Further cleans up number of derived files in `eyeris` BIDS
  > directories (which is especially useful for cloud compute
  > deployments).
  >
  > The new behavior will be:
  >
  > 1.  Create zip files in the `figures/` directory (i.e., one level up
  >     from the run directories)
  > 2.  Remove existing zip files when reprocessing
  > 3.  Remove the entire `run-XX/` directories after successful zip
  >     creation
  >
  > As such, the final file structure will be:

  ``` bash
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

- **FF**: Handle non-finite values in plotting xlim ranges to prevent
  [`plot.window()`](https://rdrr.io/r/graphics/plot.window.html)
  crashes. Added `finite=TRUE` parameter to
  [`range()`](https://rdrr.io/r/base/range.html) calls and fallback
  logic when no finite values exist in timebin or x_seq data. Resolves
  “need finite ‘xlim’ values” error during epoch visualization, by
  [@shawntz](https://github.com/shawntz) in
  [\#263](https://github.com/shawntz/eyeris/issues/263).

- **FF**: Standardize column structure before rbind in epoch summaries
  to prevent column mismatch errors. Different epochs can have varying
  metadata structures (9 vs 10 fields), causing
  [`rbind()`](https://rdrr.io/r/base/cbind.html) to fail. Added logic to
  collect all unique column names, standardize structure with NA values
  for missing columns, and ensure consistent column ordering before
  combining data frames, by [@shawntz](https://github.com/shawntz) in
  [\#264](https://github.com/shawntz/eyeris/issues/264).

- **ENH**: **Improved database summary performance**.
  [`eyeris_db_summary()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_summary.md)
  now extracts subject/session/task information directly from table
  names instead of sampling database contents, providing complete
  coverage of all subjects and much faster execution, by
  [@shawntz](https://github.com/shawntz) in
  [\#266](https://github.com/shawntz/eyeris/issues/266).

- **ENH**: **Enhanced temp table safety**. All database export functions
  now automatically detect, warn about, and exclude temporary tables
  from processing. Added safe temporary table operations with guaranteed
  cleanup even on process crashes, by
  [@shawntz](https://github.com/shawntz) in
  [\#266](https://github.com/shawntz/eyeris/issues/266).

- **ENH**: **Optimized database export performance and reliability**.
  [`eyeris_db_to_chunked_files()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_to_chunked_files.md)
  now uses a hybrid approach: database-level export via DuckDB’s `COPY`
  command for large file size limits (≥500MB) for maximum performance,
  and chunked processing for smaller limits (\<500MB) to ensure proper
  file size splitting for git-lfs workflows. Additionally, parquet file
  handling has been improved to eliminate unreliable appending that
  caused `_chunk_` files, now using a cleaner numbered file approach
  with 80% size thresholds to prevent append failures, by
  [@shawntz](https://github.com/shawntz) in
  [\#268](https://github.com/shawntz/eyeris/issues/268).

- **ENH**: **Enhanced chunked database export with epoch label
  grouping**.
  [`eyeris_db_to_chunked_files()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_to_chunked_files.md)
  now supports `group_by_epoch_label` parameter (default: TRUE) that
  processes epoch-related data types separately by epoch label, reducing
  memory footprint and creating label-specific output files. Added
  helper functions for epoch label extraction and improved table
  grouping logic for better organization of exported data, by
  [@shawntz](https://github.com/shawntz) in
  [\#270](https://github.com/shawntz/eyeris/issues/270).

### 🔧 Minor improvements and fixes

- **BF ([\#250](https://github.com/shawntz/eyeris/issues/250))**:
  Updated functions in `pipeline-confounds.R` and `pipeline-epoch.R` to
  handle cases where ‘start_matched_event’ is used instead of
  ‘matched_event’ in epoched data. Resolved an issue with inconsistent
  variable naming introduced in \#a8df0c0, and fixed time and duration
  calculations in
  [`epoch_start_end_msg()`](https://eyeris.shawnschwartz.com/reference/epoch_start_end_msg.md)
  to use correct units and sample counts, by
  [@shawntz](https://github.com/shawntz) in
  [\#251](https://github.com/shawntz/eyeris/issues/251).

- **RF**: **Added Bootstrap and Lightbox assets** (CSS, JS, fonts,
  images) to `inst/www` and updated `.Rbuildignore` to exclude them from
  builds. Refactored make_gallery to use local copies of these
  dependencies with CDN fallbacks, improving offline support and
  reliability of gallery reports, by
  [@shawntz](https://github.com/shawntz) in
  [\#252](https://github.com/shawntz/eyeris/issues/252).

- **DOC**: Added a comprehensive ‘Internal API Reference’ vignette
  documenting all internal functions for advanced users and developers
  and updated the README to link to the new vignette/included it in the
  pkgdown docs site configuration, by
  [@shawntz](https://github.com/shawntz) in
  [\#257](https://github.com/shawntz/eyeris/issues/257).

- **RF**: Updated documentation and comments across multiple files to
  improve clarity and align terminology throughout the codebase,
  especially in function descriptions, parameter names, and return value
  documentation, by [@shawntz](https://github.com/shawntz) in
  [\#257](https://github.com/shawntz/eyeris/issues/257).

- **FF**: Updated the
  [`log_message()`](https://eyeris.shawnschwartz.com/reference/log_message.md)
  function to use `tryCatch` when applying `glue` interpolation,
  ensuring that errors (e.g., from malformed braces or embedded `JSON`)
  do not interrupt logging; now, the original message is used if
  interpolation fails, by [@shawntz](https://github.com/shawntz) in
  [\#258](https://github.com/shawntz/eyeris/issues/258).

- **CHORE**: Add a GitHub Actions workflow to auto-render vignettes and
  publish them to the `eyeris` GitHub repo wiki, by
  [@shawntz](https://github.com/shawntz) in
  [\#259](https://github.com/shawntz/eyeris/issues/259).

- **RF - post-render cleanup to remove figures directory**: The
  [`cleanup_source_figures_post_render()`](https://eyeris.shawnschwartz.com/reference/cleanup_source_figures_post_render.md)
  function now removes the entire `source/figures` directory after
  report generation, as images are embedded in the `HTML`. Documentation
  and comments updated to reflect this change, and unused parameters are
  noted for compatibility, by [@shawntz](https://github.com/shawntz) in
  [\#261](https://github.com/shawntz/eyeris/issues/261).

- **RF - increase zip file embed size limit to 1GB**: Raised the maximum
  allowed zip file size for data `URL` embedding from `10MB` to `1GB` in
  [`print_lightbox_img_html()`](https://eyeris.shawnschwartz.com/reference/print_lightbox_img_html.md).
  Updated warning message to reflect the new limit, by
  [@shawntz](https://github.com/shawntz) in
  [\#261](https://github.com/shawntz/eyeris/issues/261).

- **RF - update report title in `make_report` function**: Changed the
  report title from ‘preprocessing summary report’ to ‘preprocessing
  report’ for consistency and clarity, by
  [@shawntz](https://github.com/shawntz) in
  [\#261](https://github.com/shawntz/eyeris/issues/261).

- **CHORE - update logo image URL**: Changed the `logo` image source in
  `README` files to use a `GitHub raw URL` for better compatibility, by
  [@shawntz](https://github.com/shawntz) in
  [\#261](https://github.com/shawntz/eyeris/issues/261).

- **FF**: Detect grouping column for epoch diagnostic plots. For
  start/end epochs (e.g., `"PROBE_S {STIM}"` to `"PROBE_E {STIM}"`), the
  epoched data contains `start_matched_event` instead of
  `matched_event`. Added automatic detection logic with priority:
  requested column → `start_matched_event` → `end_matched_event`.
  Includes informative logging and graceful fallback when no suitable
  column found, by [@shawntz](https://github.com/shawntz) in
  [\#265](https://github.com/shawntz/eyeris/issues/265).

- **FF**: Improved handling of mismatched start/end events in
  [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md). When
  using start/end event pairs (e.g.,
  `c("PROBE_S {STIM}", "PROBE_E {STIM}")`), the function now
  automatically matches events by extracting identifiers from event
  messages instead of failing with “Start and end timestamps must have
  the same number of rows”. Unmatched events are filtered out and the
  process continues with matched pairs only, with informative logging
  about the filtering process, by [@shawntz](https://github.com/shawntz)
  in [\#267](https://github.com/shawntz/eyeris/issues/267).

- **DOC**: Updated package documentation to include newly exported
  database functions in `_pkgdown.yml`, core function reference table in
  `README.Rmd`, and internal API vignette. Enhanced documentation
  coverage for database export and management functionality, by
  [@shawntz](https://github.com/shawntz) in
  [\#270](https://github.com/shawntz/eyeris/issues/270).

## eyeris 2.1.1 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2025-07-26

This release patches a few bugs that emerged as a result of the `v2.1.0`
minor release.

### 🔧 Minor improvements and fixes

- BF ([\#237](https://github.com/shawntz/eyeris/issues/237)): Ensure
  full raw timeseries `.csv` file is written by
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  for single-run (monocular) data, including cases with or without
  epoching and with run number override. Previously, the file was only
  written for multi-run data by [@shawntz](https://github.com/shawntz)
  in [\#240](https://github.com/shawntz/eyeris/issues/240).

- FF ([\#236](https://github.com/shawntz/eyeris/issues/236)): Certain
  `sprintf`-formatted log messages (e.g., those with ‘%s’) for
  blinks/events writing in `pipeline-bidsify.R` now parse and display
  correctly in the logger by using the internal `alert()` wrapper by
  [@shawntz](https://github.com/shawntz) in
  [\#241](https://github.com/shawntz/eyeris/issues/241).

- FF ([\#238](https://github.com/shawntz/eyeris/issues/238)): Only log
  `[INFO] Filtered epochs: ...` when data is epoched by
  [@shawntz](https://github.com/shawntz) in
  [\#242](https://github.com/shawntz/eyeris/issues/242).

- CHORE ([\#239](https://github.com/shawntz/eyeris/issues/239)): Remove
  duplicate logging events for “Created gaze heatmap” in
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  pipeline. The log message now appears only once per run and uses the
  `[OKAY]` log level for both run-level and epoch-level heatmap
  creation, improving clarity and consistency in logs, by
  [@shawntz](https://github.com/shawntz) in
  [\#243](https://github.com/shawntz/eyeris/issues/243).

- ENH DOC: Add GitHub Actions workflow for automated spellchecking of
  documentation and code using
  [r-spellcheck-action](https://github.com/marketplace/actions/r-spellcheck-action).
  The workflow runs on pushes and pull requests to `dev` and
  `release/**` branches by [@shawntz](https://github.com/shawntz) in
  [\#244](https://github.com/shawntz/eyeris/issues/244)

- CHORE: Adjust `pkgdown` CI deployment trigger conditions for PRs to be
  more specific about which branches should trigger the workflow by
  [@shawntz](https://github.com/shawntz) in
  [\#245](https://github.com/shawntz/eyeris/issues/245).

- CHORE: Update the GitHub Actions workflow configuration to fix CI
  triggers for the R CMD build/check action. The changes modify the
  workflow name and expand branch pattern matching to include release
  branches with additional path segments, by
  [@shawntz](https://github.com/shawntz) in
  [\#246](https://github.com/shawntz/eyeris/issues/246).

- CHORE: Standardize the GitHub Actions workflow configurations for air
  formatting operations. The changes update workflow names and branch
  targeting patterns to ensure consistent formatting checks and
  suggestions across the development workflow, by
  [@shawntz](https://github.com/shawntz) in
  [\#247](https://github.com/shawntz/eyeris/issues/247).

- CHORE: Resolve spelling errors throughout the package by correcting
  typos in documentation, comments, and code, while also creating a
  WORDLIST file for the spellchecker to recognize domain-specific terms
  by [@shawntz](https://github.com/shawntz) in
  [\#248](https://github.com/shawntz/eyeris/issues/248).

## eyeris 2.1.0 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2025-07-22

This minor release introduces significant enhancements, new features,
and robust improvements focusing on binocular data support, standardized
logging, and improved reporting and development workflows, substantially
elevating `eyeris'` functionality, robustness, and user experience.

### ✨ New features & enhancements

- **Comprehensive Binocular Recording Support**: `eyeris` now provides
  full binocular recording support, resolving a critical data loading
  bug previously encountered with binocular EyeLink recording data files
  ([\#216](https://github.com/shawntz/eyeris/issues/216) reported by
  [@anomalosepia](https://github.com/anomalosepia)). This enhancement
  includes nesting `eyeris` class lists with `left` and `right` parent
  lists to explicitly separate left eye (`L`) and right eye (`R`) data.
  All downstream operations are now designed to treat `L` and/or `R` as
  separate entities, which ensures no cross-contamination between data
  from the two eyes during processing. Additionally, the
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  function now appends `_eye-L` and/or `_eye-R` to all derivatives and
  output HTML reports for binocular data for clear naming conventions.
  Pipeline functions such as
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md),
  [`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md),
  [`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md),
  [`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md),
  [`zscore()`](https://eyeris.shawnschwartz.com/reference/zscore.md),
  [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md),
  [`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md),
  [`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md),
  and [`bin()`](https://eyeris.shawnschwartz.com/reference/bin.md) have
  been updated to handle binocular objects. A new
  [`plot_binocular_correlation()`](https://eyeris.shawnschwartz.com/reference/plot_binocular_correlation.md)
  function has also been added, and vignettes were updated to explain
  binocular data structures and usage with
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md);
  by [@shawntz](https://github.com/shawntz) in
  [\#228](https://github.com/shawntz/eyeris/issues/228).

- **Standardized Logging and Error Handling (using `cli` package)**: The
  `eyeris` package has undergone comprehensive refactoring to
  standardize and enhance the clarity of logging, warning, and error
  messages by transitioning fully to the `cli` R package. This replaces
  base R functions like
  [`message()`](https://rdrr.io/r/base/message.html),
  [`stop()`](https://rdrr.io/r/base/stop.html), and
  [`warning()`](https://rdrr.io/r/base/warning.html) with
  `cli::cli_alert_*`,
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
  and
  [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html)
  respectively. Consistent log level tags such as `[INFO]`, `[OKAY]`,
  `[WARN]`, and `[EXIT]` are now implemented within `cli` alert messages
  for clearer categorization and improved message clarity and uniformity
  across various pipelines and functions. These changes have been
  applied across key components including `run_bidsify`,
  `detransient_pupil`, `pipeline-epoch.R`, `interpolate_pupil`,
  `make_epoch_label`, `check_and_create_dir`, `compute_baseline`,
  `load_asc`, `process_eyeris_data`, `pipeline-glassbox.R`, and
  `plot.eyeris.R`; by [@shawntz](https://github.com/shawntz) in
  [\#229](https://github.com/shawntz/eyeris/issues/229).

- **Enhanced HTML Reports for Multi-Run Data:** HTML reports now
  accurately reflect all detected runs, including their metadata and
  call stack. This addresses a previous issue where only the latest run
  was shown, by modifying
  [`make_report()`](https://eyeris.shawnschwartz.com/reference/make_report.md)
  to detect all `run-xx` folders in `source/figures/` and updating
  [`save_progressive_summary_plots()`](https://eyeris.shawnschwartz.com/reference/save_progressive_summary_plots.md)
  to use the folder structure rather than `eyeris$timeseries` names. The
  metadata section now shows one line per run for the `.asc` file source
  and a formatted call stack for each run; by
  [@shawntz](https://github.com/shawntz) in
  [\#224](https://github.com/shawntz/eyeris/issues/224).

- **Integration of Air R Formatter:** The project’s R code formatting
  has transitioned to `Air`, an R formatter and language server written
  in Rust, replacing the lintr-based formatting system. This change
  involved deleting the `.github/workflows/linter.yml` file, introducing
  a new `.air.toml` configuration file, and adding two new GitHub
  workflows for format checking and suggestions. Furthermore,
  comprehensive code reformatting has been applied to all R source files
  to match Air’s standards, ensuring better line breaks, consistent
  spacing, and improved argument alignment throughout the entire
  codebase; by [@shawntz](https://github.com/shawntz) in
  [\#234](https://github.com/shawntz/eyeris/issues/234).

### 🔧 Minor improvements and fixes

- **Improved HTML Report Navigation:** The table of contents depth in
  HTML reports has been increased from 3 to 6 levels for better
  navigation by [@shawntz](https://github.com/shawntz) in
  [\#229](https://github.com/shawntz/eyeris/issues/229).

- **Refined Makefile Targets:** New Makefile targets have been added for
  CRAN presubmission checks, CRAN submission, GitHub releases, and code
  formatting with `Air`. This also includes expanded dependency
  installation and improved output formatting and redirection for
  cleaner logs by [@shawntz](https://github.com/shawntz) in
  [\#230](https://github.com/shawntz/eyeris/issues/230).

- **Event Placeholder Fix:** The event placeholder in example code and
  documentation has been updated from `{type}` to `{startstop}` to
  resolve a name conflict with an existing `type` column name in the
  derived `.csv` data files by [@shawntz](https://github.com/shawntz) in
  [\#232](https://github.com/shawntz/eyeris/issues/232).

### 📚 Documentation & Development Workflow Updates

- **Revamped README:** The `README` has been significantly expanded with
  detailed feature highlights, a comprehensive function reference table,
  and new sections outlining the BIDS-like file structure for both
  monocular and binocular data. It also includes quick links to eyeris
  tutorials (R CRAN package vignettes), improved example output, and
  updated section headings for clarity and visual appeal; by
  [@shawntz](https://github.com/shawntz) in
  [\#231](https://github.com/shawntz/eyeris/issues/231).

- **Enhanced Pull Request Template:** The GitHub pull request template
  has been updated to provide a more structured and detailed format for
  contributors. This update includes structured sections for problem
  description, key changes, and acknowledgments, providing detailed
  examples and expanding the breaking changes section into a more
  comprehensive checklist; by [@shawntz](https://github.com/shawntz) in
  [\#227](https://github.com/shawntz/eyeris/issues/227).

- **Makefile Maintenance:** Comments and section headers in the Makefile
  have been updated and clarified for better maintainability by
  [@shawntz](https://github.com/shawntz) in
  [\#230](https://github.com/shawntz/eyeris/issues/230).

## eyeris 2.0.0 “Lumpy Space Princess” ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png)

CRAN release: 2025-07-03

This is the largest update yet for `eyeris`, introducing a wealth of new
features and addressing numerous small issues to significantly enhance
functionality, robustness, and user experience
([\#215](https://github.com/shawntz/eyeris/issues/215)).

### ✨ New features

#### Enhanced reporting and visualization:

- **Progressive preprocessing summary plots** can now be generated and
  saved, visualizing the effects of each preprocessing step on pupil
  data and updating report structures to include these visualizations.
  The
  [`plot.eyeris()`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md)
  function now includes an `add_progressive_summary` parameter to
  optionally generate these plots by
  [@shawntz](https://github.com/shawntz) in
  [\#212](https://github.com/shawntz/eyeris/issues/212).

- **Gaze heatmap generation** is added for both runs and epoch groups
  within
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md),
  enabling visualizations of eye coordinate distributions, data quality,
  and participant attention when eye tracking and screen dimension data
  are available. A new
  [`plot_gaze_heatmap()`](https://eyeris.shawnschwartz.com/reference/plot_gaze_heatmap.md)
  function is introduced for this purpose by
  [@gustxsr](https://github.com/gustxsr) and
  [@shawntz](https://github.com/shawntz) in
  [\#213](https://github.com/shawntz/eyeris/issues/213).

- **Interactive HTML reports** now include a floating table of contents,
  enhancing navigation for longer reports by
  [@shawntz](https://github.com/shawntz) in
  [\#182](https://github.com/shawntz/eyeris/issues/182).

- The `html_report` parameter in
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  now **defaults to `TRUE`** by [@shawntz](https://github.com/shawntz)
  in [\#212](https://github.com/shawntz/eyeris/issues/212).

#### Core data processing functions:

- New [`bin()`](https://eyeris.shawnschwartz.com/reference/bin.md) and
  [`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md)
  functions are introduced for **pupil time series data processing**,
  including anti-aliasing filtering for downsampling and averaging for
  binning. Both functions are integrated into the
  [`eyeris::glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  pipeline by [@shawntz](https://github.com/shawntz) and
  [@mh105](https://github.com/mh105) in
  [\#204](https://github.com/shawntz/eyeris/issues/204).

- **Unique identifiers (`text_unique`)** are now added to event messages
  in
  [`eyeris::load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  to prevent duplicate event merges, and the
  [`merge_events_with_timeseries()`](https://eyeris.shawnschwartz.com/reference/merge_events_with_timeseries.md)
  function is updated to utilize these for correct event matching and
  merging by [@shawntz](https://github.com/shawntz) in
  [\#181](https://github.com/shawntz/eyeris/issues/181).

- **Confounds calculation and export** are integrated into the
  processing pipelines, with
  [`eyeris::summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md)
  now included in
  [`eyeris::glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  and
  [`eyeris::epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md)
  pipelines by [@shawntz](https://github.com/shawntz) in
  [\#182](https://github.com/shawntz/eyeris/issues/182).

#### Pipeline robustness and reproducibility:

- **Tracking of pipeline step provenance** is improved, adding the
  original function call and parameters for each step via a new
  `call_info` argument. This metadata is passed to `eyeris` functions to
  enhance reproducibility and debugging by
  [@shawntz](https://github.com/shawntz) in
  [\#209](https://github.com/shawntz/eyeris/issues/209).

- The
  [`eyelogger()`](https://eyeris.shawnschwartz.com/reference/eyelogger.md)
  utility documentation has been **updated with a new section in the
  README**, detailing its purpose, usage examples, parameters, and
  generated log files for improved reproducibility and debugging by
  [@shawntz](https://github.com/shawntz) in
  [\#214](https://github.com/shawntz/eyeris/issues/214).

### 🔧 Minor improvements and fixes

#### Robustness and error handling:

- **Enhanced plotting robustness** includes `tryCatch` blocks to handle
  errors and display informative messages in plots, and time series
  plotting now iterates over all intermediate steps to ensure plots are
  generated even with missing or incomplete data by
  [@shawntz](https://github.com/shawntz) in
  [\#181](https://github.com/shawntz/eyeris/issues/181),
  [\#183](https://github.com/shawntz/eyeris/issues/183).

- **Handling of missing valid samples** in random epoch plotting has
  been improved in
  [`plot.eyeris()`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md),
  adding warning messages and placeholder plots when no valid samples
  are found by [@shawntz](https://github.com/shawntz) in
  [\#181](https://github.com/shawntz/eyeris/issues/181),
  [\#183](https://github.com/shawntz/eyeris/issues/183).

- **Stricter validation checks** are added for the `prev_op` argument in
  the
  [`eyeris::zscore_pupil()`](https://eyeris.shawnschwartz.com/reference/zscore_pupil.md)
  internal function to catch missing, non-existent, or corrupted column
  names early, improving error handling by
  [@shawntz](https://github.com/shawntz) in
  [\#207](https://github.com/shawntz/eyeris/issues/207).

- **Validation for pupil data** in the
  [`eyeris::lpfilt_pupil()`](https://eyeris.shawnschwartz.com/reference/lpfilt_pupil.md)
  internal function ensures data is numeric, non-empty, and contains
  only finite values before filtering, preventing errors related to
  invalid matrix extents by [@shawntz](https://github.com/shawntz) in
  [\#210](https://github.com/shawntz/eyeris/issues/210).

- **Stricter checks for corrupted or empty `latest` pointers and output
  column names** are added in
  [`eyeris::pipeline_handler()`](https://eyeris.shawnschwartz.com/reference/pipeline_handler.md),
  improving error handling and transitioning operation calls to use
  `do.call` for flexible argument passing by
  [@shawntz](https://github.com/shawntz) in
  [\#211](https://github.com/shawntz/eyeris/issues/211).

- The
  [`eyeris::load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  function now **correctly sets the `latest` pointer** as a named list
  for *multi-block structures* and as a single value for *single block
  data*, enhancing multi-block support by
  [@shawntz](https://github.com/shawntz) in
  [\#211](https://github.com/shawntz/eyeris/issues/211).

- Fixes an **edge case** where `mad_val` is `NA` in the
  [`eyeris::detransient_pupil()`](https://eyeris.shawnschwartz.com/reference/detransient_pupil.md)
  internal function (occurring when all pupil data is `NA`), ensuring
  the original pupil data is returned unchanged and preventing
  comparison to `zero` when `mad_val` is `NA` by
  [@shawntz](https://github.com/shawntz) in
  [\#193](https://github.com/shawntz/eyeris/issues/193).

- **Baseline handling in
  [`eyeris::epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md)**
  is simplified by ***deprecating*** `calc_baseline` and
  `apply_baseline` in favor of a single `baseline` parameter, also
  resolving bugs related to baseline computation and event mismatches by
  [@shawntz](https://github.com/shawntz) in
  [\#177](https://github.com/shawntz/eyeris/issues/177).

#### Pipeline and data logic:

- The
  [`eyeris::glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  function has been **refactored to process each block** in the time
  series *individually* (except `load_asc`), improving modularity and
  ensuring correct error handling of multi-block data by
  [@shawntz](https://github.com/shawntz) in
  [\#189](https://github.com/shawntz/eyeris/issues/189).

- The calculation of `mean_gaze_distance_from_center_px` now **correctly
  uses the screen center coordinates (`cx`, `cy`)** instead of
  defaulting to the origin, ensuring the metric reflects distance from
  the actual screen center by [@shawntz](https://github.com/shawntz) in
  [\#199](https://github.com/shawntz/eyeris/issues/199).

- The
  [`eyeris::bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  function is **refactored to handle cases where no epochs are
  present**, preventing errors and unnecessary processing for users who
  want summary reports of the entire pupil time series without prior
  epoching by [@shawntz](https://github.com/shawntz) in
  [\#201](https://github.com/shawntz/eyeris/issues/201).

- The
  [`eyeris::bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  function now **properly allows manual specification of the `run_num`**
  for single-block data, while still auto-numbering multi-block files
  for improved naming consistency by
  [@shawntz](https://github.com/shawntz) in
  [\#203](https://github.com/shawntz/eyeris/issues/203).

- The **recalculation of epoched confounding variables** is now
  performed *when new epochs are created* by
  [@shawntz](https://github.com/shawntz) in
  [\#182](https://github.com/shawntz/eyeris/issues/182).

#### Documentation and internal clean-up:

- **Extensive documentation cleanup** has been performed, including
  fixing various spelling errors/typos in multiple function
  documentations by [@shawntz](https://github.com/shawntz) in
  [\#179](https://github.com/shawntz/eyeris/issues/179),
  [\#214](https://github.com/shawntz/eyeris/issues/214).

- The `pdf_report` parameter is
  [`eyeris::bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  has been ***deprecated*** in favor of `html_report = TRUE`, with
  associated removal of PDF rendering logic from the
  [`render_report()`](https://eyeris.shawnschwartz.com/reference/render_report.md)
  internal function by [@shawntz](https://github.com/shawntz) in
  [\#197](https://github.com/shawntz/eyeris/issues/197).

- Updates to `_pkgdown.yml`, `README`, `NAMESPACE`, and `R/zzz.R` to
  support new features and functionality, including exposing the
  [`eyeris_color_palette()`](https://eyeris.shawnschwartz.com/reference/eyeris_color_palette.md)
  and other global variables by [@shawntz](https://github.com/shawntz)
  in [\#214](https://github.com/shawntz/eyeris/issues/214).

- The structure of the `latest` field in mock data for unit tests was
  fixed to be a named list to ensure tests do not fail due to
  incorrectly specified data structures by
  [@shawntz](https://github.com/shawntz) in
  [\#208](https://github.com/shawntz/eyeris/issues/208).

- Added `MASS`, `viridis`, and `fields` package dependencies to
  `Imports` to support new gaze heatmaps functionality by
  [@shawntz](https://github.com/shawntz) in
  [\#213](https://github.com/shawntz/eyeris/issues/213).

------------------------------------------------------------------------

## eyeris 1.2.1 “Tree Trunks” ![Tree Trunks](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/tree-trunks.png)

CRAN release: 2025-06-13

### ✨ New features

- [`eyelogger()`](https://eyeris.shawnschwartz.com/reference/eyelogger.md):
  a new utility function to automatically capture and record R console
  output, errors, and the evaluated `eyeris` command into timestamped
  log files (`.out`, `.err`, and `.cmd`) for improved reproducibility,
  record keeping, and debugging by
  [@shawntz](https://github.com/shawntz) in
  [\#171](https://github.com/shawntz/eyeris/issues/171)

  #### `eyelogger()` usage example

  Logging your `eyeris` commands with
  [`eyelogger()`](https://eyeris.shawnschwartz.com/reference/eyelogger.md)
  is as simple as wrapping your command like this:

  ``` r

  eyelogger({
    glassbox(eyelink_asc_demo_dataset(), interactive_preview = FALSE)
  }, log_dir = file.path("~/Documents", "eyeris_logs"))
  ```

### 🐛 Bugs fixed

- Fixed edge case related to non-finite samples in `bidsify` epoch
  plotting function by [@gustxsr](https://github.com/gustxsr) in
  [\#166](https://github.com/shawntz/eyeris/issues/166)
- Fixed multi-block epoch bug by extracting data from blocks using their
  names rather than their indices by
  [@hyang336](https://github.com/hyang336) in
  [\#168](https://github.com/shawntz/eyeris/issues/168)
- Fixed plotting bug
  ([\#165](https://github.com/shawntz/eyeris/issues/165)) by
  [@shawntz](https://github.com/shawntz) in
  [\#169](https://github.com/shawntz/eyeris/issues/169)

### 🔧 Other minor improvements and fixes

- ENH: make plot color scheme more accessible/easier to read by
  [@shawntz](https://github.com/shawntz) in
  [\#169](https://github.com/shawntz/eyeris/issues/169)
- FF: missing x-axis labels on histograms in rendered reports by
  [@shawntz](https://github.com/shawntz) in
  [\#169](https://github.com/shawntz/eyeris/issues/169)
- NF: add detrend fitted values diagnostic plot to rendered reports by
  [@shawntz](https://github.com/shawntz) in
  [\#169](https://github.com/shawntz/eyeris/issues/169)

## eyeris 1.2.0 “Tree Trunks” ![Tree Trunks](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/tree-trunks.png)

CRAN release: 2025-06-07

### ✨ New features

- NF: Add vertical lines to plots to indicate where missing data (such
  as blinks and/or removed artifacts) are located in the time series
- ENH: Plotting now takes time ranges in seconds directly and does the
  conversion to row index using tracker Hz on the backend, making
  plotting in `eyeris` more intuitive. To demonstrate:

&nbsp;

    plot(eyeris_preproc,
      steps = c(1, 5),
      preview_window = c(0, max(eyeris_preproc$timeseries$block_1$time_secs))
    )

### 🐛 Bug fixes

- BF: `NA` slot offset in diagnostic plotting
  ([\#161](https://github.com/shawntz/eyeris/issues/161))
- BF: normalize physical machine time bins and convert to seconds /
  start at 0 seconds
  ([\#162](https://github.com/shawntz/eyeris/issues/162))
- BF: unit displayed on the x-axis doesn’t match the unit listed on the
  x-axis text label in plots
  ([\#162](https://github.com/shawntz/eyeris/issues/162))
- BF: minor issue where a manually specified block number in
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  wasn’t being translated to the column `block` in the resulting list of
  time series data frames
- BF: minor issue where setting `block = NULL` in
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  didn’t actually omit the block column values from the resulting data
  frames within the returned `eyeris` list object

### 🔧 Minor improvements and fixes

- RF: update package title to match that of the published bioRxiv
  preprint
- RF: Deprecated the `num_previews` parameter in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).
  - Please use `preview_n` instead.
- DOC: manually update citation file to include all authors + bioRxiv
  preprint DOI ([\#152](https://github.com/shawntz/eyeris/issues/152))
- DOC: incorrect URIs in `eyeris` documentation for
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  function ([\#160](https://github.com/shawntz/eyeris/issues/160))
- DOC: standardize default values for
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md)
  standalone \[previously `40ms`\] vs. in
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  \[now all `50ms`\]
  ([\#163](https://github.com/shawntz/eyeris/issues/163))
- DOC: updates to `pkgdown` documentation website:
  - Matching accent color theme with the `eyeris` hex logo
  - New nav bar items (buttons/links to access the bioRxiv preprint and
    socials)
  - Update funders and contributors list
  - Add funders disclaimer statement to the footer

## eyeris 1.1.0 “Princess Bubblegum” ![Princess Bubblegum](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/princess-bubblegum.png)

CRAN release: 2025-04-24

### ✨ New features

- NF: Simplify
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  caller with redesigned parameters that enhance continuity across all
  pipeline steps ([\#148](https://github.com/shawntz/eyeris/issues/148))

### 📚 Documentation

- DOC: Improved function documentation across the package and added more
  usage notes for clarity.
- DOC: Updated styling of documentation website.

### 🔧 Minor improvements and fixes

- ENH: Make histograms disabled by default when plotting an `eyeris`
  object ([\#156](https://github.com/shawntz/eyeris/issues/156)).
- RF: Deprecated the `confirm` parameter in
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md).
  - Please use `interactive_preview` instead.
- RF: Deprecated the `num_previews` parameter in
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md).
  - Please use `preview_n` instead.
- BF: Random seed assignment was not behaving as expected within the
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  pipeline.
- RF: Modify paths to documentation assets to fix broken links at build.

## eyeris 1.0.1 “Ice King” ![Ice King](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/ice-king.png)

This non-CRAN release patches a small handful of documentation-related
chores that have no direct impact on the functionality of `eyeris` for
the end user. The minor improvements and fixes contained within this
release will soon be bundled with a more substantial feature upgrade
when submitted to CRAN to reduce burden on the CRAN reviewers at this
time. Stay tuned!

### 🔧 Minor improvements and fixes ([\#159](https://github.com/shawntz/eyeris/issues/159))

- CHORE: fix duplicate `LICENSE` file issue in `release/**` branches
  ([\#145](https://github.com/shawntz/eyeris/issues/145))
- CHORE: update funders list on `DESCRIPTION`
  ([\#149](https://github.com/shawntz/eyeris/issues/149))
- CHORE: add DOI badge to `README`
  ([\#150](https://github.com/shawntz/eyeris/issues/150))
- CHORE: fix citation years in `DESCRIPTION` (i.e., put them in
  parentheses, per request of `CRAN` reviewer)
  ([\#151](https://github.com/shawntz/eyeris/issues/151))
- DOC: update `README` to include `CRAN` install code + option for
  [@latest](https://github.com/latest) `dev` branch via download with
  devtools/GitHub
  ([\#153](https://github.com/shawntz/eyeris/issues/153))
- DOC: fix version titles + urls on changelog webpage
  ([\#154](https://github.com/shawntz/eyeris/issues/154))
- CHORE: change pkgdown docs website deployment rules so that public
  webpage only updates on pushes to official release branches, and not
  the `dev` branch
  ([\#155](https://github.com/shawntz/eyeris/issues/155))
- DOC: add `CONTRIBUTING.md` guidelines file for GitHub
  ([\#157](https://github.com/shawntz/eyeris/issues/157))
- DOC: fix `/man/figures/...` image ref issues which is leading to
  broken links on the `R CRAN read-only` [GitHub mirror
  repo](https://github.com/cran/eyeris)
  ([\#158](https://github.com/shawntz/eyeris/issues/158))

## eyeris 1.0.0 “Ice King” ![Ice King](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/ice-king.png)

CRAN release: 2025-03-31

### 🎉 **First CRAN release!** ([\#144](https://github.com/shawntz/eyeris/issues/144))

This version marks the official launch of the `eyeris` package on CRAN.

### ✨ New features ([\#125](https://github.com/shawntz/eyeris/issues/125))

- Added example vignettes to demonstrate core functionality:
  - Preprocessing pipelines with
    [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  - Event-based epoching with
    [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md)
  - BIDS-style export with
    [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  - Custom pipeline extensions using
    [`pipeline_handler()`](https://eyeris.shawnschwartz.com/reference/pipeline_handler.md)

### 📚 Documentation ([\#125](https://github.com/shawntz/eyeris/issues/125))

- Improved function documentation across the package
- Added citation guidance and reproducibility tips

Thanks for checking out `eyeris`! 🧠

------------------------------------------------------------------------

**Pre-CRAN `dev` GitHub releases:**

## eyeris 0.1.1.9000 “Jake the Dog” ![Jake the Dog](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/jake.png)

### 🔧 Minor improvements and fixes

- FF ([\#115](https://github.com/shawntz/eyeris/issues/115)): add more
  aggressive handling of edge cases in
  [`eyeris::detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md)
  ([\#121](https://github.com/shawntz/eyeris/issues/121))
  - Specifically, situations where pupil data appear to have already
    undergone some type of online filtering directly from the EyeLink
    Host PC machine.
  - There is now detailed instructions on what to do if this exception
    is raised.
  - Furthermore, a new `mad_thresh` override parameter has been added to
    [`eyeris::detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md)
    for advanced users to override the `mad_thresh` computed property.
    *Note:* this new `mad_thresh` parameter defaults to `NULL` (and
    should pretty much always stay as such).
- FF ([\#122](https://github.com/shawntz/eyeris/issues/122)): fixed
  issue with incompatible unicode character in plot titles
  ([\#123](https://github.com/shawntz/eyeris/issues/123))

## eyeris 0.1.0.9000 “Jake the Dog” ![Jake the Dog](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/jake.png)

### 💥 Breaking changes

- NF ([\#10](https://github.com/shawntz/eyeris/issues/10)): add support
  for `.asc` files containing multiple recording segments within the
  same file ([\#120](https://github.com/shawntz/eyeris/issues/120))
  - There is a new `block` argument added to the
    [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
    function
  - The default setting is “auto”, which aims to automatically handle
    multiple recording segments within the same `.asc` file. We
    recommend using this default as this is likely the *safer choice*
    rather than assuming a single-block recording. **Furthermore, add
    downstream functions are intentionally designed to support any *N*
    number of blocks; using the “auto” setting automatically enables
    this support for `.asc` files containing single recording blocks by
    labeling the single recording session as `block_1`**
  - You can also manually specify a different block value (numeric)
    instead of “auto”, which can be helpful for multi-block experiments
    where each block/run was recorded to a separate `.asc` file. This is
    especially important to consider when running the downstream
    [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md) and
    [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
    functions so that derived files and summary reports are properly
    labeled with the correction block/run number
  - Currently, there is also a `NULL` option; however, this is likely
    going to just be a part of `beta` testing and will probably be
    removed in a future version given the foregoing 2 options should
    cover most (if not all) use cases

### ✨ New features

- NF: robust HTML and PDF output summary sidecar reports within
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  ([\#120](https://github.com/shawntz/eyeris/issues/120))
  - Here, reports are well-organized both by block/run and any specific
    event message epochs that have been processed using the
    [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md)
    function
  - For epoched data, we now provide a useful *interactive utility*
    within the epoch-specific HTML reports: you can now use your mouse
    and/or keyboard to swiftly navigate through an interactive gallery
    of pupil plot segments from every single trial from any given
    subject, right out-of-the-box! We hope this alleviates some of the
    complexities/roadblocks users might face when needing to perform
    manual inspections of their data for quality assurance and/or
    diagnostic purposes.

### 🔧 Minor improvements and fixes

- FF ([\#118](https://github.com/shawntz/eyeris/issues/118)): resolved a
  minor bug in the EyeLink EDF header `model` and `version` fields for
  data collected on newer EyeLink hardware/software
  ([\#120](https://github.com/shawntz/eyeris/issues/120))
- ENH: event epoching is now both **more robust** and **super fast**
  ([\#120](https://github.com/shawntz/eyeris/issues/120))
  - We have implemented more efficient data structures to swiftly handle
    large sets of pupil samples in rapid time
  - We have also added in better visual feedback within the console
    regarding epoching progress
  - Similarly, these added benefits coincide nicely with the new
    multi-block support
    ([\#10](https://github.com/shawntz/eyeris/issues/10))
- General bug fixes and enhancements to codebase and front-end UX
  ([\#120](https://github.com/shawntz/eyeris/issues/120))

## eyeris 0.0.0.9000 “Finn the Human” ![Finn the Human](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/finn.png)

- Initial beta release
