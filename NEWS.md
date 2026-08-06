# eyeris 3.3.0 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This minor release changes how missing pupil data is handled, broadens the range of eye trackers `eyeris` can ingest, and refreshes every diagnostic figure. The headline **breaking change** is that `interpolate()` no longer interpolates across arbitrarily long gaps: stretches of consecutive missing samples longer than the new `max_gap_ms` limit (default `250` ms, following Kret & Sjak-Shie, 2018) are now left as `NA`, and the downstream steps that cannot operate on missing data (`lpfilt()`, `downsample()`, `bin()`, `detrend()`) now work *around* those gaps and restore them, rather than being skipped. New features include `load_generic()`, a tracker-agnostic loader that lets non-EyeLink data enter the pipeline (with `glassbox()` now accepting a pre-loaded `eyeris` object); a sampling-uniformity guardrail plus an automatic `resample()` step for hardware that silently *drops* samples instead of zero-filling them; spline detrending via `detrend(method = "spline")`; and `simulate_eyeris()`/`sim_params()` for generating seeded, synthetic, pipeline-compatible pupil data with known ground truth. Bug fixes restore the fitted detrend coefficients on the `glassbox()` output, repair per-run epoch CSV writing in `bidsify()` for both multi-run objects and `run_num` overrides, and resolve a multi-block crash on R >= 4.2. Under the hood, all diagnostic plotting migrated from base graphics to [`reaborn`](https://reaborn.org)/`ggplot2`.

## 🚨 **Breaking changes & deprecations**

- **BREAKING (#295)**: `interpolate()` now enforces a maximum gap duration for linear interpolation via a new `max_gap_ms` parameter. Gaps of consecutive missing (`NA`) samples that are **longer** than `max_gap_ms` are now left as `NA` rather than interpolated over. The default of `250` ms follows the recommendation of Kret & Sjak-Shie (2018), who advise against interpolating across long gaps where linear interpolation is unlikely to reflect the true underlying pupil signal. Previously (eyeris ≤ 3.2.0), **all** gaps were interpolated regardless of duration. This is a change in default behavior and may affect downstream results; `eyeris` now prints a console notice (at most once per `glassbox()` run) when the limit is in effect. To restore the previous behavior of interpolating across all gaps, set `interpolate = list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` when calling `interpolate()` directly). The threshold can also be customized, e.g., `glassbox(interpolate = list(max_gap_ms = 100))`. The limit is specified in milliseconds and is internally converted to a number of samples using each recording's own sampling rate, so it behaves consistently across sampling frequencies, by @shawntz in #318.

- **ENH (#295)**: The downstream `glassbox()` steps that cannot operate on missing data — low-pass filtering (`lpfilt()`), downsampling (`downsample()`), binning (`bin()`), and detrending (`detrend()`) — now work *around* the gaps that `interpolate(max_gap_ms)` intentionally leaves as `NA`. They filter/resample/fit over the available data (filling temporarily where needed) and then restore the gaps as `NA`, so the long-gap `NA`s are preserved through to the final preprocessed output instead of causing these steps to be skipped. When interpolation has not been run upstream, the filter/resample steps still raise the usual "interpolate first" error, by @shawntz in #318.

- **ENH (#295)**: `lpfilt()` and `downsample()` now emit a warning when they filter over gaps longer than `max_gap_ms` (left as `NA` by `interpolate()`). Because a low-pass/anti-aliasing filter cannot run on missing data, these gaps are temporarily filled and then masked back to `NA`; the filter can therefore slightly bias the valid pupil samples immediately adjacent to each gap toward the (near-linear) interpolated values — analogous to the edge artifacts seen at the start/end of a filtered recording, but repeated at every long gap. The warning lets you decide whether to disable filtering and/or downsampling (`lpfilt = FALSE` and/or `downsample = FALSE`) to avoid introducing this systematic bias, at the cost of retaining more high-frequency content. The console warning fires at most once per `glassbox()` run, and the same information is recorded as a **"Data Quality Notes"** section in the HTML preprocessing report so it is captured as part of the data provenance, by @shawntz in #318.

## 🚀 New features

- **NF**: **`detrend()` now supports spline detrending via a new `method` argument.** Detrending previously only removed a straight-line (linear) trend, which cannot capture the slow, *nonlinear* drift often present in longer recordings. `detrend()` now accepts `method = "linear"` (the default, preserving the previous behavior of regressing pupil size on time) or `method = "spline"`, which removes a smooth, potentially nonlinear trend by fitting a natural cubic spline basis of time (`splines::ns(time, df = spline_df)`). The flexibility of the fitted spline is controlled by the new `spline_df` argument (degrees of freedom; default `5`). Both models return the fitted trend and the residuals exactly as before, so all downstream steps, plots, and reports are unchanged aside from the trend shape. Enable it in the pipeline with `glassbox(detrend = list(method = "spline"))` (optionally `spline_df = ...`), or call `detrend(method = "spline")` directly, by @shawntz in #349.

- **NF (#299)**: **Added `load_generic()`, a tracker-agnostic data loader for non-EyeLink eye trackers.** Until now, `load_asc()` (for SR Research EyeLink `.asc` files) was the only implemented loader, which prevented researchers using other eye trackers from running their data through `eyeris`. `load_generic()` accepts three standardized data frames — `pupil` (raw samples: timestamp + pupil size, optionally gaze `eye_x`/`eye_y`), `events` (timestamp + message text), and `blinks` (blink start/end intervals) — plus an optional fourth `gaze` data frame for the less common case where gaze is exported separately from pupil size (joined onto `pupil` by timestamp). These are assembled into the exact same `eyeris` S3 object that `load_asc()` produces, making the result a drop-in input to `glassbox()`, `epoch()`, `bidsify()`, and the plotting methods. Column names are configurable via the `mapping` argument; sampling rate is inferred from the timestamps when not supplied; timestamps may be provided in milliseconds (default) or seconds (`time_unit = "s"`); and `block`/`eye`/`pupil_type`/screen-dimension metadata mirror `load_asc()`. Native readers for specific tracker formats can now be layered on top of this function incrementally, by @shawntz in #311.

- **NF**: **`glassbox()` now accepts a pre-loaded `eyeris` object** (e.g., from `load_generic()`), automatically skipping the file-loading step and running the remaining pipeline on the object directly. This makes `load_generic(...) |> glassbox()` work as expected, and also means an already-loaded `load_asc()` object piped into `glassbox()` now processes correctly instead of raising an error, by @shawntz in #311.

- **ENH (#300)**: Added a hardware-quirk guardrail that detects non-uniform sampling intervals in the input timeseries. Different eye trackers behave differently when pupil data is missing: EyeLink zero-fills, but some hardware silently *drops* samples, leaving gaps in the otherwise evenly spaced time vector. Because downstream pipeline steps (e.g., `detransient()`, `lpfilt()`, `downsample()`) assume a fixed sampling rate, such gaps can silently distort results. The new internal `check_uniform_sampling_intervals()` helper infers the expected inter-sample interval from the data (robust to dropped samples and sub-millisecond timestamp rounding), checks each recording segment independently so that legitimate between-block gaps are not flagged, and emits an informative warning estimating the number of dropped samples when irregular intervals are detected. It also cross-checks the data-derived interval against the device's reported sampling rate to catch *systematic* dropout (e.g., every Nth sample missing), which leaves a uniform but coarser grid that gap detection alone cannot see. The check is wired into `load_asc()` so the quirk is surfaced early, before any preprocessing begins, by @shawntz in #308.

- **ENH (#320)**: Added an automatic `resample()` step that repairs the **time axis** of recordings whose hardware *drops* samples (instead of zero-filling) when pupil data is missing — the dropped-sample quirk surfaced by the #300 guardrail. When `glassbox()` detects irregular sampling intervals (gated on the robust `check_uniform_sampling_intervals()` detector from #300), it places each block onto the expected uniform sampling grid in two stages: (1) it anchors the grid on the first *reliable* regular interval (rather than the first timestamp), so early sub-period timing jitter doesn't offset the whole grid, and back-extends the grid to cover any earlier samples; then (2) it resamples the observed data onto that grid — linearly interpolating across short/jittered intervals, and inserting `NA` rows for any interval longer than the expected period (dropped samples), flagged in a new logical `is_resampled` column and left for the existing `interpolate()` step to fill. Because a gap is now represented as explicit missing data (never fabricated values), `interpolate()` decides how much of a missing span to fill according to its own policy (see the #295 `max_gap_ms` limit). This turns the "dropped-sample" problem into the ordinary "missing-value" problem the rest of the pipeline already handles. It runs **automatically and by default**, but only when needed: it is a guaranteed no-op for already-uniform data (e.g., EyeLink) and deliberately leaves high-rate recordings that report integer-millisecond timestamps untouched (so it never collapses genuine sub-millisecond samples). Opt out with `glassbox(resample = FALSE)`. `summarize_confounds()` now also reports `n_resampled`/`prop_resampled` per block, by @shawntz in #320.

- **NF**: **Added `simulate_eyeris()` and `sim_params()`, a seeded synthetic pupil data generator with known ground truth.** `eyeris` previously had no way to produce synthetic pupil data, making it difficult to write self-contained tests, build reproducible documentation examples, or demonstrate how a preprocessing step behaves without shipping and loading a real recording. `sim_params()` builds the signal-model parameter list, with every component independently toggle-able and validated: tonic baseline, slow drift, hippus, Hoeks & Levelt phasic responses, blinks (missing cores plus occlusion flank spikes), transient spikes, broadband noise, and optional line noise. `simulate_eyeris()` then generates the signal under a confined RNG (`withr::with_seed()`, so the global `.Random.seed` is left untouched) and delegates to the same internal object constructor used by `load_asc()`, so the returned `eyeris` object is structurally identical to a real recording and flows through the entire pipeline unchanged (`deblink()` … `zscore()`, `glassbox()`, `plot()`, `epoch()`, `summarize_confounds()`). This addition is purely additive and changes no existing behavior, by @shawntz in #351.

## 🐛 Bugs fixed

- **FF**: Fixed `glassbox(detrend = TRUE)` silently dropping the fitted detrend coefficients. The per-block detrend coefficients are computed inside `pipeline_handler()` and stored on the internal per-block object, but `glassbox()`'s block-recombine step copied back the timeseries, `params`, decimated sample rate, and pre-decimation data — never `$detrend_coefs`. As a result, `$detrend_coefs` was always absent from the object returned by `glassbox()`, even though it was correctly populated when calling `detrend()` directly. The recombine step (in both the main and per-eye/binocular paths) now preserves `$detrend_coefs` per block, so the fitted intercept/slope are available on the `glassbox()` output for provenance and inspection. It also clears any `$detrend_coefs` inherited from a pre-loaded input object at the start of each run, so the returned object retains only the coefficients produced in that run (and none survive when detrending is disabled or fails). The `detrend_fitted_values` column that drives the detrend diagnostic plots/reports was already preserved and is unaffected, by @shawntz in #350.

- **FF**: Fixed an error in `summarize_confounds()` (via `get_block_numbers()`) that aborted with `the condition has length > 1` when processing multi-block data on R ≥ 4.2. The internal `is.na()` guard now handles vectors of block numbers, so multi-block recordings flow through the confound summary step correctly. This latent bug was surfaced by the new multi-block `load_generic()` tests, by @shawntz in #311.

- **FF**: Fixed multi-run epoch CSV files (`*_desc-preproc_pupil_epoch-<label>.csv`) not being written for each run. When a subject's data contained multiple blocks (runs), `bidsify()` built the per-epoch preprocessed output filename from the `run_num` argument — which is intentionally ignored (`NULL`) for multi-block objects — and packed the epoch label directly into the `desc` field without the `run-` and `epoch-` BIDS entities. As a result, every run wrote to the *same* filename and silently overwrote the previous one (last-run-wins), so the expected `..._run-NN_desc-preproc_pupil_epoch-<label>.csv` files never materialized for multi-run inputs. The multi-run writer now derives the run number from each block and routes the epoch label through the BIDS filename builder, exactly mirroring the single-run path. The DuckDB/parquet outputs were already keyed by the correct per-run number and are unchanged — only the CSV filenames were affected. As part of the same fix, the multi-run raw-timeseries writers now key the `run-NN` token off each block's own block number (rather than a positional index, which mislabeled runs when blocks were not numbered sequentially), and several internal epoch/baseline metadata helpers no longer receive `verbose` in their `block_name` argument position (which crashed `bidsify()` under `verbose = FALSE` whenever epochs were present), by @alicexue and @shawntz in #322.

- **FF**: Fixed the `run_num` override being ignored for epoch CSV and per-run `metadata.json` outputs when preprocessing separate single-block `.asc` files into a shared `bids_dir`. In that workflow the run number is not forced at `load_asc()`, so every file loads as `block_1` and the per-file `run_num` is instead passed to `bidsify()`; two outputs ignored the `block_1` → `block_<run_num>` rename and collapsed onto `run-01`, so every run's `*_desc-preproc_pupil_epoch-<label>.csv` overwrote itself and every run's `*_metadata.json` ended up holding the last run's `source_file`/`call_stack`. The epoch-block rename now writes the renamed block key back onto the epoch list (previously it updated only a local copy of the block-name vector), and per-run metadata regeneration in `make_report()` is scoped to the blocks present in the current object rather than every `run-XX` directory found on disk — so sibling runs written in earlier loop iterations keep their own metadata and are only read back for the combined report, by @shawntz in #334.

## 🔧 Under the hood

- **ENH**: Migrated all diagnostic plotting from base graphics to [`reaborn`](https://reaborn.org) — an R port of the 'Python' 'seaborn' library built on 'ggplot2'. `plot.eyeris()`, the gaze heatmap (`plot_gaze_heatmap()`), the binocular-correlation panels (`plot_binocular_correlation()`), the detrend overlay, the pupil-size distribution histograms, the progressive-summary report plot, and the per-epoch gallery figures are now rendered with `reaborn` and composited with `patchwork`, giving the interactive HTML reports a consistent, publication-quality seaborn aesthetic while preserving eyeris's colour palette and axis labels. Each plotting function now builds `ggplot` objects and prints them to the active graphics device, so the existing report/gallery capture flow (`png()`/`jpeg()` → draw → `dev.off()`) is unchanged and every public plotting API keeps its signature. Multi-panel time series previews are composited with `patchwork` (replacing base `par(mfrow)` layouts), missing-sample gaps are shaded as contiguous regions, and the binocular scatter panels overlay a dashed identity line. Signal-processing frequency-response (Bode) plots for `lpfilt()`/`downsample()` intentionally remain on `gsignal`, as they fall outside the statistical-visualization scope of `reaborn`. Adds `reaborn`, `ggplot2`, and `patchwork` to `Imports` and drops the now-unused `fields`, by @shawntz in #338.

## 📚 Documentation

- **DOC**: Added a *Preprocessing Multiple Runs in Separate Files* vignette covering the common case where each run of a session is recorded to its own `.asc` file rather than as multiple blocks within one file, including the `run_num` override needed to keep runs distinct in a shared `bids_dir`, an equivalent `run_num` example, and a documented bridge function for multi-block data that `eyeris` cannot auto-separate. Demo data shown in the vignette was anonymized, by @shawntz in #324, #330, #331, and #336.

- **DOC**: Added a step-by-step breakdown of the `glassbox()` pipeline to the function documentation, so the default recipe and the order in which each step runs are visible directly from `?glassbox`, by @shawntz in #332.

- **DOC**: Added a "Built for the Age of AI Coding Agents" article to the package website describing how `eyeris`'s modular, introspectable design supports agent-assisted analysis workflows, by @shawntz in #337.

- **DOC**: Pointed the package and website URLs at <https://eyeris.shawnschwartz.com/>, and fixed stale BIDS output filenames in the README and vignettes so the documented directory layout matches what `bidsify()` actually writes, by @shawntz in #340 and #323.

# eyeris 3.2.0 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This release fixes several correctness and data-integrity bugs and adds new transparency and reproducibility tooling. Bug fixes resolve a report/figure collision when different task names shared the same run number within a subject/session, misleading diagnostic plots for pipeline steps that precede downsampling/binning, and silent data loss in `eyeris_db_read()`/`eyeris_db_collect()` when table schemas diverged across `eyeris` versions. New features add a "percent data lost" annotation to the HTML report, expose a `prop_missing`/`n_missing` missing-data column at the block and trial levels for user-defined filtering, and introduce `boilerplate()`, an fMRIPrep-style generator that auto-writes copy-and-paste-ready methods text from the parameters captured in your pipeline. Documentation now cross-references every modular preprocessing function to a complete end-to-end reference pipeline.

## 🐛 Bugs fixed

- **FF (#293)**: Fixed a conflict where running `bidsify()` for two different tasks that share the same run number (e.g., `task-study_run-01` and `task-test_run-01`) under the same subject/session caused the second task to silently overwrite the first task's HTML report and figures. BIDS allows different task names to pair with the same run number within a block, but the report (`sub-xyz.html`), the `source/figures/run-XX/` directories, and the per-run `source/logs/run-XX_metadata.json` were keyed by run number alone (and the report file by subject alone), so they collided across tasks. The combination of `task` + `run` is now treated as the unique key: figure directories and their figures are named `task-{task}_run-XX[...]`, the preprocessing report is named `sub-{sub}_task-{task}[...].html`, and the epoch gallery report and zip files are likewise task-namespaced. A new internal `make_run_dir_name()` helper is the single source of truth shared by every writer and reader. The underlying data files (CSV/parquet/database) already included the task entity and are unchanged. As a side effect this also corrects a latent mismatch where the gaze-heatmap filename and binocular-correlation plots ignored the `run_num` override. **Note:** for single-task workflows this changes the on-disk report filename and figure directory names (now task-namespaced); regenerate reports to pick up the new layout, by @shawntz and @alicexue in #293.

- **FF (#294)**: Fixed misleading diagnostic plots for pipeline steps that precede downsampling/binning. When `downsample` (or `bin`) was enabled, the working time series retained only the decimated samples, so diagnostic plots for earlier steps (e.g., `deblink`) were rendered at the decimated rate — making intact data appear largely absent. The full-resolution (pre-decimation) time series is now preserved in `eyeris$timeseries_pre_decimation` when a `downsample()`/`bin()` step runs, and `plot.eyeris()` (plus the progressive-summary report plot) now renders each step at the appropriate resolution: steps preceding decimation use the original full-resolution data, while the decimation step and any subsequent steps use the decimated data, by @shawntz and @alicexue in #294.

- **FF**: Fixed blank "raw" pupil-size histograms in multi-run diagnostic reports. When `plot_distributions = TRUE`, `plot_pupil_distribution()` outlined every histogram bar in white (`border = "white"`). For the raw step — whose wide, outlier-laden spread yields the most Freedman-Diaconis bins — the white outline completely covered the (now very thin) bar fills, so the histogram rendered as a blank white panel. Whether a given run crossed that threshold depended on its data spread (i.e., its number of bins), which is why some runs in a multi-run report showed a normal raw histogram while others appeared empty. The bar outline is now dropped once there are too many bars so the distribution always stays visible, and the helper additionally guards against empty/all-`NA`/near-constant inputs (drawing an informative panel or falling back to default breaks) instead of raising an error, by @shawntz and @alicexue in #319.

- **FF**: Fixed a crash that broke *all* multi-block (multi-run) diagnostic plotting and HTML report generation on R `>= 4.2`. `get_block_numbers()` guarded its return value with `if (is.na(block_nums))`, but for a multi-block object `block_nums` is a vector (one entry per block), so the condition had length `> 1` — a hard error on modern R (`"the condition has length > 1"`). The fallback is now applied element-wise, so multi-block objects return one number per block; this path is exercised by both `plot.eyeris()` and `bidsify()`, by @shawntz and @alicexue in #319.

- **FF (#310)**: Made `eyeris_db_read()` (and, by extension, `eyeris_db_collect()`) tolerant of tables whose column schemas diverge across `eyeris` versions. Previously, the function built a naive `SELECT * FROM t1 UNION ALL SELECT * FROM t2 ...` across every table matching a `data_type`, which requires every matched table to share an identical column schema. In an incremental study that spans an `eyeris` upgrade — e.g., older `run_confounds_*` / `confounds_events_*` tables written before the `n_missing` / `prop_missing` columns were added, collected alongside data from a newer version — DuckDB raised `Binder Error: Set operations can only apply to expressions with the same number of result columns`. Because the read was wrapped in `tryCatch()`, this surfaced as a warning and an empty `data.frame()`, i.e. **silent data loss**. `eyeris_db_read()` now computes the union of columns across all matching tables and projects each table onto a consistent, ordered column list (filling absent columns with `NULL`/`NA`) before the union, and `eyeris_db_collect()` reconciles epoch-label results with a fill-aware bind. Identifiers are SQL-escaped before quoting so column or table names are handled safely, and an informational message is logged whenever divergent schemas are detected and aligned, by @shawntz in #310.

## 🚀 New features

- **ENH (#296)**: Added a "percent data lost" annotation to the timeseries visualizations in the HTML report. Each run in the *Preprocessed Data Previews* section now displays the percent of samples in the raw pupil timeseries that are invalid (missing/during a blink, or off-screen), surfacing data loss directly in the report to reinforce workflow transparency. The metric reuses the canonical `prop_invalid` value from `summarize_confounds()` when available and falls back to computing missingness directly from the raw timeseries otherwise, by @shawntz in #296.

- **NF (#297)**: Exposed a missing-data column for user-defined filtering. `summarize_confounds()` (and the `glassbox()` pipeline that calls it) now reports the proportion of missing (`NA`) pupil samples as `prop_missing` (ranging `0`–`1`; multiply by `100` for a percentage), alongside its raw count `n_missing`. These are computed at two levels so you can choose the granularity appropriate to your design: per recording **block** (`confounds$unepoched_timeseries`, exported as `run_confounds`) and per epoched event/**trial** (`confounds$epoched_timeseries`, exported as `confounds_events`). `eyeris` intentionally does not enforce a fixed missing-data exclusion cutoff; instead, `prop_missing` is surfaced so users can define their own exclusion thresholds at whichever level (trial, epoch, or block) suits their study. Unlike `prop_invalid`, `prop_missing` reflects only `NA`/dropout samples and does not fold in blink or off-screen flags. The database guide vignette was updated with block- and trial-level filtering examples, by @shawntz in #297.

  > **Upgrade note**: because this adds columns to the `run_confounds` and `confounds_events` tables, a project `DuckDB` that already contains confounds tables written by eyeris `<= 3.1.0` should be regenerated before collecting it (e.g., via `eyeris_db_collect()`) alongside data written by this version. Mixing the old and new confounds schemas in the same database is not currently supported by `eyeris_db_read()`.

- **NEW (#302)**: Added `boilerplate()`, an fMRIPrep-style methods-text generator that auto-writes a reproducible, copy-and-paste-ready Markdown description of the exact preprocessing workflow that was run, generated directly from the parameters captured in `eyeris$params`. The generator walks the pipeline steps in canonical order (`load_asc` → `deblink` → `detransient` → `interpolate` → `lpfilt` → `downsample`/`bin` → `detrend` → `zscore` → `epoch`), substitutes in the actual parameter values used, and handles multi-block (multi-run), multi-step, custom-extension, and binocular pipelines. The boilerplate is now embedded in every diagnostic HTML report (new "Reproducible Methods Boilerplate" section) and written to `derivatives/.../source/logs/methods_boilerplate.md` alongside the per-run `.json` metadata sidecars it references. The generated text is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/), with an explicit note letting users know it is safe to paste the Markdown content directly into their manuscript's methods section as long as they cite `eyeris` to provide the required attribution, by @shawntz in #302.

## 📚 Documentation

- **DOC (#298)**: Added cross-references from every modular preprocessing function (`load_asc()`, `deblink()`, `detransient()`, `interpolate()`, `lpfilt()`, `downsample()`, `bin()`, `detrend()`, and `zscore()`) to a complete, end-to-end reference pipeline that demonstrates how all functions are chained together in practice. Each function's help page now points readers to the "Building Blocks Under the Hood" section of the *Anatomy of an `eyeris` Object* vignette, which was expanded into an explicit, fully-annotated reference that maps each step one-to-one to the default `glassbox()` recipe. The *Complete Pupillometry Pipeline Walkthrough* vignette now links to that reference from a new "Advanced: Building the Pipeline Manually" section, by @shawntz.

# eyeris 3.1.0 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This minor release delivers several robustness and stability improvements, fixing memory issues during HTML report rendering, correcting epoch plot compression after downsampling, and improving documentation accuracy.

## 🐛 Bugs fixed

- **FF (#275)**: Fixed HTML rendering failure on headless Linux systems with older pandoc versions (e.g., v2.7.3) caused by a missing sticker path. Previously, the blanket exclusion of `^inst/figures$` in `.Rbuildignore` caused `system.file("figures", "sticker.png", package = "eyeris")` to return `""`, producing `<img src=''>` markup that older pandoc versions cannot render. Replaced the blanket exclusion with targeted file-level exclusions for large demo assets (GIFs, character images, annotated example screenshots) while keeping only the essential `sticker.png` (21KB) in the installed package. Net package size increase is +21KB, well within CRAN's 5MB limit, by @shawntz in #286.

- **FF (#278)**: Fixed pandoc out-of-memory (`exit 137`) errors when rendering HTML reports with large epoch event data. `format_call_stack()` was calling `deparse()` on all parameters including large epoch event lists (thousands of rows), generating strings that consumed gigabytes of memory. Added `should_omit_parameter()` helper to detect and filter epoch-related parameters containing complex objects (lists/data.frames) before deparsing. Large parameters are now displayed as `<omitted>` in the call stack while scalar values (e.g., `epoch_length = 100`) are preserved. Added comprehensive test coverage for parameter filtering, case-insensitive matching, and scalar preservation, by @shawntz in #280.

- **FF (#291)**: Fixed downsampled epoch data being visually compressed in HTML plots. When downsampling was applied in the Glassbox pipeline, epoch plots showed 5-second windows compressed to ~0.5 seconds (a 10x compression). The root cause was that `epoch_pupil()` used the original sampling rate (`info$sample.rate`) instead of the decimated rate (`decimated.sample.rate`) when calculating epoch timebins, causing sample counts to be divided by the wrong Hz value. Updated `epoch_pupil()` to check for `decimated.sample.rate` first before falling back to the original rate. X-axis labels and CSV output were already correct; this fix applies the same logic to the plot rendering, by @shawntz and @alicexue in #292.

## 🔧 Minor improvements and fixes

- **FF (#287)**: Fixed misleading `load_asc()` usage pattern in the database guide documentation, where examples incorrectly instructed users to call `load_asc()` and pipe the result into `glassbox()`. This caused errors because `eyeris` expects a file path string to be passed directly into the pipeline; `load_asc()` is handled internally. Updated examples to show the correct usage pattern and added a runtime error in `glassbox()` that detects when a user passes an `eyeris` object instead of a file path, with a helpful message explaining the correct approach, by @shawntz and @alicexue in #288.

## 📚 Documentation

- **DOC**: Added a system requirements section to the README with platform-specific guidance for installing runtime dependencies, by @shawntz in #282.

# eyeris 3.0.1 "Lumpy Space Princess" ![Lumpy Space Princess](https://raw.githubusercontent.com/shawntz/eyeris/refs/heads/dev/inst/figures/adventure-time/lsp.png){width="50"}

This patch release improves dependency management for Arrow and DuckDB to prevent installation issues on macOS and other platforms.

## 🔧 Dependency management improvements

- **ENH**: Moved `arrow` from Imports to Suggests to prevent hanging installation issues on macOS. The arrow package requires system dependencies (pkg-config, cmake, apache-arrow via Homebrew on macOS) that could cause installation to hang indefinitely when building from source. eyeris now gracefully falls back to DuckDB for parquet operations when arrow is not available, with informative installation instructions provided via `check_arrow()` helper function, by @shawntz in #273.

- **ENH**: Added comprehensive installation guidance for Arrow and DuckDB dependencies. New `check_arrow()` helper function provides platform-specific installation instructions (macOS, Linux, Windows) with detailed steps for installing required system dependencies. Startup messages now inform users about missing optional dependencies and point to installation documentation, by @shawntz in #273.

## 📚 Documentation

- **DOC**: Updated README with detailed instructions for installing Arrow and DuckDB dependencies across different platforms, including Homebrew setup for macOS, system package installation for Linux distributions, and notes about when these packages are required, by @shawntz in #273.

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
