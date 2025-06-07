# eyeris 1.2.0

## ✨ New features
* NF: Add vertical lines to plots to indicate where missing data (such as blinks and/or removed artifacts) are located in the time series
* ENH: Plotting now takes time ranges in seconds directly and does the conversion to row index using tracker Hz on the backend, making plotting in `eyeris` more intuitive. To demonstrate:
```
plot(eyeris_preproc,
  steps = c(1, 5),
  preview_window = c(0, max(eyeris_preproc$timeseries$block_1$time_secs))
)
```

## 🐛 Bug fixes
* BF: `NA` slot offset in diagnostic plotting (#161)
* BF: normalize physical machine time bins and convert to seconds / start at 0 seconds (#162)
* BF: unit displayed on the x-axis doesn't match the unit listed on the x-axis text label in plots (#162)
* BF: minor issue where a manually specified block number in `load_asc()` wasn't being translated to the column `block` in the resulting list of time series data frames
* BF: minor issue where setting `block = NULL` in `load_asc()` didn't actually omit the block column values from the resulting data frames within the returned `eyeris` list object

## 🔧 Minor improvements and fixes
* RF: update package title to match that of the published bioRxiv preprint
* RF: Deprecated the `num_previews` parameter in `plot()`.
  * Please use `preview_n` instead.
* DOC: manually update citation file to include all authors + bioRxiv preprint DOI (#152)
* DOC: incorrect URIs in `eyeris` documentation for `load_asc()` function (#160)
* DOC: standardize default values for `deblink()` standalone [previously `40ms`] vs. in `glassbox()` [now all `50ms`] (#163)
* DOC: updates to `pkgdown` documentation website:
  * Matching accent color theme with the `eyeris` hex logo
  * New nav bar items (buttons/links to access the bioRxiv preprint and socials)
  * Update funders and contributors list
  * Add funders disclaimer statement to the footer

# eyeris 1.1.0

## ✨ New features
* NF: Simplify `glassbox()` caller with redesigned parameters that enhance continuity across all pipeline steps (#148)

## 📚 Documentation
* DOC: Improved function documentation across the package and added more usage notes for clarity.
* DOC: Updated styling of documentation website.

## 🔧 Minor improvements and fixes
* ENH: Make histograms disabled by default when plotting an `eyeris` object (#156).
* RF: Deprecated the `confirm` parameter in `glassbox()`.
  * Please use `interactive_preview` instead.
* RF: Deprecated the `num_previews` parameter in `glassbox()`.
  * Please use `preview_n` instead.
* BF: Random seed assignment was not behaving as expcted within the `glassbox()` pipeline.
* RF: Modify paths to documentation assets to fix broken links at build.

# eyeris 1.0.1

This non-CRAN release patches a small handful of documentation-related chores that have no direct impact on the functionality of `eyeris` for the end user. The minor improvements and fixes contained within this release will soon be bundled with a more substantial feature upgrade when submitted to CRAN to reduce burden on the CRAN reviewers at this time. Stay tuned!

## 🔧 Minor improvements and fixes (#159)
* CHORE: fix duplicate `LICENSE` file issue in `release/**` branches (#145)
* CHORE: update funders list on `DESCRIPTION` (#149)
* CHORE: add DOI badge to `README` (#150)
* CHORE: fix citation years in `DESCRIPTION` (i.e., put them in parentheses, per request of `CRAN` reviewer) (#151)
* DOC: update `README` to include `CRAN` install code + option for @latest `dev` branch via download with devtools/GitHub (#153)
* DOC: fix version titles + urls on changelog webpage (#154)
* CHORE: change pkgdown docs website deployment rules so that public webpage only updates on pushes to official release branches, and not the `dev` branch (#155)
* DOC: add `CONTRIBUTING.md` guidelines file for GitHub (#157)
* DOC: fix `/man/figures/...` image ref issues which is leading to broken links on the `R CRAN read-only` [GitHub mirror repo](https://github.com/cran/eyeris) (#158)

# eyeris 1.0.0

## 🎉 **First CRAN release!** (#144)

This version marks the official launch of the `eyeris` package on CRAN.

## ✨ New features (#125)
* Added example vignettes to demonstrate core functionality:
  * Preprocessing pipelines with `glassbox()`
  * Event-based epoching with `epoch()`
  * BIDS-style export with `bidsify()`
  * Custom pipeline extensions using `pipeline_handler()`

## 📚 Documentation (#125)
* Improved function documentation across the package
* Added citation guidance and reproducibility tips

Thanks for checking out `eyeris`! 🧠👁️

# eyeris 0.1.0.9001

## 🔧 Minor improvements and fixes
* FF (#115): add more aggressive handling of edge cases in `eyeris::detransient()` (#121)
  * Specifically, situations where pupil data appear to have already undergone some type of online filtering directly from the EyeLink Host PC machine.
  * There is now detailed instructions on what to do if this exception is raised.
  * Furthermore, a new `mad_thresh` override parameter has been added to `eyeris::detransient()` for advanced users to override the `mad_thresh` computed property. *Note:* this new `mad_thresh` parameter defaults to `NULL` (and should pretty much always stay as such).
* FF (#122): fixed issue with incompatible unicode character in plot titles (#123)

# eyeris 0.1.0.9000

## 💥 Breaking changes
* NF (#10): add support for `.asc` files containing multiple recording segments within the same file (#120)
  * There is a new `block` argument added to the `load_asc()` function
  * The default setting is "auto", which aims to automatically handle multiple recording segments within the same `.asc` file. We recommend using this default as this is likely the *safer choice* rather than assuming a single-block recording. **Furthermore, add downstream functions are intentionally designed to support any _N_ number of blocks; using the "auto" setting automatically enables this support for `.asc` files containing single recording blocks by labeling the single recording session as `block_1`**
  * You can also manually specify a different block value (numeric) instead of "auto", which can be helpful for multi-block experiments where each block/run was recorded to a separate `.asc` file. This is especially important to consider when running the downstream `epoch()` and `bidsify()` functions so that derived files and summary reports are properly labeled with the correction block/run number
  * Currently, there is also a `NULL` option; however, this is likely going to just be a part of `beta` testing and will probably be removed in a future version given the foregoing 2 options should cover most (if not all) use cases
  
## ✨ New features
* NF: robust HTML and PDF output summary sidecar reports within `bidsify()` (#120)
  * Here, reports are well-organized both by block/run and any specific event message epochs that have been processed using the `epoch()` function
  * For epoched data, we now provide a useful *interactive utility* within the epoch-specific HTML reports: you can now use your mouse and/or keyboard to swiftly navigate through an interactive gallery of pupil plot segments from every single trial from any given subject, right out-of-the-box! We hope this alleviates some of the complexities/roadblocks users might face when needing to perform manual inspections of their data for quality assurance and/or diagnostic purposes.

## 🔧 Minor improvements and fixes
* FF (#118): resolved a minor bug in the EyeLink EDF header `model` and `version` fields for data collected on newer EyeLink hardware/software (#120)
* ENH: event epoching is now both **more robust** and **super fast** (#120)
  * We have implemented more efficient data structures to swiftly handle large sets of pupil samples in rapid time
  * We have also added in better visual feedback within the console regarding epoching progress
  * Similarly, these added benefits coincide nicely with the new multi-block support (#10)
* General bug fixes and enhancements to codebase and front-end UX (#120)

# eyeris 0.0.0.9000

* Initial beta release

---

## Commit message tags reference:
* **BF:** bug fix (in the *release* branch)
* **FF:** feature fix (bug fixes in the *dev* branch)
* **RF:** refactoring
* **NF:** new feature
* **ENH:** enhancement
* **DOC:** for documentation-related updates and changes
* **TEST:** for commits that add or change unit tests
* **CHORE:** like **RF** but usually less important changes
