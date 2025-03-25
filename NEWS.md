# eyeris 1.0.0

---

# eyeris v1.0.0
<small>`Pending Review`</small>

🎉 **First CRAN release!**

This version marks the official launch of the `eyeris` package on CRAN.

## ✨ New Features (#125)
* Added example vignettes to demonstrate core functionality:
  * Preprocessing pipelines with `glassbox()`
  * Event-based epoching with `epoch()`
  * BIDS-style export with `bidsify()`
  * Custom pipeline extensions using `pipeline_handler()`

## 📚 Documentation (#125)
* Improved function documentation across the package
* Added citation guidance and reproducibility tips

Thanks for checking out `eyeris`! 🧠👁️

# eyeris v0.1.0.9001
<small>`February 26th, 2025`</small>

## Minor improvements and fixes
* FF (#115): add more aggressive handling of edge cases in `eyeris::detransient()` (#121)
  * Specifically, situations where pupil data appear to have already undergone some type of online filtering directly from the EyeLink Host PC machine.
  * There is now detailed instructions on what to do if this exception is raised.
  * Furthermore, a new `mad_thresh` override parameter has been added to `eyeris::detransient()` for advanced users to override the `mad_thresh` computed property. *Note:* this new `mad_thresh` parameter defaults to `NULL` (and should pretty much always stay as such).
* FF (#122): fixed issue with incompatible unicode character in plot titles (#123)

# eyeris v0.1.0.9000
<small>`February 11th, 2025`</small>

## Breaking changes
* NF (#10): add support for `.asc` files containing multiple recording segments within the same file (#120)
  * There is a new `block` argument added to the `load_asc()` function
  * The default setting is "auto", which aims to automatically handle multiple recording segments within the same `.asc` file. We recommend using this default as this is likely the *safer choice* rather than assuming a single-block recording. **Furthermore, add downstream functions are intentionally designed to support any _N_ number of blocks; using the "auto" setting automatically enables this support for `.asc` files containing single recording blocks by labeling the single recording session as `block_1`**
  * You can also manually specify a different block value (numeric) instead of "auto", which can be helpful for multi-block experiments where each block/run was recorded to a separate `.asc` file. This is especially important to consider when running the downstream `epoch()` and `bidsify()` functions so that derived files and summary reports are properly labeled with the correction block/run number
  * Currently, there is also a `NULL` option; however, this is likely going to just be a part of `beta` testing and will probably be removed in a future version given the foregoing 2 options should cover most (if not all) use cases
  
## New features 
* NF: robust HTML and PDF output summary sidecar reports within `bidsify()` (#120)
  * Here, reports are well-organized both by block/run and any specific event message epochs that have been processed using the `epoch()` function
  * For epoched data, we now provide a useful *interactive utility* within the epoch-specific HTML reports: you can now use your mouse and/or keyboard to swiftly navigate through an interactive gallery of pupil plot segments from every single trial from any given subject, right out-of-the-box! We hope this alleviates some of the complexities/roadblocks users might face when needing to perform manual inspections of their data for quality assurance and/or diagnostic purposes.

## Minor improvements and fixes
* FF (#118): resolved a minor bug in the EyeLink EDF header `model` and `version` fields for data collected on newer EyeLink hardware/software (#120)
* ENH: event epoching is now both **more robust** and **super fast** (#120)
  * We have implemented more efficient data structures to swiftly handle large sets of pupil samples in rapid time
  * We have also added in better visual feedback within the console regarding epoching progress
  * Similarly, these added benefits coincide nicely with the new multi-block support (#10)
* General bug fixes and enhancements to codebase and front-end UX (#120)

# eyeris v0.0.0.9000
<small>`November 23rd, 2024`</small>

* Initial beta release

---

## Commit message tags
* **BF:** bug fix (in the *release* branch)
* **FF:** feature fix (bug fixes in the *dev* branch)
* **RF:** refactoring
* **NF:** new feature
* **ENH:** enhancement
* **DOC:** for documentation-related updates and changes
* **TEST:** for commits that add or change unit tests
* **CHORE:** like **RF** but usually less important changes
