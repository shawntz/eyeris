## Changelog entry

<!-- Add your changelog entry below. It will be automatically added to NEWS.md -->
<!-- Example: Fixed issue with function parameters (#123) -->

### Problem Addressed
> **EXAMPLE:** Previously, users encountered an error where `eyeris` wasn't playing nicely with binocular EyeLink recording data files. Specifically, an error occurred in `dplyr::select()` indicating that the column ps didn't exist, which hindered the correct loading of data with `eyelinker`. While a temporary "hack" involved adjusting the`load_asc()` function to load the left eye by using psl, xpl, and ypl, **this PR provides a robust, system-wide solution to this issue.**

### Key Changes and Enhancements (Targeting `vX.Y.Z` [`Major`/`Minor`/`Patch`] Release)
> **EXAMPLE:** The following system-wide changes are included for implementation in a minor release `v2.1.0` to better support both monocular and binocular data files:
>
> 1. **Nest all `eyeris` class lists with `left` and `right` parent lists** to explicitly separate out left eye (`L`) and right eye (`R`) data. This design now supports both monocular and binocular data without requiring any special settings or parameters; if the data is monocular, the the resulting `eyeris` objects will remain unchanged from #221.
> 2. **Nest all downstream operations to treat `L` and/or `R` as separate entities**. This crucial step ensures that there is no cross-contamination 😷 between data from the two eyes during any subsequent processing.  
> 3. For the `bidsify()` function, `eyeris` will now append **`_eye-L` and/or `_eye-R` to all derivatives and output HTML reports**. This provides clear and consistent naming conventions, making it easier to identify and manage outputs related to specific eyes.

### Acknowledgments
> **EXAMPLE:** 🙏 Thanks to @shawntz for flagging this issue in #218.

## Breaking changes checklist

- [ ] This PR includes breaking changes
- [ ] Version number has been bumped appropriately
- [ ] Migration guide added to NEWS.md
