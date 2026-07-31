## CRAN Submission

This release changes how missing pupil data is handled (long gaps are no longer interpolated over by default), adds a tracker-agnostic data loader plus a synthetic data generator, and migrates all diagnostic plotting to 'ggplot2'.

## Notes

All checks (`R CMD check`, `devtools::check(remote = TRUE)`, GitHub Actions CI) pass on macOS, Windows, and Ubuntu.

Thank you for your time and consideration.

## R CMD check results (eyeris v3.3.0)

Duration: 5m 9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

No strong reverse dependencies to be checked.
