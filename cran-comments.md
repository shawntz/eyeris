## CRAN Submission

This release changes how missing pupil data is handled (long gaps are no longer interpolated over by default), adds a tracker-agnostic data loader plus a synthetic data generator, and migrates all diagnostic plotting to 'ggplot2'.

## Resubmission

This is a resubmission. The previous submission was flagged by the incoming pre-tests
with a NOTE for examples exceeding the elapsed-time thresholds:

    Examples with CPU (user + system) or elapsed time > 10s (Windows) / > 5s (Debian)
      plot.eyeris, detrend, interpolate, deblink, glassbox

The cause is that this release migrates all diagnostic plotting from base graphics to
'ggplot2' (via 'reaborn'), which is considerably slower to render. The affected examples
each drew several multi-panel figures spanning the default 10-second preview window.

The examples now pass an explicit, shorter `preview_window` to `plot()` (a documented
argument of that function) so that each figure covers a 2-second subset of the demo
recording, and one duplicated `plot()` call was removed from `?plot.eyeris`. No
functionality, arguments, or demonstrated behavior were changed, and no example was
moved to `\donttest{}`.

On a local machine where the previously submitted tarball timed `plot.eyeris` at 4.1s
elapsed, it now times at 1.1s (all other examples < 1.2s), a 2-4x reduction across the
five flagged topics.

## Notes

All checks (`R CMD check`, `devtools::check(remote = TRUE)`, GitHub Actions CI) pass on macOS, Windows, and Ubuntu.

Thank you for your time and consideration.

## R CMD check results (eyeris v3.3.0)

Duration: 4m 41.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

No strong reverse dependencies to be checked.
