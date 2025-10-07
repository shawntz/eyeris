## CRAN Submission

This patch release improves dependency management for Arrow and DuckDB to prevent installation issues on macOS and other platforms.

## Notes

All checks (`R CMD check`, `devtools::check(remote = TRUE)`, GitHub Actions CI) pass on macOS, Windows, and Ubuntu.

Thank you for your time and consideration.

## R CMD check results (eyeris v3.0.1)

── R CMD check results ──────────────────────────────────────────────────────────────────────────────────────── eyeris 3.0.1 ────
Duration: 2m 56.9s

❯ checking CRAN incoming feasibility ... [4s/33s] NOTE
  Maintainer: ‘Shawn Schwartz <shawn.t.schwartz@gmail.com>’
  
  Number of updates in past 6 months: 7

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Downstream dependencies

No strong reverse dependencies to be checked.
