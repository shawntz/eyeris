
<!-- README.md is generated from README.Rmd-->

# `eyeris`: Flexible, Extensible, & Reproducible Processing of Pupil Data <a href="http://shawnschwartz.com/eyeris/" title="eyeris website"><img src="man/figures/logo.png" align="right" width="100" alt="eyeris website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/eyeris)](https://CRAN.R-project.org/package=eyeris)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![build](https://github.com/shawntz/eyeris/actions/workflows/build.yml/badge.svg)](https://github.com/shawntz/eyeris/actions/workflows/build.yml)
[![linter](https://github.com/shawntz/eyeris/actions/workflows/linter.yml/badge.svg)](https://github.com/shawntz/eyeris/actions/workflows/linter.yml)
[![pkgdown](https://github.com/shawntz/eyeris/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/shawntz/eyeris/actions/workflows/pkgdown.yml)
<!-- badges: end -->

<!-- The goal of eyeris is to ... -->

## Motivation

Despite decades of pupillometry research, many established packages and
workflows unfortunately lack design principles based on (F)indability
(A)ccessbility (I)nteroperability (R)eusability (FAIR) principles.
`eyeris`, on the other hand follows a thoughtful design philosophy that
results in an intuitive, modular, performant, and extensible
pupillometry data preprocessing framework. Much of these design
principles were heavily inspired by `Nipype`.

`eyeris` also provides a highly opinionated pipeline for tonic and
phasic pupillometry preprocessing (inspired by `fMRIPrep`). These
opinions are the product of many hours of discussions from core members
and signal processing experts from the Stanford Memory Lab (Shawn
Schwartz, Mingjian He, Haopei Yang, Alice Xue, and Anthony Wagner).

`eyeris` also introduces a `BIDS`-like structure for organizing
derivative (preprocessed) pupillometry data, as well as an intuitive
workflow for inspecting preprocessed pupillometry epochs within
beautiful, interactive HTML report files (see demonstration below ⬇️)!

<img src="https://github.com/shawntz/eyeris/raw/dev/inst/figures/interactive-reports-demo.gif" width="100%" />

## Installation

### stable release from CRAN

You can install the stable release of [`eyeris` from
CRAN](https://cran.r-project.org/package=eyeris) with:

``` r
install.packages("eyeris")
```

or

``` r
# install.packages("pak")
pak::pak("eyeris")
```

### development version from GitHub

You can install the development version of [`eyeris` from
GitHub](https://github.com/shawntz/eyeris) with:

``` r
# install.packages("devtools")
devtools::install_github("shawntz/eyeris", ref = "dev")
```

## Example

### the `glassbox()` “prescription” function

This is a basic example of how to use `eyeris` out of the box with our
very *opinionated* set of steps and parameters that one should start out
with when preprocessing pupillometry data. Critically, this is a
“glassbox” – as opposed to a “blackbox” – since each step and parameter
implemented herein is fully open and accessible to you. We designed each
pipeline step / function to be like legos – they are intentionally and
carefully designed in a way that allows you to flexibly construct and
compare different pipelines.

We hope you enjoy! -shawn

``` r
set.seed(32)

library(eyeris)

demo_data <- eyelink_asc_demo_dataset()

eyeris_preproc <- glassbox(
  demo_data,
  lpfilt = list(plot_freqz = FALSE)
)
#> ✔ [  OK  ] - Running eyeris::load_asc()
#> ✔ [  OK  ] - Running eyeris::deblink()
#> ✔ [  OK  ] - Running eyeris::detransient()
#> ✔ [  OK  ] - Running eyeris::interpolate()
#> ✔ [  OK  ] - Running eyeris::lpfilt()
#> ! [ SKIP ] - Skipping eyeris::detrend()
#> ✔ [  OK  ] - Running eyeris::zscore()
```

### step-wise correction of pupillary signal

``` r
plot(eyeris_preproc)
```

<div style="display: flex; justify-content: center; gap: 20px;">

<img src="https://github.com/shawntz/eyeris/raw/dev/inst/figures/ts_coalesced.gif" width="49%" alt="glassbox timeseries animation"><img src="https://github.com/shawntz/eyeris/raw/dev/inst/figures/hists_coalesced.gif" width="49%" alt="glassbox histograms animation">

</div>

### final pre-post correction of pupillary signal (raw ➡ preprocessed)

``` r
plot(eyeris_preproc,
  steps = c(1, 5),
  preview_window = c(0, nrow(eyeris_preproc$timeseries$block_1))
)
#> ! Plotting block 1 from possible blocks: 1
```

<img src="man/figures/README-timeseries-plot-1.png" width="100%" /><img src="man/figures/README-timeseries-plot-2.png" width="100%" />

------------------------------------------------------------------------

## `eyeris` dependency graph :see_no_evil:

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

------------------------------------------------------------------------

# Contributing to `eyeris`

Thank you for considering contributing to the open-source `eyeris` R
package; there are many ways one could contribute to `eyeris`.

We believe the best preprocessing practices emerge from collective
expertise and rigorous discussion. Please see the [contribution
guidelines](https://shawnschwartz.com/eyeris/CONTRIBUTING.html) for more
information on how to get started..

## Code of Conduct

Please note that the eyeris project is released with a [Contributor Code
of Conduct](https://shawnschwartz.com/eyeris/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Suggestions, questions, issues?

Please use the issues tab (<https://github.com/shawntz/eyeris/issues>)
to make note of any bugs, comments, suggestions, feedback, etc… all are
welcomed and appreciated, thanks!

## 📚 Citing `eyeris`

<div class="alert alert-light" style="padding-bottom: 0;">

If you use the `eyeris` package in your research, please cite it!

Run the following in R to get the citation:

</div>

``` r
citation("eyeris")
#> To cite package 'eyeris' in publications use:
#> 
#>   Schwartz S (2025). _eyeris: Flexible, Extensible, & Reproducible
#>   Processing of Pupil Data_. R package version 1.1.0,
#>   <https://shawnschwartz.com/eyeris/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {eyeris: Flexible, Extensible, & Reproducible Processing of Pupil Data},
#>     author = {Shawn Schwartz},
#>     year = {2025},
#>     note = {R package version 1.1.0},
#>     url = {https://shawnschwartz.com/eyeris/},
#>   }
```
