# Generate a reproducible, copy-and-paste-ready methods boilerplate

Auto-generates human-readable, "fMRIPrep-style" methods-section
boilerplate text that describes – in plain prose – the exact
preprocessing workflow that was run on an `eyeris` object. Because every
`eyeris` preprocessing step records its own call stack and parameters
(in `eyeris$params`), the pipeline is able to self-document precisely
what it did to your data, ready to paste directly into the methods
section of a manuscript.

This reinforces the workflow-level transparency at the heart of the
`eyeris` "glassbox" philosophy: the code that ran and the prose that
describes it are generated from one and the same source of truth.

## Usage

``` r
boilerplate(
  eyeris,
  version = NULL,
  include_citation = TRUE,
  include_license = TRUE
)
```

## Arguments

- eyeris:

  An object of class `eyeris` (e.g., the output of
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)).
  Binocular objects are supported.

- version:

  Optional character string giving the `eyeris` version to cite in the
  boilerplate. Defaults to `NULL`, in which case the currently installed
  `eyeris` version is used.

- include_citation:

  Logical. Whether to append a formatted citation for `eyeris`. Defaults
  to `TRUE`.

- include_license:

  Logical. Whether to append the Creative Commons CC BY 4.0 license note
  telling users it is safe to paste the text into a manuscript as long
  as they cite `eyeris` for attribution. Defaults to `TRUE`.

## Value

A length-one character string of Markdown-formatted methods boilerplate
(invisibly classed as `eyeris_boilerplate` so it prints nicely at the
console). The raw string can be passed to
[`base::cat()`](https://rdrr.io/r/base/cat.html),
[`base::writeLines()`](https://rdrr.io/r/base/writeLines.html), or
embedded directly in a report.

## Details

The generated text walks the recorded pipeline steps in the canonical
order in which `eyeris` applies them (`load_asc` -\> `deblink` -\>
`detransient` -\> `interpolate` -\> `lpfilt` -\> `downsample`/`bin` -\>
`detrend` -\> `zscore` -\> `epoch`) and emits one sentence per step,
substituting in the actual parameter values that were used. Steps that
were skipped are omitted, and any custom (user-supplied) pipeline
extensions are appended at the end so that multi-step and non-default
pipelines are described faithfully. Multi-block (multi-run) and
binocular recordings are handled automatically.

The boilerplate also references the per-run, machine-readable `.json`
metadata sidecar that `eyeris` writes for every run
(`source/logs/run-XX_metadata.json`), which together with the reported
package version is sufficient to reproduce the pipeline exactly.

**License & attribution.** The auto-generated boilerplate text is
licensed under the Creative Commons Attribution 4.0 International (CC BY
4.0) license (<https://creativecommons.org/licenses/by/4.0/>). It is
explicitly safe to copy and paste the generated Markdown content
directly into your manuscript, provided that you give appropriate credit
by citing `eyeris` (run `citation("eyeris")` for the reference);
including that citation in your references satisfies the attribution
requirement. This mirrors the approach taken by fMRIPrep and other
reproducible-pipeline tools.

When you run
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md) (or
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
with reporting enabled), this same boilerplate is embedded in the
diagnostic HTML report and written to `derivatives/.../source/logs/` as
a standalone Markdown file.

## See also

[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md),
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

eyeris_obj <- demo_data |>
  eyeris::glassbox(verbose = FALSE)

# print the copy-and-paste-ready methods boilerplate
boilerplate(eyeris_obj)
#> > **Copy-and-paste-ready methods text.** The Markdown below was generated automatically by `eyeris` (version 3.3.0) directly from the parameters recorded in your preprocessing pipeline. It is provided so that your manuscript's methods section can faithfully and reproducibly reflect exactly what `eyeris` did to your data. It is safe to copy and paste this content directly into your manuscript and adapt it as needed; please cite `eyeris` (see below) to provide attribution as required by its license.
#> 
#> Pupillometry data were preprocessed in R using the `eyeris` package (version 3.3.0) (Schwartz et al., 2025), which implements a fully transparent, parameterized "glassbox" preprocessing pipeline. Raw pupil time series were imported from the EyeLink `.asc` recording (sampled at 1000 Hz). Blink and missing-data artifacts were attenuated by extending (i.e., NA-padding) each gap of missing samples by 50 ms in both directions. Physiologically implausible transient samples -- those whose sample-to-sample change in pupil size exceeded a speed-based median absolute deviation (MAD) threshold -- were removed (MAD multiplier n = 16). Remaining gaps of missing data were reconstructed using linear interpolation. The pupil time series was smoothed with a zero-phase (forward-and-reverse) Butterworth low-pass filter (passband edge = 4 Hz, stopband edge = 8 Hz, passband ripple = 1 dB, stopband attenuation = 35 dB). The pupil time series was z-scored (rescaled to a mean of 0 and a standard deviation of 1) within each recording block.
#> 
#> **Reproducibility.** A complete, machine-readable record of every preprocessing operation and the exact parameter values used is stored alongside your data in a per-run JSON metadata sidecar (`source/logs/run-XX_metadata.json`). This call-stack provenance, together with the `eyeris` version reported above, is sufficient to reproduce this pipeline.
#> 
#> **Citation.** If you use `eyeris` in your research, please cite: Schwartz ST, Yang H, Xue AM, He M (2025). “eyeris: A flexible, extensible, and reproducible pupillometry preprocessing framework in R.” _bioRxiv_, 1-37. doi:10.1101/2025.06.01.657312 <https://doi.org/10.1101/2025.06.01.657312>.
#> 
#> > **License & attribution.** This auto-generated boilerplate text is licensed under the [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license. You are free to copy, adapt, and paste it directly into your manuscript, provided that you give appropriate credit by citing `eyeris` (run `citation('eyeris')` for the reference). Including the `eyeris` citation in your references satisfies this attribution requirement.

# capture it as plain Markdown text (e.g., to write to a file)
methods_md <- boilerplate(eyeris_obj)
cat(methods_md)
#> > **Copy-and-paste-ready methods text.** The Markdown below was generated automatically by `eyeris` (version 3.3.0) directly from the parameters recorded in your preprocessing pipeline. It is provided so that your manuscript's methods section can faithfully and reproducibly reflect exactly what `eyeris` did to your data. It is safe to copy and paste this content directly into your manuscript and adapt it as needed; please cite `eyeris` (see below) to provide attribution as required by its license.
#> 
#> Pupillometry data were preprocessed in R using the `eyeris` package (version 3.3.0) (Schwartz et al., 2025), which implements a fully transparent, parameterized "glassbox" preprocessing pipeline. Raw pupil time series were imported from the EyeLink `.asc` recording (sampled at 1000 Hz). Blink and missing-data artifacts were attenuated by extending (i.e., NA-padding) each gap of missing samples by 50 ms in both directions. Physiologically implausible transient samples -- those whose sample-to-sample change in pupil size exceeded a speed-based median absolute deviation (MAD) threshold -- were removed (MAD multiplier n = 16). Remaining gaps of missing data were reconstructed using linear interpolation. The pupil time series was smoothed with a zero-phase (forward-and-reverse) Butterworth low-pass filter (passband edge = 4 Hz, stopband edge = 8 Hz, passband ripple = 1 dB, stopband attenuation = 35 dB). The pupil time series was z-scored (rescaled to a mean of 0 and a standard deviation of 1) within each recording block.
#> 
#> **Reproducibility.** A complete, machine-readable record of every preprocessing operation and the exact parameter values used is stored alongside your data in a per-run JSON metadata sidecar (`source/logs/run-XX_metadata.json`). This call-stack provenance, together with the `eyeris` version reported above, is sufficient to reproduce this pipeline.
#> 
#> **Citation.** If you use `eyeris` in your research, please cite: Schwartz ST, Yang H, Xue AM, He M (2025). “eyeris: A flexible, extensible, and reproducible pupillometry preprocessing framework in R.” _bioRxiv_, 1-37. doi:10.1101/2025.06.01.657312 <https://doi.org/10.1101/2025.06.01.657312>.
#> 
#> > **License & attribution.** This auto-generated boilerplate text is licensed under the [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license. You are free to copy, adapt, and paste it directly into your manuscript, provided that you give appropriate credit by citing `eyeris` (run `citation('eyeris')` for the reference). Including the `eyeris` citation in your references satisfies this attribution requirement.
```
