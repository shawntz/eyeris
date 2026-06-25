# Z-score pupil time series data

The intended use of this method is to scale the arbitrary units of the
pupil size time series to have a mean of `0` and a standard deviation of
`1`. This is accomplished by mean centering the data points and then
dividing them by their standard deviation (i.e., z-scoring the data,
similar to [`base::scale()`](https://rdrr.io/r/base/scale.html)). Opting
to z-score your pupil data helps with trial-level and between-subjects
analyses where arbitrary units of pupil size recorded by the tracker do
not scale across participants, and therefore make analyses that depend
on data from more than one participant difficult to interpret.

## Usage

``` r
zscore(eyeris, call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call

## Value

An `eyeris` object with a new column in `time series`:
`pupil_raw_{...}_z`

## Details

This function is automatically called by
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
by default. Use `glassbox(zscore = FALSE)` to disable this step as
needed.

Users should prefer using
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

In general, it is common to z-score pupil data within any given
participant, and furthermore, z-score that participant's data as a
function of block number (for tasks/experiments where participants
complete more than one block of trials) to account for potential
time-on-task effects across task/experiment blocks.

As such, if you use the `eyeris` package as intended, you should NOT
need to specify any groups for the participant/block-level situations
described above. This is because `eyeris` is designed to preprocess a
single block of pupil data for a single participant, one at a time.
Therefore, when you later merge all of the preprocessed data from
`eyeris`, each individual, preprocessed block of data for each
participant will have already been independently scaled from the others.

Additionally, if you intend to compare mean z-scored pupil size across
task conditions, such as that for memory successes vs. memory failures,
then do NOT set your behavioral outcome (i.e., success/failure) variable
as a grouping variable within your analysis. If you do, you will
consequently obtain a mean pupil size of 0 and standard deviation of 1
within each group (since the scaled pupil size would be calculated on
the time series from each outcome variable group, separately). Instead,
you should compute the z-score on the entire pupil time series (before
epoching the data), and then split and take the mean of the z-scored
time series as a function of condition variable.

## Note

This function is part of the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Use `glassbox(zscore = TRUE)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
for the recommended way to run this step as part of the full eyeris
glassbox preprocessing pipeline

For a complete, end-to-end reference pipeline that demonstrates how all
`eyeris` preprocessing functions are chained together in practice, see
the "Building Blocks Under the Hood" section of the *Anatomy of an
`eyeris` Object* vignette —
[`vignette("anatomy", package = "eyeris")`](https://shawnschwartz.com/eyeris/articles/anatomy.md)
— as well as the *Complete Pupillometry Pipeline Walkthrough* vignette:
[`vignette("complete-pipeline", package = "eyeris")`](https://shawnschwartz.com/eyeris/articles/complete-pipeline.md).

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox(zscore = TRUE) |> # set to FALSE to skip (not recommended)
  plot(seed = 0)
#> ✔ [2026-06-25 04:52:26] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-25 04:52:26] [INFO] Processing block: block_1
#> ✔ [2026-06-25 04:52:26] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-25 04:52:26] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-25 04:52:26] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-25 04:52:26] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-06-25 04:52:27] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-25 04:52:27] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-25 04:52:27] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-25 04:52:27] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-25 04:52:27] [INFO] Block processing summary:
#> ℹ [2026-06-25 04:52:27] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-06-25 04:52:27] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-06-25 04:52:27] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1






```
