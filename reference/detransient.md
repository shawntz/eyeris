# Remove pupil samples that are physiologically unlikely

The intended use of this method is for removing pupil samples that
emerge more quickly than would be physiologically expected. This is
accomplished by rejecting samples that exceed a "speed"-based threshold
(i.e., median absolute deviation from sample-to-sample). This threshold
is computed based on the constant `n`, which defaults to the value `16`.

## Usage

``` r
detransient(eyeris, n = 16, mad_thresh = NULL, call_info = NULL)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- n:

  A constant used to compute the median absolute deviation (MAD)
  threshold. Defaults to `16`

- mad_thresh:

  Default `NULL`. This parameter provides alternative options for
  handling edge cases where the computed properties here within
  `detransient()` \\mad\\val\\ and \\median\\speed\\ are very small. For
  example, if \$\$mad\\val = 0 \quad \text{and} \quad median\\speed =
  1,\$\$ then, with the default multiplier \\n = 16\\, \$\$mad\\thresh =
  median\\speed + (n \times mad\\val) = 1 + (16 \times 0) = 1.\$\$ In
  this situation, any speed \\p_i \ge 1\\ would be flagged as a
  transient, which might be overly sensitive. To reduce this
  sensitivity, two possible adjustments are available:

  1.  If \\mad\\thresh = 1\\, the transient detection criterion is
      modified from \$\$p_i \ge mad\\thresh\$\$ to \$\$p_i \>
      mad\\thresh .\$\$

  2.  If \\mad\\thresh\\ is very small, the user may manually adjust the
      sensitivity by supplying an alternative threshold value here
      directly via this `mad_thresh` parameter.

- call_info:

  A list of call information and parameters. If not provided, it will be
  generated from the function call. Defaults to `NULL`

## Value

An `eyeris` object with a new column in `time series`:
`pupil_raw_{...}_detransient`

## Details

This function is automatically called by
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
by default. If needed, customize the parameters for `detransient` by
providing a parameter list. Use `glassbox(detransient = FALSE)` to
disable this step as needed.

Users should prefer using
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
rather than invoking this function directly unless they have a specific
reason to customize the pipeline manually.

**Computed properties:**

- **`pupil_speed`:** Compute speed of pupil by approximating the
  derivative of `x` (pupil) with respect to `y` (time) using finite
  differences.

  - Let \\x = (x_1, x_2, \dots, x_n)\\ and \\y = (y_1, y_2, \dots,
    y_n)\\ be two numeric vectors with \\n \ge 2\\; then, the finite
    differences are computed as: \$\$\delta_i = \frac{x\_{i+1} -
    x_i}{y\_{i+1} - y_i}, \quad i = 1, 2, \dots, n-1.\$\$

  - This produces an output vector \\p = (p_1, p_2, \dots, p_n)\\
    defined by:

    - For the first element: \$\$p_1 = \|\delta_1\|,\$\$

    - For the last element: \$\$p_n = \|\delta\_{n-1}\|,\$\$

    - For the intermediate elements (\\i = 2, 3, \dots, n-1\\): \$\$p_i
      = \max\\\|\delta\_{i-1}\|,\\\|\delta_i\|\\.\$\$

- **`median_speed`:** The median of the computed `pupil_speed`:
  \$\$median\\speed = median(p)\$\$

- **`mad_val`:** The median absolute deviation (MAD) of `pupil_speed`
  from the median: \$\$mad\\val = median(\|p - median\\speed\|)\$\$

- **`mad_thresh`:** A threshold computed from the median speed and the
  MAD, using a constant multiplier \\n\\ (default value: 16):
  \$\$mad\\thresh = median\\speed + (n \times mad\\val)\$\$

## Note

This function is part of the
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
preprocessing pipeline and is not intended for direct use in most cases.
Provide parameters via `detransient = list(...)`.

Advanced users may call it directly if needed.

## See also

[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
for the recommended way to run this step as part of the full `eyeris`
glassbox preprocessing pipeline.

For a complete, end-to-end reference pipeline that demonstrates how all
`eyeris` preprocessing functions are chained together in practice, see
the "Building Blocks Under the Hood" section of the *Anatomy of an
`eyeris` Object* vignette —
[`vignette("anatomy", package = "eyeris")`](https://eyeris.shawnschwartz.com/articles/anatomy.md)
— as well as the *Complete Pupillometry Pipeline Walkthrough* vignette:
[`vignette("complete-pipeline", package = "eyeris")`](https://eyeris.shawnschwartz.com/articles/complete-pipeline.md).

## Examples

``` r
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox(
    detransient = list(n = 16) # set to FALSE to skip step (not recommended)
  ) |>
  plot(seed = 0)
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-07-03 02:17:31] [INFO] Processing block: block_1
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-07-03 02:17:31] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-03 02:17:31] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-07-03 02:17:31] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-03 02:17:31] [INFO] Block processing summary:
#> ℹ [2026-07-03 02:17:31] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-07-03 02:17:31] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-07-03 02:17:31] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1






```
