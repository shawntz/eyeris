# Generate a synthetic `eyeris` object with realistic pupil characteristics

Simulates a pupil time series whose statistical and morphological
characteristics resemble real EyeLink recordings — a tonic baseline with
slow drift and hippus, canonical task-evoked phasic dilations, blinks
with partial-occlusion flank spikes, isolated transient artifacts,
measurement noise, and optional high-frequency line noise — and wraps it
in a fully valid S3 `eyeris` object.

Because the returned object is byte-compatible with the output of
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md),
it flows unchanged through the entire `eyeris` pipeline:
[`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md),
[`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md),
[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md),
[`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md),
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md),
[`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md),
[`zscore()`](https://eyeris.shawnschwartz.com/reference/zscore.md),
[`plot.eyeris()`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md),
[`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md), and
[`summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md).

## Usage

``` r
simulate_eyeris(seed = 1L, params = sim_params(), block = 1L, verbose = TRUE)
```

## Arguments

- seed:

  Integer random seed for reproducible generation. Defaults to `1`

- params:

  A parameter list from
  [`sim_params()`](https://eyeris.shawnschwartz.com/reference/sim_params.md).
  Defaults to
  [`sim_params()`](https://eyeris.shawnschwartz.com/reference/sim_params.md)

- block:

  Numeric block label written into the time series. Defaults to `1`

- verbose:

  Logical; print a short status message. Defaults to `TRUE`

## Value

An object of S3 class `eyeris` (see the *Anatomy of an `eyeris` Object*
vignette —
[`vignette("anatomy", package = "eyeris")`](https://eyeris.shawnschwartz.com/articles/anatomy.md)),
with a `"sim_truth"` attribute describing the ground-truth signal.

## Details

The signal is generated deterministically given `seed`: the same `seed`
and `params` always yield an identical object, and the global random
number generator state is left untouched (generation is confined via
[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)).
See
[`sim_params()`](https://eyeris.shawnschwartz.com/reference/sim_params.md)
for the full generative model and every tunable component.

The clean latent signal (before artifacts and noise) and the indices of
the injected artifacts are attached to the returned object as the
attribute `"sim_truth"` for use as ground truth in quantitative
demonstrations; note that this attribute does **not** survive the
pipeline step functions (which rebuild the time series data frame), so
downstream tooling recomputes ground truth from a retained copy rather
than relying on the attribute.

## See also

[`sim_params()`](https://eyeris.shawnschwartz.com/reference/sim_params.md)
to configure the synthetic signal.

## Examples

``` r
# generate a synthetic recording and run it through the pipeline
sim <- simulate_eyeris(seed = 1, params = sim_params(duration_secs = 20))
#> ℹ [2026-08-13 18:06:52] [INFO] Simulated 20.0 s @ 1000 Hz (20000 samples, 5.72%
#> missing, 6 blinks, 3 transients).

# \donttest{
out <- sim |>
  eyeris::deblink() |>
  eyeris::detransient() |>
  eyeris::interpolate() |>
  eyeris::lpfilt() |>
  eyeris::zscore()
#> ! [2026-08-13 18:06:52] [WARN] Left 995 sample(s) as NA across gaps longer than
#> 250 ms (not interpolated).
#> ! [2026-08-13 18:06:52] [WARN] `lpfilt()` is operating on data that contains
#> gaps longer than the interpolation limit (`max_gap_ms`), which were left as
#> `NA`. These gaps are temporarily filled so the filter can run and then masked
#> back to `NA`; this can slightly bias the valid pupil samples immediately
#> adjacent to each gap toward the interpolated values. If this bias is a concern
#> for your analysis, consider disabling filtering and/or downsampling (e.g.
#> `lpfilt = FALSE` and/or `downsample = FALSE` in `glassbox()`).

pdf(tempfile(fileext = ".pdf"))
plot(out, seed = 1)
#> ℹ [2026-08-13 18:06:52] [INFO] Plotting block 1 with sampling rate 1000 Hz from
#> possible blocks: 1
dev.off()
#> agg_record_1c3a1b1721ba 
#>                       2 
# }
```
