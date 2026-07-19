# Configure the synthetic pupil signal model

Builds the parameter list that controls
[`simulate_eyeris()`](https://eyeris.shawnschwartz.com/reference/simulate_eyeris.md).
Every component of the synthetic pupil time series (tonic baseline, slow
drift, hippus, task-evoked phasic responses, blinks, transient spikes,
measurement noise, and optional line noise) is independently toggle-able
and parameterized here, so a single scenario can isolate exactly the
feature it needs to teach.

## Usage

``` r
sim_params(
  fs = 1000L,
  duration_secs = 60,
  baseline = TRUE,
  baseline_mean = 5000,
  drift = FALSE,
  drift_slope = 8,
  drift_curv = 0,
  hippus = TRUE,
  hippus_amp = 40,
  hippus_freq = 0.12,
  phasic = TRUE,
  phasic_n = 10.1,
  phasic_tmax = 0.93,
  phasic_amp = c(120, 300),
  phasic_isi = c(6, 8),
  phasic_onsets_ms = NULL,
  blinks = TRUE,
  n_blinks = 6,
  blink_dur_ms = c(100, 300),
  blink_flank_ms = 40,
  blink_depth = 400,
  blink_overshoot = 300,
  blink_flank_shape = c("linear", "cosine"),
  transients = TRUE,
  n_transients = 3,
  transient_amp = c(200, 350),
  transient_width_ms = 1.5,
  noise = TRUE,
  noise_sd = 3,
  noise_ar = 0,
  line = FALSE,
  line_freq = 96,
  line_amp = 20,
  dropout_frac = 0.003,
  dropout_run_ms = c(1, 5),
  clip = c(3200, 7200),
  eye = c("R", "L"),
  pupil_dtype = c("DIAMETER", "AREA")
)
```

## Arguments

- fs:

  Sampling rate in Hz. Must divide 1000 evenly (e.g. `1000`, `500`,
  `250`) so sample timestamps remain integer milliseconds. Defaults to
  `1000`

- duration_secs:

  Recording duration in seconds. Defaults to `60`

- baseline:

  Logical; include the tonic baseline offset. Defaults to `TRUE`

- baseline_mean:

  Tonic baseline pupil size in a.u. Defaults to `5000`

- drift:

  Logical; include a slow linear drift. Defaults to `FALSE`

- drift_slope:

  Drift slope magnitude in a.u. per second (applied as a decline).
  Defaults to `8` (i.e. a 480 a.u. decline over 60 s)

- drift_curv:

  Quadratic drift coefficient in a.u. per second squared. Defaults to
  `0`

- hippus:

  Logical; include the slow hippus oscillation. Defaults to `TRUE`

- hippus_amp:

  Hippus amplitude in a.u. Defaults to `40`

- hippus_freq:

  Hippus frequency in Hz. Defaults to `0.12`

- phasic:

  Logical; include task-evoked phasic responses. Defaults to `TRUE`

- phasic_n:

  Erlang shape parameter (unitless). Defaults to `10.1`

- phasic_tmax:

  Time-to-peak of the phasic response in seconds. Defaults to `0.930`

- phasic_amp:

  Length-2 numeric `c(min, max)`; peak phasic amplitudes are drawn
  uniformly from this range (a.u.). Defaults to `c(120, 300)`

- phasic_isi:

  Length-2 numeric `c(min, max)`; inter-stimulus intervals (seconds) are
  drawn uniformly from this range when `phasic_onsets_ms` is `NULL`.
  Defaults to `c(6, 8)`

- phasic_onsets_ms:

  Optional numeric vector of explicit stimulus onset times in
  milliseconds. When `NULL` (default), onsets are generated from
  `phasic_isi`

- blinks:

  Logical; include blink artifacts. Defaults to `TRUE`

- n_blinks:

  Number of blinks. Defaults to `6`

- blink_dur_ms:

  Length-2 numeric `c(min, max)`; blink core (missing-data) durations in
  milliseconds are drawn uniformly from this range. Defaults to
  `c(100, 300)`

- blink_flank_ms:

  Duration in milliseconds of the occlusion spike on each side of a
  blink core. Keep below the
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md)
  `extend` value (default 50 ms) so deblinking fully removes it.
  Defaults to `40`

- blink_depth:

  Depth in a.u. of the leading occlusion dip. Defaults to `400`

- blink_overshoot:

  Height in a.u. of the trailing recovery overshoot. Defaults to `300`

- blink_flank_shape:

  Shape of the occlusion ramp: `"linear"` or `"cosine"`. Defaults to
  `"linear"`

- transients:

  Logical; include isolated transient spikes. Defaults to `TRUE`

- n_transients:

  Number of transient spikes. Defaults to `3`

- transient_amp:

  Length-2 numeric `c(min, max)`; transient amplitudes are drawn
  uniformly from this range (a.u.) with random sign. Defaults to
  `c(200, 350)`

- transient_width_ms:

  Gaussian width (standard deviation) of each transient in milliseconds.
  Defaults to `1.5`

- noise:

  Logical; include broadband measurement noise. Should remain `TRUE` for
  any signal that will be passed to
  [`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md).
  Defaults to `TRUE`

- noise_sd:

  Standard deviation of the white measurement noise in a.u. Defaults to
  `3`

- noise_ar:

  Lag-1 autocorrelation for optional AR(1) noise coloring in `[0, 1)`.
  Defaults to `0` (white noise)

- line:

  Logical; include a high-frequency line-noise tone. Defaults to `FALSE`

- line_freq:

  Line-noise frequency in Hz. Defaults to `96`

- line_amp:

  Line-noise amplitude in a.u. Defaults to `20`

- dropout_frac:

  Fraction of (non-blink) samples to mark missing as short scattered
  dropout runs. Defaults to `0.003`

- dropout_run_ms:

  Length-2 numeric `c(min, max)`; dropout run lengths in milliseconds.
  Defaults to `c(1, 5)`

- clip:

  Length-2 numeric `c(min, max)`; observed (non-missing) samples are
  clamped to this a.u. range. Defaults to `c(3200, 7200)`

- eye:

  Which eye to label the data as (`"L"` or `"R"`). Defaults to `"R"`

- pupil_dtype:

  Pupil data type label (`"DIAMETER"` or `"AREA"`). Defaults to
  `"DIAMETER"`

## Value

A named list of class `eyeris_sim_params`.

## Details

The synthetic signal is composed additively in arbitrary units (a.u.)
matched to real EyeLink **pupil diameter** data (which typically ranges
from roughly 3600–7000 a.u.):

\$\$L(t) = B_0 + \mathrm{drift}(t) + \mathrm{hippus}(t) + \sum_k
\mathrm{phasic}\_k(t)\$\$ \$\$\mathrm{pupil}(t) = L(t) + \sum_j
\mathrm{transient}\_j(t) + \mathrm{line}(t) + \mathrm{noise}(t)\$\$

after which blink and dropout artifacts are stamped on (see
[`simulate_eyeris()`](https://eyeris.shawnschwartz.com/reference/simulate_eyeris.md)).

**Components (each independently toggle-able):**

- **Tonic baseline** (`baseline`): a constant offset `baseline_mean`.

- **Linear drift** (`drift`): a slow tonic decline
  \\-\mathrm{drift\\slope} \cdot t\\ (a.u. per second), plus an optional
  quadratic term `drift_curv`. This is precisely what
  [`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md)
  removes.

- **Hippus** (`hippus`): a slow arousal oscillation \\A_h \sin(2\pi f_h
  t + \phi)\\ that sits below the low-pass cutoff and therefore survives
  [`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md).

- **Phasic response** (`phasic`): the canonical task-evoked pupil
  response modeled with the Hoeks & Levelt (1993) Erlang gamma kernel
  \\h(\tau) = \tau^n e^{-n\tau / t\_{max}}\\, peak-normalized and scaled
  by `phasic_amp`, time-locked to stimulus onsets.

- **Blinks** (`blinks`): runs of **missing data (`NA`)** flanked by the
  rapid partial-occlusion down-then-up spikes that surround real blinks
  — the artifact
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md)
  is designed to remove.

- **Transients** (`transients`): isolated, physiologically implausible
  fast spikes (tracker glitches) that
  [`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md)
  targets.

- **Noise** (`noise`): broadband white measurement noise. Always keep
  `noise_sd > 0` — a perfectly noiseless signal makes the median
  absolute deviation of the pupil speed zero, which aborts
  [`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md).

- **Line noise** (`line`): an optional high-frequency tone used to
  demonstrate aliasing when decimating without an anti-alias filter.

## See also

[`simulate_eyeris()`](https://eyeris.shawnschwartz.com/reference/simulate_eyeris.md)
to generate data from these parameters.

## Examples

``` r
# default parameters
p <- sim_params()

# a short recording with a strong linear drift for a detrend demo
p2 <- sim_params(duration_secs = 20, drift = TRUE, drift_slope = 12)
```
