# Load pupillometry data from a non-EyeLink eye tracker

Construct a valid `eyeris` S3 object from standardized data frames so
that data from eye trackers other than SR Research EyeLink can enter the
`eyeris` preprocessing pipeline. While
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
parses EyeLink `.asc` files specifically, `load_generic()` provides a
tracker-agnostic ingestion path: you supply the raw samples (and,
optionally, event messages, gaze coordinates, and blink intervals) as
plain R data frames, and `load_generic()` assembles them into the same
object structure that the rest of `eyeris` (including
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md))
expects.

## Usage

``` r
load_generic(
  pupil,
  events = NULL,
  blinks = NULL,
  gaze = NULL,
  sample_rate = NULL,
  time_unit = c("ms", "s"),
  block = "auto",
  eye = c("L", "R", "LR"),
  pupil_type = c("area", "diameter"),
  screen_width = NA_real_,
  screen_height = NA_real_,
  tracker = "generic",
  model = NA_character_,
  mapping = NULL,
  path = NULL,
  verbose = TRUE
)
```

## Arguments

- pupil:

  A data frame of raw samples. Must contain a timestamp column and a
  pupil-size column (see `mapping`). May optionally contain gaze
  coordinate and `block` columns.

- events:

  An optional data frame of event messages with a timestamp column (on
  the same clock as `pupil`) and a message-text column. If `NULL`
  (default), the object is created with empty event tables.

- blinks:

  An optional data frame of blink intervals with start and end timestamp
  columns. If `NULL` (default), empty blink tables are created. Note
  that blink padding via
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md)
  does not depend on this table.

- gaze:

  An optional data frame of gaze coordinate samples (timestamp, `x`,
  `y`), used only when gaze is exported separately from pupil size. It
  is left-joined onto `pupil` by timestamp. If `NULL` (default), gaze is
  taken from `pupil` if those columns are present, otherwise filled with
  `NA`.

- sample_rate:

  Numeric sampling rate of the tracker in Hz. If `NULL` (default), it is
  inferred from the median spacing of the `pupil` timestamps; inference
  is reported and we recommend supplying the true rate explicitly.

- time_unit:

  Unit of all timestamp columns (in `pupil`, `events`, `gaze`, and
  `blinks`). Either `"ms"` (milliseconds, the default, matching EyeLink)
  or `"s"` (seconds). Timestamps are stored internally in milliseconds.

- block:

  Block specification, mirroring
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md):

  - `"auto"` (default): if the `pupil` data contains a block column with
    more than one unique value, the data are split into multiple blocks;
    otherwise a single block (`block_1`) is created.

  - `NULL`: omit the block column and create a single block.

  - Numeric value: assign this block number to all samples.

- eye:

  Which eye the data correspond to: `"L"` (left, default), `"R"`
  (right), or `"LR"` (both, e.g., averaged). Recorded as metadata.

- pupil_type:

  Pupil measurement units: `"area"` (default) or `"diameter"`. Recorded
  as metadata (the `type` column).

- screen_width, screen_height:

  Optional screen dimensions in pixels, used for gaze heatmaps and
  gaze-based confounds. Leave as `NA` (default) if unknown; the gaze
  heatmap is simply skipped.

- tracker:

  Character label for the source tracker/system (default `"generic"`).
  Recorded in `info$version`.

- model:

  Optional character label for the specific tracker model. Recorded in
  `info$model`.

- mapping:

  An optional named list remapping the default column names to the ones
  present in your data frames. Recognized names are `time`, `pupil`,
  `eye_x`, `eye_y`, `text`, `stime`, `etime`, and `block`. For example,
  `mapping = list(time = "t_ms", pupil = "pup_size")`.

- path:

  Optional character path/identifier stored in the object's `file` slot
  (used in report titles). Defaults to the value of `tracker`.

- verbose:

  Logical. Whether to print verbose output (default `TRUE`).

## Value

An object of S3 class `eyeris` with the same structure as
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md):

1.  `file`: The `path` identifier for the source data.

2.  `timeseries`: A named list of per-block data frames of raw time
    series data (`time_orig`, `time_secs`, `time_scaled`, `eye_x`,
    `eye_y`, `eye`, `hz`, `type`, `pupil_raw`).

3.  `events`: A named list of per-block event-message data frames.

4.  `blinks`: A named list of per-block blink data frames.

5.  `info`: Tracker metadata (`sample.rate`, `mono`, `left`, `right`,
    `pupil.dtype`, `version`, `model`, `screen.x`, `screen.y`).

6.  `latest`: `eyeris` pointer for tracking pipeline run history.

7.  `binocular`, `binocular_mode`, `decimated.sample.rate`, `params`.

## Details

`eyeris` was designed to be extensible, but historically
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
was the only implemented loader. `load_generic()` closes that gap.
Native support for specific tracker formats (e.g., Tobii, SMI, Pupil
Labs, GazePoint) can be layered on top of this function incrementally: a
format-specific reader only needs to produce the standardized data
frames documented below and then call `load_generic()`.

The resulting object is structurally identical to one returned by
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md),
so it is a drop-in input to
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
and the individual preprocessing steps
([`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md),
[`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md),
[`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md),
[`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md),
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md),
[`bin()`](https://eyeris.shawnschwartz.com/reference/bin.md),
[`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md),
[`zscore()`](https://eyeris.shawnschwartz.com/reference/zscore.md)),
[`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md),
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md),
and the plotting methods.

### The three standardized data frames

Following the conceptual model that most trackers export,
`load_generic()` accepts three core data frames (plus an optional fourth
for gaze that is exported separately):

1.  **`pupil`** (required) – the raw sample stream. Must contain a
    timestamp column and a pupil-size column. May *also* carry gaze
    coordinate columns (`eye_x`, `eye_y`) if your tracker exports
    samples as one wide table.

2.  **`events`** (optional) – experimental event messages. Must contain
    a timestamp column (on the same clock as `pupil`) and a message-text
    column. Required only if you intend to epoch on event messages
    later.

3.  **`blinks`** (optional) – blink intervals reported by the tracker.
    Must contain blink start and end timestamp columns. Note that blink
    padding via
    [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md)
    does *not* depend on this table – it reconstructs missing/blink
    regions directly from `NA` (and `0`) values in the pupil column – so
    this table is purely for record-keeping and export.

A fourth, **`gaze`**, data frame is accepted for the less common case
where gaze coordinates are exported separately from pupil size
(timestamp + `x`/`y` columns); it is joined onto `pupil` by timestamp.
If your `pupil` data frame already contains gaze columns, leave this
`NULL`.

If a column in your data frames does not use the default name expected
by `load_generic()`, remap it via the `mapping` argument (see below)
rather than renaming your data by hand.

### Column requirements and defaults

By default `load_generic()` looks for these columns (override any of
them with `mapping`):

|                |                   |                    |
|----------------|-------------------|--------------------|
| **data frame** | **role**          | **default column** |
| `pupil`        | timestamp         | `time`             |
| `pupil`        | pupil size        | `pupil`            |
| `pupil`        | gaze x (optional) | `eye_x`            |
| `pupil`        | gaze y (optional) | `eye_y`            |
| `pupil`        | block (optional)  | `block`            |
| `events`       | timestamp         | `time`             |
| `events`       | message text      | `text`             |
| `blinks`       | blink start       | `stime`            |
| `blinks`       | blink end         | `etime`            |
| `gaze`         | timestamp         | `time`             |
| `gaze`         | gaze x            | `eye_x`            |
| `gaze`         | gaze y            | `eye_y`            |

### Handling tracker quirks

`eyeris` assumes EyeLink-style regularly-sampled data. Two practical
notes for other trackers:

- **Missing samples.**
  [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md)
  reconstructs missing/blink regions directly from `NA` (and `0`) values
  in the pupil column – it does *not* require the `blinks` table. If
  your tracker drops samples or encodes missing pupil data some other
  way, set those samples to `NA` in the `pupil` column so deblinking and
  the confound calculations behave correctly.

- **Irregular sampling.** If consecutive timestamps are not uniformly
  spaced, `load_generic()` emits a warning, because several downstream
  steps assume a fixed sampling interval. (A fuller guardrail is tracked
  separately.)

## See also

[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
for loading SR Research EyeLink `.asc` files.

[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
for running the full `eyeris` preprocessing pipeline on the object
returned by this function.

## Examples

``` r
# build three small standardized data frames from any non-EyeLink tracker
set.seed(1)
n <- 1000
samples <- data.frame(
  time = seq(0, by = 1, length.out = n), # 1000 Hz -> 1 ms spacing
  pupil = 1000 + cumsum(rnorm(n, 0, 5)),
  eye_x = 960 + rnorm(n, 0, 10),
  eye_y = 540 + rnorm(n, 0, 10)
)

events <- data.frame(
  time = c(100, 500),
  text = c("TRIALID 1", "TRIALID 2")
)

# construct a valid eyeris object
eye <- eyeris::load_generic(
  pupil = samples,
  events = events,
  sample_rate = 1000,
  screen_width = 1920,
  screen_height = 1080,
  tracker = "my-tracker"
)
#> ℹ [2026-07-18 21:24:43] [INFO] Loaded generic 'my-tracker' data: 1 block(s),
#> 1000 Hz.

# ...and run it straight through the glassbox pipeline
eye |>
  eyeris::glassbox(lpfilt = list(plot_freqz = FALSE))
#> ℹ [2026-07-18 21:24:43] [INFO] Received a pre-loaded `eyeris` object; skipping
#> the load step and running the remaining pipeline on it directly.
#> ℹ [2026-07-18 21:24:43] [INFO] Processing block: block_1
#> ✔ [2026-07-18 21:24:43] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-07-18 21:24:43] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-07-18 21:24:43] [OKAY] Running eyeris::interpolate() for block_1
#> ! [2026-07-18 21:24:43] [WARN] Interpolation now leaves gaps longer than 250 ms
#> as `NA` instead of interpolating across them (following Kret & Sjak-Shie,
#> 2018). This is a change in default behavior from eyeris <= 3.2.0 and may affect
#> your results. To restore the previous behavior, set `interpolate =
#> list(max_gap_ms = Inf)` in `glassbox()` (or `max_gap_ms = Inf` in
#> `interpolate()`).
#> ! [2026-07-18 21:24:43] [WARN] No NAs detected in pupil data... Skipping
#> interpolation!
#> ✔ [2026-07-18 21:24:43] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2026-07-18 21:24:43] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-07-18 21:24:43] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-07-18 21:24:43] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-07-18 21:24:43] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-07-18 21:24:43] [INFO] Block processing summary:
#> ℹ [2026-07-18 21:24:43] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-07-18 21:24:43] [OKAY] Running eyeris::summarize_confounds()
#> $file
#> [1] "my-tracker"
#> 
#> $timeseries
#> $timeseries$block_1
#>      block time_orig time_secs time_scaled    eye_x    eye_y eye   hz type
#> 1        1         0     0.000       0.000 971.3497 531.1385   L 1000 area
#> 2        1         1     0.001       0.001 971.1193 520.7775   L 1000 area
#> 3        1         2     0.002       0.002 951.2922 556.1970   L 1000 area
#> 4        1         3     0.003       0.003 962.1073 545.1927   L 1000 area
#> 5        1         4     0.004       0.004 960.6940 539.4415   L 1000 area
#> 6        1         5     0.005       0.005 943.3735 546.9642   L 1000 area
#> 7        1         6     0.006       0.006 968.1084 540.5352   L 1000 area
#> 8        1         7     0.007       0.007 940.8765 526.8972   L 1000 area
#> 9        1         8     0.008       0.008 947.5325 518.7693   L 1000 area
#> 10       1         9     0.009       0.009 969.9815 537.9192   L 1000 area
#> 11       1        10     0.010       0.010 954.5913 536.8721   L 1000 area
#> 12       1        11     0.011       0.011 957.8362 529.4176   L 1000 area
#> 13       1        12     0.012       0.012 943.7806 544.1722   L 1000 area
#> 14       1        13     0.013       0.013 945.4904 536.8455   L 1000 area
#> 15       1        14     0.014       0.014 963.5091 548.2555   L 1000 area
#> 16       1        15     0.015       0.015 958.2545 552.9127   L 1000 area
#> 17       1        16     0.016       0.016 954.0857 533.7490   L 1000 area
#> 18       1        17     0.017       0.017 946.6597 531.2485   L 1000 area
#> 19       1        18     0.018       0.018 949.0270 541.4336   L 1000 area
#> 20       1        19     0.019       0.019 980.3610 554.2519   L 1000 area
#> 21       1        20     0.020       0.020 956.7351 522.6525   L 1000 area
#> 22       1        21     0.021       0.021 967.7401 531.6260   L 1000 area
#> 23       1        22     0.022       0.022 967.8501 532.8425   L 1000 area
#> 24       1        23     0.023       0.023 967.6325 543.7482   L 1000 area
#> 25       1        24     0.024       0.024 962.9481 563.8333   L 1000 area
#> 26       1        25     0.025       0.025 947.4764 535.3609   L 1000 area
#> 27       1        26     0.026       0.026 949.9050 535.7350   L 1000 area
#> 28       1        27     0.027       0.027 967.5139 522.9690   L 1000 area
#> 29       1        28     0.028       0.028 946.9165 529.5850   L 1000 area
#> 30       1        29     0.029       0.029 965.2754 547.6873   L 1000 area
#> 31       1        30     0.030       0.030 954.6646 532.5468   L 1000 area
#> 32       1        31     0.031       0.031 956.0162 535.1353   L 1000 area
#> 33       1        32     0.032       0.032 952.1043 536.4696   L 1000 area
#> 34       1        33     0.033       0.033 957.6986 539.3770   L 1000 area
#> 35       1        34     0.034       0.034 968.7718 556.1962   L 1000 area
#> 36       1        35     0.035       0.035 964.5373 552.4621   L 1000 area
#> 37       1        36     0.036       0.036 957.6754 560.0859   L 1000 area
#> 38       1        37     0.037       0.037 968.7001 521.4558   L 1000 area
#> 39       1        38     0.038       0.038 976.5600 535.3729   L 1000 area
#> 40       1        39     0.039       0.039 959.9363 550.4310   L 1000 area
#> 41       1        40     0.040       0.040 964.7049 536.9616   L 1000 area
#> 42       1        41     0.041       0.041 962.7822 543.1808   L 1000 area
#> 43       1        42     0.042       0.042 950.2210 533.5125   L 1000 area
#> 44       1        43     0.043       0.043 950.7341 521.3110   L 1000 area
#> 45       1        44     0.044       0.044 979.1977 527.9300   L 1000 area
#> 46       1        45     0.045       0.045 968.8128 558.9726   L 1000 area
#> 47       1        46     0.046       0.046 967.4208 537.3841   L 1000 area
#> 48       1        47     0.047       0.047 961.4757 557.8798   L 1000 area
#> 49       1        48     0.048       0.048 964.8539 548.6730   L 1000 area
#> 50       1        49     0.049       0.049 961.5186 547.0258   L 1000 area
#> 51       1        50     0.050       0.050 960.4200 556.2353   L 1000 area
#> 52       1        51     0.051       0.051 962.2342 524.0984   L 1000 area
#> 53       1        52     0.052       0.052 949.8953 533.9001   L 1000 area
#> 54       1        53     0.053       0.053 984.0122 536.8672   L 1000 area
#> 55       1        54     0.054       0.054 968.0196 530.7787   L 1000 area
#> 56       1        55     0.055       0.055 957.4879 537.4352   L 1000 area
#> 57       1        56     0.056       0.056 972.1289 519.1715   L 1000 area
#> 58       1        57     0.057       0.057 953.7274 525.8285   L 1000 area
#> 59       1        58     0.058       0.058 977.1116 546.1078   L 1000 area
#> 60       1        59     0.059       0.059 956.0563 542.4152   L 1000 area
#> 61       1        60     0.060       0.060 936.7851 553.0396   L 1000 area
#> 62       1        61     0.061       0.061 973.6412 536.6547   L 1000 area
#> 63       1        62     0.062       0.062 971.3223 536.1546   L 1000 area
#> 64       1        63     0.063       0.063 952.2568 535.7883   L 1000 area
#> 65       1        64     0.064       0.064 945.8963 530.4164   L 1000 area
#> 66       1        65     0.065       0.065 941.6547 542.4912   L 1000 area
#> 67       1        66     0.066       0.066 957.3099 528.2112   L 1000 area
#> 68       1        67     0.067       0.067 941.6607 544.9559   L 1000 area
#> 69       1        68     0.068       0.068 951.8553 538.2230   L 1000 area
#> 70       1        69     0.069       0.069 961.6357 547.7473   L 1000 area
#> 71       1        70     0.070       0.070 968.5552 552.4795   L 1000 area
#> 72       1        71     0.071       0.071 951.8004 535.8378   L 1000 area
#> 73       1        72     0.072       0.072 958.7640 535.7028   L 1000 area
#> 74       1        73     0.073       0.073 962.5495 539.5347   L 1000 area
#> 75       1        74     0.074       0.074 977.1893 533.4141   L 1000 area
#> 76       1        75     0.075       0.075 950.4146 556.0258   L 1000 area
#> 77       1        76     0.076       0.076 943.9569 539.3633   L 1000 area
#> 78       1        77     0.077       0.077 941.5439 548.6883   L 1000 area
#> 79       1        78     0.078       0.078 965.5574 543.7838   L 1000 area
#> 80       1        79     0.079       0.079 959.3988 554.1569   L 1000 area
#> 81       1        80     0.080       0.080 967.7209 537.9097   L 1000 area
#> 82       1        81     0.081       0.081 958.5916 513.2196   L 1000 area
#> 83       1        82     0.082       0.082 963.9309 546.0918   L 1000 area
#> 84       1        83     0.083       0.083 962.2422 529.6636   L 1000 area
#> 85       1        84     0.084       0.084 960.2354 544.9739   L 1000 area
#> 86       1        85     0.085       0.085 953.7704 530.2611   L 1000 area
#> 87       1        86     0.086       0.086 972.6201 539.0553   L 1000 area
#> 88       1        87     0.087       0.087 955.9423 534.4602   L 1000 area
#> 89       1        88     0.088       0.088 966.6676 537.5424   L 1000 area
#> 90       1        89     0.089       0.089 961.6464 533.4521   L 1000 area
#> 91       1        90     0.090       0.090 977.8152 530.1619   L 1000 area
#> 92       1        91     0.091       0.091 967.1121 541.9641   L 1000 area
#> 93       1        92     0.092       0.092 956.6231 527.8347   L 1000 area
#> 94       1        93     0.093       0.093 959.9085 537.1496   L 1000 area
#> 95       1        94     0.094       0.094 958.7469 536.9965   L 1000 area
#> 96       1        95     0.095       0.095 939.0915 533.6138   L 1000 area
#> 97       1        96     0.096       0.096 976.9739 557.4285   L 1000 area
#> 98       1        97     0.097       0.097 970.6388 521.3060   L 1000 area
#> 99       1        98     0.098       0.098 952.3338 540.3118   L 1000 area
#> 100      1        99     0.099       0.099 963.8201 527.9089   L 1000 area
#> 101      1       100     0.100       0.100 962.4190 526.5895   L 1000 area
#> 102      1       101     0.101       0.101 948.6724 539.5429   L 1000 area
#> 103      1       102     0.102       0.102 974.8991 561.8799   L 1000 area
#> 104      1       103     0.103       0.103 957.5175 554.2210   L 1000 area
#> 105      1       104     0.104       0.104 961.8358 541.8325   L 1000 area
#> 106      1       105     0.105       0.105 964.0487 533.4707   L 1000 area
#> 107      1       106     0.106       0.106 950.0588 560.4256   L 1000 area
#> 108      1       107     0.107       0.107 949.1457 537.6551   L 1000 area
#> 109      1       108     0.108       0.108 959.5146 531.0858   L 1000 area
#> 110      1       109     0.109       0.109 965.7609 526.4253   L 1000 area
#> 111      1       110     0.110       0.110 960.7383 534.1597   L 1000 area
#> 112      1       111     0.111       0.111 967.0595 556.6810   L 1000 area
#> 113      1       112     0.112       0.112 963.3498 550.8574   L 1000 area
#> 114      1       113     0.113       0.113 965.4539 545.5753   L 1000 area
#> 115      1       114     0.114       0.114 945.9709 523.6317   L 1000 area
#> 116      1       115     0.115       0.115 966.7705 537.2846   L 1000 area
#> 117      1       116     0.116       0.116 952.1020 550.9180   L 1000 area
#> 118      1       117     0.117       0.117 955.3427 536.8625   L 1000 area
#> 119      1       118     0.118       0.118 958.9515 533.1798   L 1000 area
#> 120      1       119     0.119       0.119 943.5215 540.5208   L 1000 area
#> 121      1       120     0.120       0.120 959.0046 532.9070   L 1000 area
#> 122      1       121     0.121       0.121 955.6014 536.4529   L 1000 area
#> 123      1       122     0.122       0.122 952.8149 563.9503   L 1000 area
#> 124      1       123     0.123       0.123 954.4540 545.5101   L 1000 area
#> 125      1       124     0.124       0.124 972.4549 552.4879   L 1000 area
#> 126      1       125     0.125       0.125 947.4108 540.4947   L 1000 area
#> 127      1       126     0.126       0.126 957.8462 535.4445   L 1000 area
#> 128      1       127     0.127       0.127 935.2804 546.5789   L 1000 area
#> 129      1       128     0.128       0.128 953.2583 525.1453   L 1000 area
#> 130      1       129     0.129       0.129 954.9870 554.6918   L 1000 area
#> 131      1       130     0.130       0.130 975.4233 537.7824   L 1000 area
#> 132      1       131     0.131       0.131 950.3798 509.7770   L 1000 area
#> 133      1       132     0.132       0.132 951.2782 531.3090   L 1000 area
#> 134      1       133     0.133       0.133 946.0237 535.8611   L 1000 area
#> 135      1       134     0.134       0.134 961.7981 559.8922   L 1000 area
#> 136      1       135     0.135       0.135 971.5409 539.2101   L 1000 area
#> 137      1       136     0.136       0.136 948.0147 525.1617   L 1000 area
#> 138      1       137     0.137       0.137 955.7428 537.1313   L 1000 area
#> 139      1       138     0.138       0.138 973.6631 530.9898   L 1000 area
#> 140      1       139     0.139       0.139 953.1570 524.2598   L 1000 area
#> 141      1       140     0.140       0.140 966.8551 533.3169   L 1000 area
#> 142      1       141     0.141       0.141 963.8950 521.4384   L 1000 area
#> 143      1       142     0.142       0.142 946.9460 566.8392   L 1000 area
#> 144      1       143     0.143       0.143 972.1689 542.7825   L 1000 area
#> 145      1       144     0.144       0.144 967.9517 546.2507   L 1000 area
#> 146      1       145     0.145       0.145 955.1180 528.4857   L 1000 area
#> 147      1       146     0.146       0.146 950.9601 552.9567   L 1000 area
#> 148      1       147     0.147       0.147 956.0958 545.2213   L 1000 area
#> 149      1       148     0.148       0.148 968.1406 530.6137   L 1000 area
#> 150      1       149     0.149       0.149 954.3575 538.9226   L 1000 area
#> 151      1       150     0.150       0.150 941.2579 536.0037   L 1000 area
#> 152      1       151     0.151       0.151 958.5710 552.8670   L 1000 area
#> 153      1       152     0.152       0.152 967.7190 538.3635   L 1000 area
#> 154      1       153     0.153       0.153 948.4088 550.8523   L 1000 area
#> 155      1       154     0.154       0.154 957.6208 547.2983   L 1000 area
#> 156      1       155     0.155       0.155 947.7781 537.0082   L 1000 area
#> 157      1       156     0.156       0.156 961.1681 539.5008   L 1000 area
#> 158      1       157     0.157       0.157 958.6750 531.3759   L 1000 area
#> 159      1       158     0.158       0.158 959.6632 535.0200   L 1000 area
#> 160      1       159     0.159       0.159 953.7767 526.0408   L 1000 area
#> 161      1       160     0.160       0.160 952.9064 535.7562   L 1000 area
#> 162      1       161     0.161       0.161 968.7145 536.3511   L 1000 area
#> 163      1       162     0.162       0.162 961.0514 537.8192   L 1000 area
#> 164      1       163     0.163       0.163 958.1306 564.9935   L 1000 area
#> 165      1       164     0.164       0.164 927.8681 541.4517   L 1000 area
#> 166      1       165     0.165       0.165 947.2438 525.2220   L 1000 area
#> 167      1       166     0.166       0.166 967.6291 549.2924   L 1000 area
#> 168      1       167     0.167       0.167 955.9318 537.1198   L 1000 area
#> 169      1       168     0.168       0.168 947.9168 554.0937   L 1000 area
#> 170      1       169     0.169       0.169 955.6068 540.5377   L 1000 area
#> 171      1       170     0.170       0.170 956.2442 556.0677   L 1000 area
#> 172      1       171     0.171       0.171 954.9856 539.8277   L 1000 area
#> 173      1       172     0.172       0.172 964.9661 534.5736   L 1000 area
#> 174      1       173     0.173       0.173 975.2088 559.7869   L 1000 area
#> 175      1       174     0.174       0.174 969.8809 526.3389   L 1000 area
#> 176      1       175     0.175       0.175 972.4612 544.2490   L 1000 area
#> 177      1       176     0.176       0.176 956.7013 530.2369   L 1000 area
#> 178      1       177     0.177       0.177 968.4434 551.9297   L 1000 area
#> 179      1       178     0.178       0.178 950.1892 544.4096   L 1000 area
#> 180      1       179     0.179       0.179 958.6078 535.7090   L 1000 area
#> 181      1       180     0.180       0.180 981.8544 536.2234   L 1000 area
#> 182      1       181     0.181       0.181 959.8717 548.8870   L 1000 area
#> 183      1       182     0.182       0.182 956.9469 529.3499   L 1000 area
#> 184      1       183     0.183       0.183 954.1579 532.2273   L 1000 area
#> 185      1       184     0.184       0.184 967.7127 544.0786   L 1000 area
#> 186      1       185     0.185       0.185 981.0619 547.5027   L 1000 area
#> 187      1       186     0.186       0.186 964.1216 522.8500   L 1000 area
#> 188      1       187     0.187       0.187 957.3874 541.9225   L 1000 area
#> 189      1       188     0.188       0.188 980.7378 528.3827   L 1000 area
#> 190      1       189     0.189       0.189 952.2117 533.5817   L 1000 area
#> 191      1       190     0.190       0.190 971.3153 553.1244   L 1000 area
#> 192      1       191     0.191       0.191 955.7865 550.0154   L 1000 area
#> 193      1       192     0.192       0.192 949.7825 538.0647   L 1000 area
#> 194      1       193     0.193       0.193 972.1831 530.6682   L 1000 area
#> 195      1       194     0.194       0.194 942.0024 543.1033   L 1000 area
#> 196      1       195     0.195       0.195 956.9175 555.8332   L 1000 area
#> 197      1       196     0.196       0.196 960.1552 549.0615   L 1000 area
#> 198      1       197     0.197       0.197 955.5768 544.0114   L 1000 area
#> 199      1       198     0.198       0.198 943.6199 540.7570   L 1000 area
#> 200      1       199     0.199       0.199 953.5860 550.5949   L 1000 area
#> 201      1       200     0.200       0.200 944.4296 549.1694   L 1000 area
#> 202      1       201     0.201       0.201 979.2316 548.0927   L 1000 area
#> 203      1       202     0.202       0.202 941.4317 532.8838   L 1000 area
#> 204      1       203     0.203       0.203 938.9388 513.1041   L 1000 area
#> 205      1       204     0.204       0.204 966.9765 534.3295   L 1000 area
#> 206      1       205     0.205       0.205 969.0744 552.9920   L 1000 area
#> 207      1       206     0.206       0.206 958.0401 538.0971   L 1000 area
#> 208      1       207     0.207       0.207 957.9318 533.6195   L 1000 area
#> 209      1       208     0.208       0.208 967.2504 519.6462   L 1000 area
#> 210      1       209     0.209       0.209 973.9872 533.9563   L 1000 area
#> 211      1       210     0.210       0.210 944.0944 531.9184   L 1000 area
#> 212      1       211     0.211       0.211 973.0450 533.9322   L 1000 area
#> 213      1       212     0.212       0.212 961.9601 540.8691   L 1000 area
#> 214      1       213     0.213       0.213 956.6066 537.6570   L 1000 area
#> 215      1       214     0.214       0.214 972.6514 552.6253   L 1000 area
#> 216      1       215     0.215       0.215 969.3977 524.3620   L 1000 area
#> 217      1       216     0.216       0.216 972.7795 525.0653   L 1000 area
#> 218      1       217     0.217       0.217 957.0884 529.0287   L 1000 area
#> 219      1       218     0.218       0.218 967.7617 539.7089   L 1000 area
#> 220      1       219     0.219       0.219 962.9573 561.7368   L 1000 area
#> 221      1       220     0.220       0.220 954.8280 553.6979   L 1000 area
#> 222      1       221     0.221       0.221 977.5723 536.2162   L 1000 area
#> 223      1       222     0.222       0.222 961.6173 532.2126   L 1000 area
#> 224      1       223     0.223       0.223 959.4532 552.6409   L 1000 area
#> 225      1       224     0.224       0.224 965.2897 543.0801   L 1000 area
#> 226      1       225     0.225       0.225 963.8973 535.8150   L 1000 area
#> 227      1       226     0.226       0.226 952.9423 539.6236   L 1000 area
#> 228      1       227     0.227       0.227 959.4682 551.8730   L 1000 area
#> 229      1       228     0.228       0.228 986.0644 538.9434   L 1000 area
#> 230      1       229     0.229       0.229 959.2741 542.0244   L 1000 area
#> 231      1       230     0.230       0.230 957.0373 547.8041   L 1000 area
#> 232      1       231     0.231       0.231 967.3433 534.5786   L 1000 area
#> 233      1       232     0.232       0.232 964.0148 519.7347   L 1000 area
#> 234      1       233     0.233       0.233 954.6170 539.7833   L 1000 area
#> 235      1       234     0.234       0.234 949.1728 552.2228   L 1000 area
#> 236      1       235     0.235       0.235 968.4780 538.2632   L 1000 area
#> 237      1       236     0.236       0.236 961.1885 546.6401   L 1000 area
#> 238      1       237     0.237       0.237 964.8225 534.5216   L 1000 area
#> 239      1       238     0.238       0.238 957.0868 530.8256   L 1000 area
#> 240      1       239     0.239       0.239 943.4789 544.3711   L 1000 area
#> 241      1       240     0.240       0.240 960.6909 536.1503   L 1000 area
#> 242      1       241     0.241       0.241 983.9058 548.4238   L 1000 area
#> 243      1       242     0.242       0.242 945.3823 549.5302   L 1000 area
#> 244      1       243     0.243       0.243 950.1519 535.8937   L 1000 area
#> 245      1       244     0.244       0.244 946.6395 541.0081   L 1000 area
#> 246      1       245     0.245       0.245 956.3487 549.2076   L 1000 area
#> 247      1       246     0.246       0.246 973.8145 554.5960   L 1000 area
#> 248      1       247     0.247       0.247 958.4668 536.0316   L 1000 area
#> 249      1       248     0.248       0.248 957.4443 535.1692   L 1000 area
#> 250      1       249     0.249       0.249 947.1137 536.8378   L 1000 area
#> 251      1       250     0.250       0.250 960.6527 552.1000   L 1000 area
#> 252      1       251     0.251       0.251 970.3533 532.4702   L 1000 area
#> 253      1       252     0.252       0.252 982.6022 533.5073   L 1000 area
#> 254      1       253     0.253       0.253 973.1470 537.4576   L 1000 area
#> 255      1       254     0.254       0.254 951.2998 527.0363   L 1000 area
#> 256      1       255     0.255       0.255 954.9687 535.7534   L 1000 area
#> 257      1       256     0.256       0.256 966.0746 536.0084   L 1000 area
#> 258      1       257     0.257       0.257 959.8996 540.5987   L 1000 area
#> 259      1       258     0.258       0.258 962.7533 546.8068   L 1000 area
#> 260      1       259     0.259       0.259 946.8770 555.6942   L 1000 area
#> 261      1       260     0.260       0.260 955.7347 548.1131   L 1000 area
#> 262      1       261     0.261       0.261 950.7360 527.2873   L 1000 area
#> 263      1       262     0.262       0.262 950.7144 555.0468   L 1000 area
#> 264      1       263     0.263       0.263 952.8110 548.0699   L 1000 area
#> 265      1       264     0.264       0.264 955.8521 540.6458   L 1000 area
#> 266      1       265     0.265       0.265 959.9433 553.4405   L 1000 area
#> 267      1       266     0.266       0.266 966.3345 545.4190   L 1000 area
#> 268      1       267     0.267       0.267 954.9082 538.4593   L 1000 area
#> 269      1       268     0.268       0.268 951.4299 534.1767   L 1000 area
#> 270      1       269     0.269       0.269 976.1617 548.8659   L 1000 area
#> 271      1       270     0.270       0.270 969.9387 517.8884   L 1000 area
#> 272      1       271     0.271       0.271 966.9685 538.4511   L 1000 area
#> 273      1       272     0.272       0.272 976.9997 528.7289   L 1000 area
#> 274      1       273     0.273       0.273 950.2226 519.7988   L 1000 area
#> 275      1       274     0.274       0.274 980.4882 542.2263   L 1000 area
#> 276      1       275     0.275       0.275 972.2645 538.7210   L 1000 area
#> 277      1       276     0.276       0.276 963.0702 519.7595   L 1000 area
#> 278      1       277     0.277       0.277 966.2433 537.2032   L 1000 area
#> 279      1       278     0.278       0.278 960.6134 544.2063   L 1000 area
#> 280      1       279     0.279       0.279 958.8917 546.4288   L 1000 area
#> 281      1       280     0.280       0.280 944.2377 542.9438   L 1000 area
#> 282      1       281     0.281       0.281 967.4259 547.1914   L 1000 area
#> 283      1       282     0.282       0.282 981.4232 541.9250   L 1000 area
#> 284      1       283     0.283       0.283 980.8977 543.6744   L 1000 area
#> 285      1       284     0.284       0.284 961.6979 538.8660   L 1000 area
#> 286      1       285     0.285       0.285 958.9212 529.6341   L 1000 area
#> 287      1       286     0.286       0.286 961.8199 542.5259   L 1000 area
#> 288      1       287     0.287       0.287 971.4593 549.2255   L 1000 area
#> 289      1       288     0.288       0.288 974.7730 562.7494   L 1000 area
#> 290      1       289     0.289       0.289 964.0794 535.5856   L 1000 area
#> 291      1       290     0.290       0.290 971.1428 540.1175   L 1000 area
#> 292      1       291     0.291       0.291 959.7292 518.7301   L 1000 area
#> 293      1       292     0.292       0.292 964.9772 532.0789   L 1000 area
#> 294      1       293     0.293       0.293 971.8564 552.8882   L 1000 area
#> 295      1       294     0.294       0.294 996.3957 537.4566   L 1000 area
#> 296      1       295     0.295       0.295 959.4600 545.6889   L 1000 area
#> 297      1       296     0.296       0.296 953.3185 546.6745   L 1000 area
#> 298      1       297     0.297       0.297 964.4581 530.5038   L 1000 area
#> 299      1       298     0.298       0.298 955.9417 540.1551   L 1000 area
#> 300      1       299     0.299       0.299 966.3528 542.3253   L 1000 area
#> 301      1       300     0.300       0.300 963.4125 536.8831   L 1000 area
#> 302      1       301     0.301       0.301 973.1617 542.0575   L 1000 area
#> 303      1       302     0.302       0.302 950.4022 533.4601   L 1000 area
#> 304      1       303     0.303       0.303 947.9442 528.4674   L 1000 area
#> 305      1       304     0.304       0.304 975.6757 545.2749   L 1000 area
#> 306      1       305     0.305       0.305 962.2529 553.9392   L 1000 area
#> 307      1       306     0.306       0.306 950.7759 541.8163   L 1000 area
#> 308      1       307     0.307       0.307 949.2623 539.3070   L 1000 area
#> 309      1       308     0.308       0.308 954.4764 537.0553   L 1000 area
#> 310      1       309     0.309       0.309 965.9046 554.1403   L 1000 area
#> 311      1       310     0.310       0.310 954.7272 551.0000   L 1000 area
#> 312      1       311     0.311       0.311 973.4185 554.0441   L 1000 area
#> 313      1       312     0.312       0.312 962.4384 546.2313   L 1000 area
#> 314      1       313     0.313       0.313 961.9588 542.7394   L 1000 area
#> 315      1       314     0.314       0.314 959.8448 544.9965   L 1000 area
#> 316      1       315     0.315       0.315 957.0242 542.6334   L 1000 area
#> 317      1       316     0.316       0.316 958.3232 527.9988   L 1000 area
#> 318      1       317     0.317       0.317 971.5648 561.5178   L 1000 area
#> 319      1       318     0.318       0.318 946.6124 528.5453   L 1000 area
#> 320      1       319     0.319       0.319 970.8677 549.5231   L 1000 area
#> 321      1       320     0.320       0.320 956.4932 527.6408   L 1000 area
#> 322      1       321     0.321       0.321 967.3472 531.0615   L 1000 area
#> 323      1       322     0.322       0.322 959.4159 532.9109   L 1000 area
#> 324      1       323     0.323       0.323 946.5419 528.0234   L 1000 area
#> 325      1       324     0.324       0.324 963.4691 539.2991   L 1000 area
#> 326      1       325     0.325       0.325 953.6160 531.5077   L 1000 area
#> 327      1       326     0.326       0.326 971.2439 541.5686   L 1000 area
#> 328      1       327     0.327       0.327 951.7227 560.1742   L 1000 area
#> 329      1       328     0.328       0.328 942.1222 547.3759   L 1000 area
#> 330      1       329     0.329       0.329 951.4599 523.0841   L 1000 area
#> 331      1       330     0.330       0.330 954.2798 544.7772   L 1000 area
#> 332      1       331     0.331       0.331 951.7407 540.7468   L 1000 area
#> 333      1       332     0.332       0.332 957.6242 559.4243   L 1000 area
#> 334      1       333     0.333       0.333 954.0467 545.0515   L 1000 area
#> 335      1       334     0.334       0.334 955.8926 537.4154   L 1000 area
#> 336      1       335     0.335       0.335 963.2799 527.3753   L 1000 area
#> 337      1       336     0.336       0.336 948.3635 540.4679   L 1000 area
#> 338      1       337     0.337       0.337 967.6018 551.8997   L 1000 area
#> 339      1       338     0.338       0.338 959.9744 531.0597   L 1000 area
#> 340      1       339     0.339       0.339 959.6177 532.3471   L 1000 area
#> 341      1       340     0.340       0.340 939.6376 549.8453   L 1000 area
#> 342      1       341     0.341       0.341 956.9877 549.1677   L 1000 area
#> 343      1       342     0.342       0.342 954.4435 537.1046   L 1000 area
#> 344      1       343     0.343       0.343 968.4730 535.1249   L 1000 area
#> 345      1       344     0.344       0.344 973.9082 537.2817   L 1000 area
#> 346      1       345     0.345       0.345 972.3125 551.0622   L 1000 area
#> 347      1       346     0.346       0.346 948.8666 540.5862   L 1000 area
#> 348      1       347     0.347       0.347 952.0395 542.5866   L 1000 area
#> 349      1       348     0.348       0.348 959.1048 556.3141   L 1000 area
#> 350      1       349     0.349       0.349 949.4034 548.6937   L 1000 area
#> 351      1       350     0.350       0.350 943.9569 552.1704   L 1000 area
#> 352      1       351     0.351       0.351 967.9135 557.2040   L 1000 area
#> 353      1       352     0.352       0.352 960.6816 536.8722   L 1000 area
#> 354      1       353     0.353       0.353 966.1440 538.9782   L 1000 area
#> 355      1       354     0.354       0.354 947.9693 541.6720   L 1000 area
#> 356      1       355     0.355       0.355 956.5955 544.5661   L 1000 area
#> 357      1       356     0.356       0.356 948.2875 534.9843   L 1000 area
#> 358      1       357     0.357       0.357 951.7059 542.2694   L 1000 area
#> 359      1       358     0.358       0.358 962.2921 532.9137   L 1000 area
#> 360      1       359     0.359       0.359 951.8501 543.5349   L 1000 area
#> 361      1       360     0.360       0.360 950.0655 530.5280   L 1000 area
#> 362      1       361     0.361       0.361 946.9383 537.6765   L 1000 area
#> 363      1       362     0.362       0.362 945.6578 553.9156   L 1000 area
#> 364      1       363     0.363       0.363 951.3715 536.5578   L 1000 area
#> 365      1       364     0.364       0.364 977.0380 533.0258   L 1000 area
#> 366      1       365     0.365       0.365 953.4437 545.8541   L 1000 area
#> 367      1       366     0.366       0.366 948.8683 510.0490   L 1000 area
#> 368      1       367     0.367       0.367 945.9449 541.0199   L 1000 area
#> 369      1       368     0.368       0.368 958.4936 542.0676   L 1000 area
#> 370      1       369     0.369       0.369 956.0599 563.4443   L 1000 area
#> 371      1       370     0.370       0.370 962.5207 534.3912   L 1000 area
#> 372      1       371     0.371       0.371 957.1729 531.9778   L 1000 area
#> 373      1       372     0.372       0.372 968.6767 562.8543   L 1000 area
#> 374      1       373     0.373       0.373 967.2009 543.6724   L 1000 area
#> 375      1       374     0.374       0.374 970.5607 539.9413   L 1000 area
#> 376      1       375     0.375       0.375 962.4350 545.1208   L 1000 area
#> 377      1       376     0.376       0.376 971.1436 535.5262   L 1000 area
#> 378      1       377     0.377       0.377 959.2345 527.8897   L 1000 area
#> 379      1       378     0.378       0.378 969.3293 547.7095   L 1000 area
#> 380      1       379     0.379       0.379 958.4202 540.8901   L 1000 area
#> 381      1       380     0.380       0.380 953.1849 539.0108   L 1000 area
#> 382      1       381     0.381       0.381 947.7990 526.8428   L 1000 area
#> 383      1       382     0.382       0.382 952.9109 532.0438   L 1000 area
#> 384      1       383     0.383       0.383 952.5132 544.8238   L 1000 area
#> 385      1       384     0.384       0.384 965.7608 530.4837   L 1000 area
#> 386      1       385     0.385       0.385 959.4790 544.9954   L 1000 area
#> 387      1       386     0.386       0.386 953.6966 552.4215   L 1000 area
#> 388      1       387     0.387       0.387 951.0166 549.4728   L 1000 area
#> 389      1       388     0.388       0.388 975.1269 526.7125   L 1000 area
#> 390      1       389     0.389       0.389 957.6325 541.2696   L 1000 area
#> 391      1       390     0.390       0.390 947.5120 540.0992   L 1000 area
#> 392      1       391     0.391       0.391 955.6999 549.0496   L 1000 area
#> 393      1       392     0.392       0.392 960.9330 543.8322   L 1000 area
#> 394      1       393     0.393       0.393 968.3304 526.9157   L 1000 area
#> 395      1       394     0.394       0.394 959.1507 536.6164   L 1000 area
#> 396      1       395     0.395       0.395 961.1185 551.2719   L 1000 area
#> 397      1       396     0.396       0.396 949.6056 549.3473   L 1000 area
#> 398      1       397     0.397       0.397 967.9642 558.3060   L 1000 area
#> 399      1       398     0.398       0.398 960.9990 530.1927   L 1000 area
#> 400      1       399     0.399       0.399 967.3696 533.2925   L 1000 area
#> 401      1       400     0.400       0.400 975.4688 536.2567   L 1000 area
#> 402      1       401     0.401       0.401 961.7892 549.9535   L 1000 area
#> 403      1       402     0.402       0.402 957.1745 541.0214   L 1000 area
#> 404      1       403     0.403       0.403 952.3270 554.8294   L 1000 area
#> 405      1       404     0.404       0.404 954.2360 545.6005   L 1000 area
#> 406      1       405     0.405       0.405 950.8514 541.4245   L 1000 area
#> 407      1       406     0.406       0.406 963.6991 558.2967   L 1000 area
#> 408      1       407     0.407       0.407 945.3232 545.1840   L 1000 area
#> 409      1       408     0.408       0.408 942.5464 546.9187   L 1000 area
#> 410      1       409     0.409       0.409 953.3262 540.2801   L 1000 area
#> 411      1       410     0.410       0.410 970.8428 538.3495   L 1000 area
#> 412      1       411     0.411       0.411 961.2283 532.7896   L 1000 area
#> 413      1       412     0.412       0.412 955.0575 527.1817   L 1000 area
#> 414      1       413     0.413       0.413 966.6959 537.7715   L 1000 area
#> 415      1       414     0.414       0.414 956.4852 540.2732   L 1000 area
#> 416      1       415     0.415       0.415 958.8502 558.2763   L 1000 area
#> 417      1       416     0.416       0.416 960.7299 551.7841   L 1000 area
#> 418      1       417     0.417       0.417 970.8877 536.4369   L 1000 area
#> 419      1       418     0.418       0.418 972.9257 549.3379   L 1000 area
#> 420      1       419     0.419       0.419 972.8251 529.1466   L 1000 area
#> 421      1       420     0.420       0.420 969.7906 541.1981   L 1000 area
#> 422      1       421     0.421       0.421 963.2947 544.7176   L 1000 area
#> 423      1       422     0.422       0.422 978.7964 523.0261   L 1000 area
#> 424      1       423     0.423       0.423 980.4290 551.9271   L 1000 area
#> 425      1       424     0.424       0.424 973.2858 533.1220   L 1000 area
#> 426      1       425     0.425       0.425 957.8033 521.8954   L 1000 area
#> 427      1       426     0.426       0.426 949.5995 527.5730   L 1000 area
#> 428      1       427     0.427       0.427 961.9295 520.2924   L 1000 area
#> 429      1       428     0.428       0.428 954.4476 535.8796   L 1000 area
#> 430      1       429     0.429       0.429 962.7499 548.2580   L 1000 area
#> 431      1       430     0.430       0.430 954.4949 534.3844   L 1000 area
#> 432      1       431     0.431       0.431 969.2126 527.7318   L 1000 area
#> 433      1       432     0.432       0.432 981.8946 536.6642   L 1000 area
#> 434      1       433     0.433       0.433 969.6399 540.0617   L 1000 area
#> 435      1       434     0.434       0.434 967.4182 542.4656   L 1000 area
#> 436      1       435     0.435       0.435 960.3736 544.4757   L 1000 area
#> 437      1       436     0.436       0.436 981.8083 537.5498   L 1000 area
#> 438      1       437     0.437       0.437 959.2041 539.2357   L 1000 area
#> 439      1       438     0.438       0.438 973.7630 551.2305   L 1000 area
#> 440      1       439     0.439       0.439 975.7532 555.8075   L 1000 area
#> 441      1       440     0.440       0.440 961.6038 531.7214   L 1000 area
#> 442      1       441     0.441       0.441 927.4678 544.9121   L 1000 area
#> 443      1       442     0.442       0.442 956.7007 540.1858   L 1000 area
#> 444      1       443     0.443       0.443 964.5709 553.2095   L 1000 area
#> 445      1       444     0.444       0.444 946.9590 549.3023   L 1000 area
#> 446      1       445     0.445       0.445 947.0883 550.7622   L 1000 area
#> 447      1       446     0.446       0.446 969.0638 536.9973   L 1000 area
#> 448      1       447     0.447       0.447 932.0404 550.4474   L 1000 area
#> 449      1       448     0.448       0.448 962.3356 547.0914   L 1000 area
#> 450      1       449     0.449       0.449 957.3096 527.3059   L 1000 area
#> 451      1       450     0.450       0.450 953.4145 530.6791   L 1000 area
#> 452      1       451     0.451       0.451 960.6335 553.7361   L 1000 area
#> 453      1       452     0.452       0.452 960.5448 535.3895   L 1000 area
#> 454      1       453     0.453       0.453 962.4701 553.1570   L 1000 area
#> 455      1       454     0.454       0.454 961.2404 546.5893   L 1000 area
#> 456      1       455     0.455       0.455 954.6474 529.2901   L 1000 area
#> 457      1       456     0.456       0.456 962.5848 534.6101   L 1000 area
#> 458      1       457     0.457       0.457 973.8771 527.0261   L 1000 area
#> 459      1       458     0.458       0.458 966.6717 541.9023   L 1000 area
#> 460      1       459     0.459       0.459 974.9705 504.6041   L 1000 area
#> 461      1       460     0.460       0.460 957.2512 552.1140   L 1000 area
#> 462      1       461     0.461       0.461 965.8308 542.9088   L 1000 area
#> 463      1       462     0.462       0.462 965.0336 548.1477   L 1000 area
#> 464      1       463     0.463       0.463 952.5675 537.7766   L 1000 area
#> 465      1       464     0.464       0.464 951.8312 554.3483   L 1000 area
#> 466      1       465     0.465       0.465 978.7028 541.8961   L 1000 area
#> 467      1       466     0.466       0.466 951.7275 530.1841   L 1000 area
#> 468      1       467     0.467       0.467 969.5071 539.2170   L 1000 area
#> 469      1       468     0.468       0.468 957.3383 549.0074   L 1000 area
#> 470      1       469     0.469       0.469 949.0092 525.5390   L 1000 area
#> 471      1       470     0.470       0.470 944.6578 539.2852   L 1000 area
#> 472      1       471     0.471       0.471 947.9369 532.0386   L 1000 area
#> 473      1       472     0.472       0.472 940.4997 524.5456   L 1000 area
#> 474      1       473     0.473       0.473 951.1596 560.9620   L 1000 area
#> 475      1       474     0.474       0.474 969.7341 541.6735   L 1000 area
#> 476      1       475     0.475       0.475 954.1903 545.1410   L 1000 area
#> 477      1       476     0.476       0.476 977.3677 541.2539   L 1000 area
#> 478      1       477     0.477       0.477 958.2732 524.1875   L 1000 area
#> 479      1       478     0.478       0.478 950.6201 540.7304   L 1000 area
#> 480      1       479     0.479       0.479 957.1541 553.4770   L 1000 area
#> 481      1       480     0.480       0.480 954.1098 553.7540   L 1000 area
#> 482      1       481     0.481       0.481 955.5607 534.8292   L 1000 area
#> 483      1       482     0.482       0.482 960.6000 555.1583   L 1000 area
#> 484      1       483     0.483       0.483 960.8223 554.9913   L 1000 area
#> 485      1       484     0.484       0.484 967.5840 548.1458   L 1000 area
#> 486      1       485     0.485       0.485 965.3966 556.6565   L 1000 area
#> 487      1       486     0.486       0.486 967.7665 550.1497   L 1000 area
#> 488      1       487     0.487       0.487 961.3217 545.2716   L 1000 area
#> 489      1       488     0.488       0.488 951.9313 520.5506   L 1000 area
#> 490      1       489     0.489       0.489 964.4935 546.8831   L 1000 area
#> 491      1       490     0.490       0.490 964.1264 540.7633   L 1000 area
#> 492      1       491     0.491       0.491 955.9639 531.6250   L 1000 area
#> 493      1       492     0.492       0.492 956.1191 538.4981   L 1000 area
#> 494      1       493     0.493       0.493 970.9519 530.0379   L 1000 area
#> 495      1       494     0.494       0.494 973.4048 552.1660   L 1000 area
#> 496      1       495     0.495       0.495 961.1441 538.2816   L 1000 area
#> 497      1       496     0.496       0.496 956.0774 526.6604   L 1000 area
#> 498      1       497     0.497       0.497 976.5236 541.4156   L 1000 area
#> 499      1       498     0.498       0.498 947.5630 527.0859   L 1000 area
#> 500      1       499     0.499       0.499 950.8807 526.6751   L 1000 area
#> 501      1       500     0.500       0.500 968.5004 521.9452   L 1000 area
#> 502      1       501     0.501       0.501 950.7469 533.2196   L 1000 area
#> 503      1       502     0.502       0.502 968.9358 535.2664   L 1000 area
#> 504      1       503     0.503       0.503 950.5899 550.2742   L 1000 area
#> 505      1       504     0.504       0.504 965.3895 534.0261   L 1000 area
#> 506      1       505     0.505       0.505 958.1803 551.5985   L 1000 area
#> 507      1       506     0.506       0.506 968.9177 526.6677   L 1000 area
#> 508      1       507     0.507       0.507 973.2921 530.7424   L 1000 area
#> 509      1       508     0.508       0.508 958.9653 529.2550   L 1000 area
#> 510      1       509     0.509       0.509 966.1506 525.4888   L 1000 area
#> 511      1       510     0.510       0.510 942.0023 534.1127   L 1000 area
#> 512      1       511     0.511       0.511 957.3729 538.8462   L 1000 area
#> 513      1       512     0.512       0.512 947.8986 532.7963   L 1000 area
#> 514      1       513     0.513       0.513 962.0448 540.0330   L 1000 area
#> 515      1       514     0.514       0.514 959.8601 549.8736   L 1000 area
#> 516      1       515     0.515       0.515 957.6480 549.5351   L 1000 area
#> 517      1       516     0.516       0.516 958.1168 522.9130   L 1000 area
#> 518      1       517     0.517       0.517 969.5128 542.9340   L 1000 area
#> 519      1       518     0.518       0.518 958.4599 528.5100   L 1000 area
#> 520      1       519     0.519       0.519 978.5424 538.2471   L 1000 area
#> 521      1       520     0.520       0.520 940.9583 540.3515   L 1000 area
#> 522      1       521     0.521       0.521 963.5930 533.3321   L 1000 area
#> 523      1       522     0.522       0.522 981.1453 531.8431   L 1000 area
#> 524      1       523     0.523       0.523 948.6425 541.2188   L 1000 area
#> 525      1       524     0.524       0.524 952.0374 540.6535   L 1000 area
#> 526      1       525     0.525       0.525 956.0243 522.7365   L 1000 area
#> 527      1       526     0.526       0.526 960.9172 530.6441   L 1000 area
#> 528      1       527     0.527       0.527 957.6131 552.9865   L 1000 area
#> 529      1       528     0.528       0.528 955.3804 534.1002   L 1000 area
#> 530      1       529     0.529       0.529 979.2634 545.5529   L 1000 area
#> 531      1       530     0.530       0.530 961.2205 538.0506   L 1000 area
#> 532      1       531     0.531       0.531 970.9205 533.1187   L 1000 area
#> 533      1       532     0.532       0.532 930.2774 540.8270   L 1000 area
#> 534      1       533     0.533       0.533 971.2849 541.6815   L 1000 area
#> 535      1       534     0.534       0.534 973.1200 548.0555   L 1000 area
#> 536      1       535     0.535       0.535 955.1739 537.8780   L 1000 area
#> 537      1       536     0.536       0.536 957.7514 531.4757   L 1000 area
#> 538      1       537     0.537       0.537 976.6777 527.0333   L 1000 area
#> 539      1       538     0.538       0.538 960.4122 514.5534   L 1000 area
#> 540      1       539     0.539       0.539 970.3817 545.3626   L 1000 area
#> 541      1       540     0.540       0.540 965.8764 540.2347   L 1000 area
#> 542      1       541     0.541       0.541 958.4170 550.4693   L 1000 area
#> 543      1       542     0.542       0.542 957.0791 534.9360   L 1000 area
#> 544      1       543     0.543       0.543 945.8010 546.4796   L 1000 area
#> 545      1       544     0.544       0.544 938.6168 544.8572   L 1000 area
#> 546      1       545     0.545       0.545 959.7750 527.9300   L 1000 area
#> 547      1       546     0.546       0.546 962.6977 539.9703   L 1000 area
#> 548      1       547     0.547       0.547 954.9703 555.8686   L 1000 area
#> 549      1       548     0.548       0.548 966.3081 530.0678   L 1000 area
#> 550      1       549     0.549       0.549 964.7457 540.6002   L 1000 area
#> 551      1       550     0.550       0.550 956.5934 546.5741   L 1000 area
#> 552      1       551     0.551       0.551 960.4960 537.6689   L 1000 area
#> 553      1       552     0.552       0.552 972.9526 547.4568   L 1000 area
#> 554      1       553     0.553       0.553 963.1462 545.4108   L 1000 area
#> 555      1       554     0.554       0.554 968.0157 531.6484   L 1000 area
#> 556      1       555     0.555       0.555 964.8606 541.3481   L 1000 area
#> 557      1       556     0.556       0.556 977.2556 537.1558   L 1000 area
#> 558      1       557     0.557       0.557 961.0243 544.6668   L 1000 area
#> 559      1       558     0.558       0.558 950.1016 528.9358   L 1000 area
#> 560      1       559     0.559       0.559 955.3956 538.9218   L 1000 area
#> 561      1       560     0.560       0.560 962.3766 540.5024   L 1000 area
#> 562      1       561     0.561       0.561 957.3060 543.1088   L 1000 area
#> 563      1       562     0.562       0.562 958.3045 541.7889   L 1000 area
#> 564      1       563     0.563       0.563 966.6487 529.4022   L 1000 area
#> 565      1       564     0.564       0.564 952.2445 530.2637   L 1000 area
#> 566      1       565     0.565       0.565 940.8074 534.3866   L 1000 area
#> 567      1       566     0.566       0.566 980.1933 527.1710   L 1000 area
#> 568      1       567     0.567       0.567 964.3521 559.8664   L 1000 area
#> 569      1       568     0.568       0.568 970.5017 540.7118   L 1000 area
#> 570      1       569     0.569       0.569 956.8789 535.9791   L 1000 area
#> 571      1       570     0.570       0.570 960.9362 538.0423   L 1000 area
#> 572      1       571     0.571       0.571 938.4681 536.0678   L 1000 area
#> 573      1       572     0.572       0.572 963.5799 539.0986   L 1000 area
#> 574      1       573     0.573       0.573 964.6786 538.0669   L 1000 area
#> 575      1       574     0.574       0.574 950.9331 533.6770   L 1000 area
#> 576      1       575     0.575       0.575 954.7065 536.2832   L 1000 area
#> 577      1       576     0.576       0.576 973.4358 532.6741   L 1000 area
#> 578      1       577     0.577       0.577 964.6943 531.8671   L 1000 area
#> 579      1       578     0.578       0.578 957.4689 545.8775   L 1000 area
#> 580      1       579     0.579       0.579 963.6910 534.6788   L 1000 area
#> 581      1       580     0.580       0.580 946.5329 554.7014   L 1000 area
#> 582      1       581     0.581       0.581 971.7194 541.2605   L 1000 area
#> 583      1       582     0.582       0.582 940.9126 548.7762   L 1000 area
#> 584      1       583     0.583       0.583 949.9561 535.2254   L 1000 area
#> 585      1       584     0.584       0.584 966.7669 517.5707   L 1000 area
#> 586      1       585     0.585       0.585 971.5232 540.1562   L 1000 area
#> 587      1       586     0.586       0.586 942.6724 551.3217   L 1000 area
#> 588      1       587     0.587       0.587 966.0131 539.2459   L 1000 area
#> 589      1       588     0.588       0.588 963.4165 535.5629   L 1000 area
#> 590      1       589     0.589       0.589 959.6472 542.6008   L 1000 area
#> 591      1       590     0.590       0.590 953.7383 540.1334   L 1000 area
#> 592      1       591     0.591       0.591 951.4403 530.3612   L 1000 area
#> 593      1       592     0.592       0.592 961.5178 556.5731   L 1000 area
#> 594      1       593     0.593       0.593 972.0637 537.7493   L 1000 area
#> 595      1       594     0.594       0.594 945.1142 524.8143   L 1000 area
#> 596      1       595     0.595       0.595 949.4963 526.6442   L 1000 area
#> 597      1       596     0.596       0.596 976.3787 545.2953   L 1000 area
#> 598      1       597     0.597       0.597 947.9267 553.9326   L 1000 area
#> 599      1       598     0.598       0.598 933.7384 551.2663   L 1000 area
#> 600      1       599     0.599       0.599 960.1746 539.8864   L 1000 area
#> 601      1       600     0.600       0.600 963.4419 549.4034   L 1000 area
#> 602      1       601     0.601       0.601 960.1272 547.8786   L 1000 area
#> 603      1       602     0.602       0.602 951.2655 540.8694   L 1000 area
#> 604      1       603     0.603       0.603 963.4280 540.3280   L 1000 area
#> 605      1       604     0.604       0.604 958.2261 555.5286   L 1000 area
#> 606      1       605     0.605       0.605 969.2143 515.9512   L 1000 area
#> 607      1       606     0.606       0.606 963.0094 541.6733   L 1000 area
#> 608      1       607     0.607       0.607 966.9339 535.9687   L 1000 area
#> 609      1       608     0.608       0.608 963.2547 522.5416   L 1000 area
#> 610      1       609     0.609       0.609 964.0805 533.0616   L 1000 area
#> 611      1       610     0.610       0.610 967.5984 538.6033   L 1000 area
#> 612      1       611     0.611       0.611 937.1285 535.4176   L 1000 area
#> 613      1       612     0.612       0.612 965.1784 549.8400   L 1000 area
#> 614      1       613     0.613       0.613 946.5276 546.8723   L 1000 area
#> 615      1       614     0.614       0.614 963.6316 528.8940   L 1000 area
#> 616      1       615     0.615       0.615 973.1475 545.4159   L 1000 area
#> 617      1       616     0.616       0.616 955.5169 547.8018   L 1000 area
#> 618      1       617     0.617       0.617 951.9184 528.0781   L 1000 area
#> 619      1       618     0.618       0.618 959.1373 537.5273   L 1000 area
#> 620      1       619     0.619       0.619 975.8542 556.5407   L 1000 area
#> 621      1       620     0.620       0.620 961.0106 545.0304   L 1000 area
#> 622      1       621     0.621       0.621 961.4876 549.1005   L 1000 area
#> 623      1       622     0.622       0.622 954.5357 557.5622   L 1000 area
#> 624      1       623     0.623       0.623 954.7232 541.2704   L 1000 area
#> 625      1       624     0.624       0.624 958.6013 542.6597   L 1000 area
#> 626      1       625     0.625       0.625 944.1059 550.8004   L 1000 area
#> 627      1       626     0.626       0.626 972.3410 540.0937   L 1000 area
#> 628      1       627     0.627       0.627 956.6455 543.8422   L 1000 area
#> 629      1       628     0.628       0.628 939.9900 534.6703   L 1000 area
#> 630      1       629     0.629       0.629 959.8254 531.7745   L 1000 area
#> 631      1       630     0.630       0.630 957.5903 539.5335   L 1000 area
#> 632      1       631     0.631       0.631 942.0801 534.3719   L 1000 area
#> 633      1       632     0.632       0.632 980.4969 547.8689   L 1000 area
#> 634      1       633     0.633       0.633 948.8278 533.5243   L 1000 area
#> 635      1       634     0.634       0.634 946.4645 532.1997   L 1000 area
#> 636      1       635     0.635       0.635 964.5130 530.9847   L 1000 area
#> 637      1       636     0.636       0.636 970.3323 551.2854   L 1000 area
#> 638      1       637     0.637       0.637 958.0411 536.1617   L 1000 area
#> 639      1       638     0.638       0.638 962.2731 534.0672   L 1000 area
#> 640      1       639     0.639       0.639 948.9780 538.7497   L 1000 area
#> 641      1       640     0.640       0.640 969.5528 535.1731   L 1000 area
#> 642      1       641     0.641       0.641 963.1283 556.7105   L 1000 area
#> 643      1       642     0.642       0.642 961.0101 546.8600   L 1000 area
#> 644      1       643     0.643       0.643 967.4812 532.7235   L 1000 area
#> 645      1       644     0.644       0.644 969.9896 543.0209   L 1000 area
#> 646      1       645     0.645       0.645 952.6748 542.5766   L 1000 area
#> 647      1       646     0.646       0.646 964.8090 551.2498   L 1000 area
#> 648      1       647     0.647       0.647 950.1055 536.8989   L 1000 area
#> 649      1       648     0.648       0.648 982.2956 548.2459   L 1000 area
#> 650      1       649     0.649       0.649 964.5674 551.7489   L 1000 area
#> 651      1       650     0.650       0.650 959.6418 559.7629   L 1000 area
#> 652      1       651     0.651       0.651 955.0956 539.6390   L 1000 area
#> 653      1       652     0.652       0.652 957.7943 540.7117   L 1000 area
#> 654      1       653     0.653       0.653 948.9879 552.3355   L 1000 area
#> 655      1       654     0.654       0.654 960.6347 542.8578   L 1000 area
#> 656      1       655     0.655       0.655 965.8353 542.1580   L 1000 area
#> 657      1       656     0.656       0.656 954.8664 553.7532   L 1000 area
#> 658      1       657     0.657       0.657 951.8745 548.8658   L 1000 area
#> 659      1       658     0.658       0.658 943.7068 540.1751   L 1000 area
#> 660      1       659     0.659       0.659 965.6891 523.6614   L 1000 area
#> 661      1       660     0.660       0.660 963.3378 538.9963   L 1000 area
#> 662      1       661     0.661       0.661 957.8868 551.2964   L 1000 area
#> 663      1       662     0.662       0.662 954.4890 534.8882   L 1000 area
#> 664      1       663     0.663       0.663 962.5836 516.9195   L 1000 area
#> 665      1       664     0.664       0.664 946.2479 542.7471   L 1000 area
#> 666      1       665     0.665       0.665 945.2725 533.7783   L 1000 area
#> 667      1       666     0.666       0.666 945.5479 538.8849   L 1000 area
#> 668      1       667     0.667       0.667 956.3871 538.8590   L 1000 area
#> 669      1       668     0.668       0.668 967.9071 528.1467   L 1000 area
#> 670      1       669     0.669       0.669 967.1466 538.2442   L 1000 area
#> 671      1       670     0.670       0.670 954.3485 534.7874   L 1000 area
#> 672      1       671     0.671       0.671 974.1801 537.7890   L 1000 area
#> 673      1       672     0.672       0.672 948.5424 534.4712   L 1000 area
#> 674      1       673     0.673       0.673 955.0718 535.6149   L 1000 area
#> 675      1       674     0.674       0.674 952.6695 534.3238   L 1000 area
#> 676      1       675     0.675       0.675 961.5923 558.1772   L 1000 area
#> 677      1       676     0.676       0.676 942.4881 545.9778   L 1000 area
#> 678      1       677     0.677       0.677 971.8483 543.0372   L 1000 area
#> 679      1       678     0.678       0.678 972.0703 557.1796   L 1000 area
#> 680      1       679     0.679       0.679 969.7765 531.0733   L 1000 area
#> 681      1       680     0.680       0.680 959.1801 545.7353   L 1000 area
#> 682      1       681     0.681       0.681 971.5626 529.8764   L 1000 area
#> 683      1       682     0.682       0.682 964.8680 535.1797   L 1000 area
#> 684      1       683     0.683       0.683 969.5885 545.1141   L 1000 area
#> 685      1       684     0.684       0.684 961.7779 534.4011   L 1000 area
#> 686      1       685     0.685       0.685 962.9225 535.1736   L 1000 area
#> 687      1       686     0.686       0.686 938.8666 554.7293   L 1000 area
#> 688      1       687     0.687       0.687 954.6195 555.9422   L 1000 area
#> 689      1       688     0.688       0.688 970.1403 532.6913   L 1000 area
#> 690      1       689     0.689       0.689 961.0127 539.6171   L 1000 area
#> 691      1       690     0.690       0.690 947.8129 549.9176   L 1000 area
#> 692      1       691     0.691       0.691 950.4162 521.3372   L 1000 area
#> 693      1       692     0.692       0.692 954.4420 535.6267   L 1000 area
#> 694      1       693     0.693       0.693 981.2188 549.3608   L 1000 area
#> 695      1       694     0.694       0.694 958.2041 543.8318   L 1000 area
#> 696      1       695     0.695       0.695 953.5526 536.9827   L 1000 area
#> 697      1       696     0.696       0.696 939.0742 529.9575   L 1000 area
#> 698      1       697     0.697       0.697 940.9813 548.0980   L 1000 area
#> 699      1       698     0.698       0.698 972.2225 554.5966   L 1000 area
#> 700      1       699     0.699       0.699 951.4878 518.0877   L 1000 area
#> 701      1       700     0.700       0.700 976.2120 535.9466   L 1000 area
#> 702      1       701     0.701       0.701 956.7090 559.4067   L 1000 area
#> 703      1       702     0.702       0.702 936.7359 544.8497   L 1000 area
#> 704      1       703     0.703       0.703 981.9300 537.9790   L 1000 area
#> 705      1       704     0.704       0.704 949.1752 528.3037   L 1000 area
#> 706      1       705     0.705       0.705 954.9364 536.3015   L 1000 area
#> 707      1       706     0.706       0.706 960.9592 547.5979   L 1000 area
#> 708      1       707     0.707       0.707 958.7983 541.9779   L 1000 area
#> 709      1       708     0.708       0.708 944.8133 535.4526   L 1000 area
#> 710      1       709     0.709       0.709 944.8941 530.7940   L 1000 area
#> 711      1       710     0.710       0.710 973.9560 538.9229   L 1000 area
#> 712      1       711     0.711       0.711 952.9158 527.9634   L 1000 area
#> 713      1       712     0.712       0.712 968.2794 566.7487   L 1000 area
#> 714      1       713     0.713       0.713 977.0776 555.7247   L 1000 area
#> 715      1       714     0.714       0.714 952.2083 546.7842   L 1000 area
#> 716      1       715     0.715       0.715 952.9310 541.1641   L 1000 area
#> 717      1       716     0.716       0.716 940.0710 535.3369   L 1000 area
#> 718      1       717     0.717       0.717 978.9988 558.4623   L 1000 area
#> 719      1       718     0.718       0.718 952.4379 527.0860   L 1000 area
#> 720      1       719     0.719       0.719 956.2204 549.6074   L 1000 area
#> 721      1       720     0.720       0.720 946.3605 542.3254   L 1000 area
#> 722      1       721     0.721       0.721 967.4196 548.6255   L 1000 area
#> 723      1       722     0.722       0.722 976.1098 554.1143   L 1000 area
#> 724      1       723     0.723       0.723 962.2739 539.8699   L 1000 area
#> 725      1       724     0.724       0.724 955.9948 542.0226   L 1000 area
#> 726      1       725     0.725       0.725 943.3042 512.1399   L 1000 area
#> 727      1       726     0.726       0.726 974.9070 538.4200   L 1000 area
#> 728      1       727     0.727       0.727 966.9149 548.3164   L 1000 area
#> 729      1       728     0.728       0.728 959.5902 561.8853   L 1000 area
#> 730      1       729     0.729       0.729 972.0434 528.5391   L 1000 area
#> 731      1       730     0.730       0.730 972.9851 542.3125   L 1000 area
#> 732      1       731     0.731       0.731 955.3778 540.2265   L 1000 area
#> 733      1       732     0.732       0.732 947.7423 553.7913   L 1000 area
#> 734      1       733     0.733       0.733 969.6327 537.5375   L 1000 area
#> 735      1       734     0.734       0.734 966.9321 539.9487   L 1000 area
#> 736      1       735     0.735       0.735 954.9861 530.3700   L 1000 area
#> 737      1       736     0.736       0.736 991.5397 539.8063   L 1000 area
#> 738      1       737     0.737       0.737 934.5089 528.4133   L 1000 area
#> 739      1       738     0.738       0.738 959.9186 538.4293   L 1000 area
#> 740      1       739     0.739       0.739 971.3878 533.9928   L 1000 area
#> 741      1       740     0.740       0.740 974.8235 538.3600   L 1000 area
#> 742      1       741     0.741       0.741 969.7585 533.1620   L 1000 area
#> 743      1       742     0.742       0.742 961.4835 556.6298   L 1000 area
#> 744      1       743     0.743       0.743 967.8891 534.2833   L 1000 area
#> 745      1       744     0.744       0.744 965.5758 539.6454   L 1000 area
#> 746      1       745     0.745       0.745 959.7261 536.0294   L 1000 area
#> 747      1       746     0.746       0.746 951.2293 547.1473   L 1000 area
#> 748      1       747     0.747       0.747 972.1624 552.6745   L 1000 area
#> 749      1       748     0.748       0.748 956.4875 536.4321   L 1000 area
#> 750      1       749     0.749       0.749 957.0561 529.7468   L 1000 area
#> 751      1       750     0.750       0.750 962.5314 544.0972   L 1000 area
#> 752      1       751     0.751       0.751 960.1150 545.2975   L 1000 area
#> 753      1       752     0.752       0.752 940.2422 536.3687   L 1000 area
#> 754      1       753     0.753       0.753 955.7367 546.8175   L 1000 area
#> 755      1       754     0.754       0.754 966.1230 542.0341   L 1000 area
#> 756      1       755     0.755       0.755 972.3081 527.1545   L 1000 area
#> 757      1       756     0.756       0.756 947.2919 546.1060   L 1000 area
#> 758      1       757     0.757       0.757 953.4944 528.0371   L 1000 area
#> 759      1       758     0.758       0.758 935.5683 553.2261   L 1000 area
#> 760      1       759     0.759       0.759 969.9655 543.7430   L 1000 area
#> 761      1       760     0.760       0.760 960.0511 556.3734   L 1000 area
#> 762      1       761     0.761       0.761 967.9102 539.0441   L 1000 area
#> 763      1       762     0.762       0.762 954.3681 524.3997   L 1000 area
#> 764      1       763     0.763       0.763 962.1383 526.0936   L 1000 area
#> 765      1       764     0.764       0.764 969.7708 552.8583   L 1000 area
#> 766      1       765     0.765       0.765 957.6584 533.9293   L 1000 area
#> 767      1       766     0.766       0.766 968.4866 530.2007   L 1000 area
#> 768      1       767     0.767       0.767 966.5397 543.0243   L 1000 area
#> 769      1       768     0.768       0.768 944.0943 537.4310   L 1000 area
#> 770      1       769     0.769       0.769 963.0942 529.3181   L 1000 area
#> 771      1       770     0.770       0.770 935.1402 514.6861   L 1000 area
#> 772      1       771     0.771       0.771 957.2156 535.4907   L 1000 area
#> 773      1       772     0.772       0.772 954.4757 543.5441   L 1000 area
#> 774      1       773     0.773       0.773 968.7604 536.5102   L 1000 area
#> 775      1       774     0.774       0.774 962.9348 538.5392   L 1000 area
#> 776      1       775     0.775       0.775 954.0278 535.4742   L 1000 area
#> 777      1       776     0.776       0.776 958.4280 542.3490   L 1000 area
#> 778      1       777     0.777       0.777 961.8419 547.0126   L 1000 area
#> 779      1       778     0.778       0.778 949.8697 553.2660   L 1000 area
#> 780      1       779     0.779       0.779 953.2016 524.2687   L 1000 area
#> 781      1       780     0.780       0.780 947.5731 542.1828   L 1000 area
#> 782      1       781     0.781       0.781 944.6498 537.4666   L 1000 area
#> 783      1       782     0.782       0.782 954.7546 550.5290   L 1000 area
#> 784      1       783     0.783       0.783 959.8147 562.2304   L 1000 area
#> 785      1       784     0.784       0.784 944.3185 539.9215   L 1000 area
#> 786      1       785     0.785       0.785 964.6785 544.2850   L 1000 area
#> 787      1       786     0.786       0.786 953.3217 550.5634   L 1000 area
#> 788      1       787     0.787       0.787 963.1108 555.2170   L 1000 area
#> 789      1       788     0.788       0.788 961.4255 551.9959   L 1000 area
#> 790      1       789     0.789       0.789 956.2597 519.4110   L 1000 area
#> 791      1       790     0.790       0.790 940.5507 528.4337   L 1000 area
#> 792      1       791     0.791       0.791 944.0232 536.9591   L 1000 area
#> 793      1       792     0.792       0.792 934.5934 558.9913   L 1000 area
#> 794      1       793     0.793       0.793 973.6205 541.4184   L 1000 area
#> 795      1       794     0.794       0.794 956.3933 540.9232   L 1000 area
#> 796      1       795     0.795       0.795 979.3640 540.9880   L 1000 area
#> 797      1       796     0.796       0.796 945.4412 545.9297   L 1000 area
#> 798      1       797     0.797       0.797 974.8191 552.7484   L 1000 area
#> 799      1       798     0.798       0.798 970.7612 550.0699   L 1000 area
#> 800      1       799     0.799       0.799 952.4251 516.3817   L 1000 area
#> 801      1       800     0.800       0.800 967.1409 518.9594   L 1000 area
#> 802      1       801     0.801       0.801 965.8138 539.1556   L 1000 area
#> 803      1       802     0.802       0.802 958.5328 547.5633   L 1000 area
#> 804      1       803     0.803       0.803 975.0698 524.1928   L 1000 area
#> 805      1       804     0.804       0.804 957.2047 547.0725   L 1000 area
#> 806      1       805     0.805       0.805 980.2774 529.5401   L 1000 area
#> 807      1       806     0.806       0.806 948.0426 542.5914   L 1000 area
#> 808      1       807     0.807       0.807 973.1232 539.9832   L 1000 area
#> 809      1       808     0.808       0.808 954.7599 528.2445   L 1000 area
#> 810      1       809     0.809       0.809 963.5425 557.4463   L 1000 area
#> 811      1       810     0.810       0.810 959.2833 547.1827   L 1000 area
#> 812      1       811     0.811       0.811 958.6686 568.6214   L 1000 area
#> 813      1       812     0.812       0.812 959.1192 547.1799   L 1000 area
#> 814      1       813     0.813       0.813 969.1779 536.7073   L 1000 area
#> 815      1       814     0.814       0.814 960.3144 538.6952   L 1000 area
#> 816      1       815     0.815       0.815 973.5893 534.5267   L 1000 area
#> 817      1       816     0.816       0.816 961.1349 527.1824   L 1000 area
#> 818      1       817     0.817       0.817 961.7431 518.2949   L 1000 area
#> 819      1       818     0.818       0.818 959.4652 540.0056   L 1000 area
#> 820      1       819     0.819       0.819 955.7524 555.4400   L 1000 area
#> 821      1       820     0.820       0.820 962.0791 543.8859   L 1000 area
#> 822      1       821     0.821       0.821 969.9292 536.8517   L 1000 area
#> 823      1       822     0.822       0.822 968.1386 547.9083   L 1000 area
#> 824      1       823     0.823       0.823 969.9601 541.8859   L 1000 area
#> 825      1       824     0.824       0.824 977.3274 545.8434   L 1000 area
#> 826      1       825     0.825       0.825 963.5704 545.1499   L 1000 area
#> 827      1       826     0.826       0.826 961.5573 538.2977   L 1000 area
#> 828      1       827     0.827       0.827 975.5279 550.4120   L 1000 area
#> 829      1       828     0.828       0.828 959.3593 543.2115   L 1000 area
#> 830      1       829     0.829       0.829 949.9178 524.4241   L 1000 area
#> 831      1       830     0.830       0.830 951.0710 546.2641   L 1000 area
#> 832      1       831     0.831       0.831 953.4549 559.7365   L 1000 area
#> 833      1       832     0.832       0.832 945.6622 543.0348   L 1000 area
#> 834      1       833     0.833       0.833 974.1818 559.8956   L 1000 area
#> 835      1       834     0.834       0.834 954.0611 544.8033   L 1000 area
#> 836      1       835     0.835       0.835 971.1897 545.4516   L 1000 area
#> 837      1       836     0.836       0.836 983.8408 537.6736   L 1000 area
#> 838      1       837     0.837       0.837 955.9671 519.4388   L 1000 area
#> 839      1       838     0.838       0.838 963.7011 528.5264   L 1000 area
#> 840      1       839     0.839       0.839 954.2471 538.1995   L 1000 area
#> 841      1       840     0.840       0.840 989.1923 528.9969   L 1000 area
#> 842      1       841     0.841       0.841 949.2109 524.3109   L 1000 area
#> 843      1       842     0.842       0.842 971.7849 539.5166   L 1000 area
#> 844      1       843     0.843       0.843 968.6960 534.0211   L 1000 area
#> 845      1       844     0.844       0.844 969.3117 564.1734   L 1000 area
#> 846      1       845     0.845       0.845 970.2468 541.0958   L 1000 area
#> 847      1       846     0.846       0.846 966.1298 544.4995   L 1000 area
#> 848      1       847     0.847       0.847 966.0819 544.9077   L 1000 area
#> 849      1       848     0.848       0.848 951.0711 540.5022   L 1000 area
#> 850      1       849     0.849       0.849 964.5204 546.5469   L 1000 area
#> 851      1       850     0.850       0.850 956.2529 545.3178   L 1000 area
#> 852      1       851     0.851       0.851 988.4019 547.7072   L 1000 area
#> 853      1       852     0.852       0.852 967.3239 549.9313   L 1000 area
#> 854      1       853     0.853       0.853 968.4052 550.4351   L 1000 area
#> 855      1       854     0.854       0.854 937.8035 540.5782   L 1000 area
#> 856      1       855     0.855       0.855 946.7254 527.3675   L 1000 area
#> 857      1       856     0.856       0.856 950.2334 513.4637   L 1000 area
#> 858      1       857     0.857       0.857 954.1292 535.5056   L 1000 area
#> 859      1       858     0.858       0.858 954.3199 524.8259   L 1000 area
#> 860      1       859     0.859       0.859 966.4364 536.8194   L 1000 area
#> 861      1       860     0.860       0.860 957.6269 537.5734   L 1000 area
#> 862      1       861     0.861       0.861 957.0142 542.9274   L 1000 area
#> 863      1       862     0.862       0.862 971.8956 556.7191   L 1000 area
#> 864      1       863     0.863       0.863 938.9106 556.9792   L 1000 area
#> 865      1       864     0.864       0.864 959.2672 549.1867   L 1000 area
#> 866      1       865     0.865       0.865 968.6340 542.3231   L 1000 area
#> 867      1       866     0.866       0.866 970.7471 544.2221   L 1000 area
#> 868      1       867     0.867       0.867 957.9415 539.5214   L 1000 area
#> 869      1       868     0.868       0.868 955.6639 524.0649   L 1000 area
#> 870      1       869     0.869       0.869 958.1778 559.4913   L 1000 area
#> 871      1       870     0.870       0.870 955.6892 551.8666   L 1000 area
#> 872      1       871     0.871       0.871 955.7539 551.7415   L 1000 area
#> 873      1       872     0.872       0.872 946.4618 534.9416   L 1000 area
#> 874      1       873     0.873       0.873 958.0341 545.7826   L 1000 area
#> 875      1       874     0.874       0.874 962.3412 544.9219   L 1000 area
#> 876      1       875     0.875       0.875 954.4279 524.3270   L 1000 area
#> 877      1       876     0.876       0.876 948.8667 538.3198   L 1000 area
#> 878      1       877     0.877       0.877 943.6936 543.7761   L 1000 area
#> 879      1       878     0.878       0.878 956.1218 544.0310   L 1000 area
#> 880      1       879     0.879       0.879 977.2263 533.7250   L 1000 area
#> 881      1       880     0.880       0.880 954.4150 558.8266   L 1000 area
#> 882      1       881     0.881       0.881 953.6334 558.5555   L 1000 area
#> 883      1       882     0.882       0.882 987.8139 546.9630   L 1000 area
#> 884      1       883     0.883       0.883 940.4274 533.5780   L 1000 area
#> 885      1       884     0.884       0.884 961.4253 530.6241   L 1000 area
#> 886      1       885     0.885       0.885 966.6450 527.0059   L 1000 area
#> 887      1       886     0.886       0.886 955.2137 533.4540   L 1000 area
#> 888      1       887     0.887       0.887 971.8369 521.6177   L 1000 area
#> 889      1       888     0.888       0.888 968.0544 544.9755   L 1000 area
#> 890      1       889     0.889       0.889 960.5332 525.3652   L 1000 area
#> 891      1       890     0.890       0.890 948.3265 528.0183   L 1000 area
#> 892      1       891     0.891       0.891 963.2335 546.8110   L 1000 area
#> 893      1       892     0.892       0.892 956.7922 544.0762   L 1000 area
#> 894      1       893     0.893       0.893 951.1405 551.3832   L 1000 area
#> 895      1       894     0.894       0.894 961.5128 545.0172   L 1000 area
#> 896      1       895     0.895       0.895 965.7936 539.3296   L 1000 area
#> 897      1       896     0.896       0.896 959.2083 544.4005   L 1000 area
#> 898      1       897     0.897       0.897 968.4877 524.7804   L 1000 area
#> 899      1       898     0.898       0.898 934.8897 541.0962   L 1000 area
#> 900      1       899     0.899       0.899 967.3848 541.8904   L 1000 area
#> 901      1       900     0.900       0.900 954.2901 547.8104   L 1000 area
#> 902      1       901     0.901       0.901 962.8654 539.5349   L 1000 area
#> 903      1       902     0.902       0.902 971.4762 540.9577   L 1000 area
#> 904      1       903     0.903       0.903 961.3956 526.6474   L 1000 area
#> 905      1       904     0.904       0.904 960.8893 545.4879   L 1000 area
#> 906      1       905     0.905       0.905 933.6984 520.9476   L 1000 area
#> 907      1       906     0.906       0.906 947.8278 542.0427   L 1000 area
#> 908      1       907     0.907       0.907 967.4915 544.0804   L 1000 area
#> 909      1       908     0.908       0.908 945.8407 554.2675   L 1000 area
#> 910      1       909     0.909       0.909 958.8762 549.2527   L 1000 area
#> 911      1       910     0.910       0.910 963.0403 550.2432   L 1000 area
#> 912      1       911     0.911       0.911 956.4599 522.0186   L 1000 area
#> 913      1       912     0.912       0.912 951.4876 556.0232   L 1000 area
#> 914      1       913     0.913       0.913 945.3883 535.6457   L 1000 area
#> 915      1       914     0.914       0.914 962.6425 566.4712   L 1000 area
#> 916      1       915     0.915       0.915 940.9264 533.2625   L 1000 area
#> 917      1       916     0.916       0.916 950.7477 528.2119   L 1000 area
#> 918      1       917     0.917       0.917 958.8843 547.1355   L 1000 area
#> 919      1       918     0.918       0.918 950.3477 539.8790   L 1000 area
#> 920      1       919     0.919       0.919 958.3710 552.5987   L 1000 area
#> 921      1       920     0.920       0.920 968.6018 535.2302   L 1000 area
#> 922      1       921     0.921       0.921 960.1810 530.5728   L 1000 area
#> 923      1       922     0.922       0.922 961.3726 547.6068   L 1000 area
#> 924      1       923     0.923       0.923 975.9889 537.5005   L 1000 area
#> 925      1       924     0.924       0.924 958.8870 550.7886   L 1000 area
#> 926      1       925     0.925       0.925 962.8546 546.4284   L 1000 area
#> 927      1       926     0.926       0.926 940.1609 537.7241   L 1000 area
#> 928      1       927     0.927       0.927 960.3074 541.7595   L 1000 area
#> 929      1       928     0.928       0.928 952.4000 543.6862   L 1000 area
#> 930      1       929     0.929       0.929 947.7599 528.3105   L 1000 area
#> 931      1       930     0.930       0.930 970.0237 534.2057   L 1000 area
#> 932      1       931     0.931       0.931 959.7846 542.9734   L 1000 area
#> 933      1       932     0.932       0.932 969.4532 558.5245   L 1000 area
#> 934      1       933     0.933       0.933 950.9151 548.0671   L 1000 area
#> 935      1       934     0.934       0.934 935.9780 544.8819   L 1000 area
#> 936      1       935     0.935       0.935 961.6294 549.2650   L 1000 area
#> 937      1       936     0.936       0.936 969.4321 544.5831   L 1000 area
#> 938      1       937     0.937       0.937 985.4460 533.4806   L 1000 area
#> 939      1       938     0.938       0.938 968.8719 552.1580   L 1000 area
#> 940      1       939     0.939       0.939 970.6044 529.1349   L 1000 area
#> 941      1       940     0.940       0.940 953.8425 530.2490   L 1000 area
#> 942      1       941     0.941       0.941 963.5285 543.1303   L 1000 area
#> 943      1       942     0.942       0.942 955.2467 556.9153   L 1000 area
#> 944      1       943     0.943       0.943 964.2206 534.2761   L 1000 area
#> 945      1       944     0.944       0.944 948.1644 550.5126   L 1000 area
#> 946      1       945     0.945       0.945 956.1625 538.1875   L 1000 area
#> 947      1       946     0.946       0.946 945.4033 544.8565   L 1000 area
#> 948      1       947     0.947       0.947 960.4577 525.6407   L 1000 area
#> 949      1       948     0.948       0.948 964.5246 529.3806   L 1000 area
#> 950      1       949     0.949       0.949 960.5757 558.9261   L 1000 area
#> 951      1       950     0.950       0.950 953.6131 540.2449   L 1000 area
#> 952      1       951     0.951       0.951 957.6132 540.6785   L 1000 area
#> 953      1       952     0.952       0.952 943.5710 514.2758   L 1000 area
#> 954      1       953     0.953       0.953 957.7743 552.2030   L 1000 area
#> 955      1       954     0.954       0.954 942.2443 538.6630   L 1000 area
#> 956      1       955     0.955       0.955 959.8453 544.5462   L 1000 area
#> 957      1       956     0.956       0.956 977.5105 543.6168   L 1000 area
#> 958      1       957     0.957       0.957 967.0421 543.9142   L 1000 area
#> 959      1       958     0.958       0.958 974.8102 535.9797   L 1000 area
#> 960      1       959     0.959       0.959 973.5289 558.8500   L 1000 area
#> 961      1       960     0.960       0.960 956.7004 541.3579   L 1000 area
#> 962      1       961     0.961       0.961 963.6350 538.3086   L 1000 area
#> 963      1       962     0.962       0.962 941.7234 556.2183   L 1000 area
#> 964      1       963     0.963       0.963 968.9378 536.2698   L 1000 area
#> 965      1       964     0.964       0.964 952.8088 526.5354   L 1000 area
#> 966      1       965     0.965       0.965 969.4051 519.1132   L 1000 area
#> 967      1       966     0.966       0.966 941.2884 538.9838   L 1000 area
#> 968      1       967     0.967       0.967 971.4700 534.0560   L 1000 area
#> 969      1       968     0.968       0.968 954.9701 537.8411   L 1000 area
#> 970      1       969     0.969       0.969 974.2590 540.7526   L 1000 area
#> 971      1       970     0.970       0.970 959.8066 561.6693   L 1000 area
#> 972      1       971     0.971       0.971 953.6193 546.4540   L 1000 area
#> 973      1       972     0.972       0.972 964.9827 538.0267   L 1000 area
#> 974      1       973     0.973       0.973 959.4175 552.7368   L 1000 area
#> 975      1       974     0.974       0.974 953.7378 536.9266   L 1000 area
#> 976      1       975     0.975       0.975 951.0538 545.2202   L 1000 area
#> 977      1       976     0.976       0.976 984.1006 551.6263   L 1000 area
#> 978      1       977     0.977       0.977 950.5035 539.0715   L 1000 area
#> 979      1       978     0.978       0.978 965.5915 520.9805   L 1000 area
#> 980      1       979     0.979       0.979 957.8859 535.0673   L 1000 area
#> 981      1       980     0.980       0.980 949.8215 558.6369   L 1000 area
#> 982      1       981     0.981       0.981 946.0981 537.8719   L 1000 area
#> 983      1       982     0.982       0.982 945.1137 551.3666   L 1000 area
#> 984      1       983     0.983       0.983 945.9396 544.4274   L 1000 area
#> 985      1       984     0.984       0.984 945.0376 540.6990   L 1000 area
#> 986      1       985     0.985       0.985 966.8615 513.7412   L 1000 area
#> 987      1       986     0.986       0.986 957.6324 547.1641   L 1000 area
#> 988      1       987     0.987       0.987 979.9914 533.9539   L 1000 area
#> 989      1       988     0.988       0.988 966.3324 536.2942   L 1000 area
#> 990      1       989     0.989       0.989 976.4820 534.6633   L 1000 area
#> 991      1       990     0.990       0.990 965.0272 558.1219   L 1000 area
#> 992      1       991     0.991       0.991 968.4547 528.0369   L 1000 area
#> 993      1       992     0.992       0.992 964.6725 513.0823   L 1000 area
#> 994      1       993     0.993       0.993 955.9794 545.4651   L 1000 area
#> 995      1       994     0.994       0.994 969.2353 547.6893   L 1000 area
#> 996      1       995     0.995       0.995 959.9194 538.1324   L 1000 area
#> 997      1       996     0.996       0.996 970.3378 537.7064   L 1000 area
#> 998      1       997     0.997       0.997 952.0087 556.3019   L 1000 area
#> 999      1       998     0.998       0.998 970.0423 518.3533   L 1000 area
#> 1000     1       999     0.999       0.999 956.8803 529.2222   L 1000 area
#>      pupil_raw pupil_raw_deblink pupil_raw_deblink_detransient
#> 1     996.8677          996.8677                      996.8677
#> 2     997.7859          997.7859                      997.7859
#> 3     993.6078          993.6078                      993.6078
#> 4    1001.5842         1001.5842                     1001.5842
#> 5    1003.2317         1003.2317                     1003.2317
#> 6     999.1294          999.1294                      999.1294
#> 7    1001.5666         1001.5666                     1001.5666
#> 8    1005.2582         1005.2582                     1005.2582
#> 9    1008.1371         1008.1371                     1008.1371
#> 10   1006.6101         1006.6101                     1006.6101
#> 11   1014.1690         1014.1690                     1014.1690
#> 12   1016.1183         1016.1183                     1016.1183
#> 13   1013.0121         1013.0121                     1013.0121
#> 14   1001.9386         1001.9386                     1001.9386
#> 15   1007.5632         1007.5632                     1007.5632
#> 16   1007.3385         1007.3385                     1007.3385
#> 17   1007.2576         1007.2576                     1007.2576
#> 18   1011.9768         1011.9768                     1011.9768
#> 19   1016.0829         1016.0829                     1016.0829
#> 20   1019.0524         1019.0524                     1019.0524
#> 21   1023.6473         1023.6473                     1023.6473
#> 22   1027.5580         1027.5580                     1027.5580
#> 23   1027.9308         1027.9308                     1027.9308
#> 24   1017.9840         1017.9840                     1017.9840
#> 25   1021.0832         1021.0832                     1021.0832
#> 26   1020.8025         1020.8025                     1020.8025
#> 27   1020.0235         1020.0235                     1020.0235
#> 28   1012.6698         1012.6698                     1012.6698
#> 29   1010.2790         1010.2790                     1010.2790
#> 30   1012.3687         1012.3687                     1012.3687
#> 31   1019.1621         1019.1621                     1019.1621
#> 32   1018.6482         1018.6482                     1018.6482
#> 33   1020.5865         1020.5865                     1020.5865
#> 34   1020.3175         1020.3175                     1020.3175
#> 35   1013.4322         1013.4322                     1013.4322
#> 36   1011.3572         1011.3572                     1011.3572
#> 37   1009.3858         1009.3858                     1009.3858
#> 38   1009.0892         1009.0892                     1009.0892
#> 39   1014.5894         1014.5894                     1014.5894
#> 40   1018.4052         1018.4052                     1018.4052
#> 41   1017.5826         1017.5826                     1017.5826
#> 42   1016.3158         1016.3158                     1016.3158
#> 43   1019.8006         1019.8006                     1019.8006
#> 44   1022.5839         1022.5839                     1022.5839
#> 45   1019.1402         1019.1402                     1019.1402
#> 46   1015.6027         1015.6027                     1015.6027
#> 47   1017.4256         1017.4256                     1017.4256
#> 48   1021.2683         1021.2683                     1021.2683
#> 49   1020.7065         1020.7065                     1020.7065
#> 50   1025.1121         1025.1121                     1025.1121
#> 51   1027.1026         1027.1026                     1027.1026
#> 52   1024.0425         1024.0425                     1024.0425
#> 53   1025.7481         1025.7481                     1025.7481
#> 54   1020.1013         1020.1013                     1020.1013
#> 55   1027.2664         1027.2664                     1027.2664
#> 56   1037.1684         1037.1684                     1037.1684
#> 57   1035.3323         1035.3323                     1035.3323
#> 58   1030.1116         1030.1116                     1030.1116
#> 59   1032.9602         1032.9602                     1032.9602
#> 60   1032.2849         1032.2849                     1032.2849
#> 61   1044.2930         1044.2930                     1044.2930
#> 62   1044.0968         1044.0968                     1044.0968
#> 63   1047.5455         1047.5455                     1047.5455
#> 64   1047.6855         1047.6855                     1047.6855
#> 65   1043.9691         1043.9691                     1043.9691
#> 66   1044.9131         1044.9131                     1044.9131
#> 67   1035.8883         1035.8883                     1035.8883
#> 68   1043.2161         1043.2161                     1043.2161
#> 69   1043.9824         1043.9824                     1043.9824
#> 70   1054.8454         1054.8454                     1054.8454
#> 71   1057.2230         1057.2230                     1057.2230
#> 72   1053.6732         1053.6732                     1053.6732
#> 73   1056.7269         1056.7269                     1056.7269
#> 74   1052.0564         1052.0564                     1052.0564
#> 75   1045.7882         1045.7882                     1045.7882
#> 76   1047.2454         1047.2454                     1047.2454
#> 77   1045.0290         1045.0290                     1045.0290
#> 78   1045.0345         1045.0345                     1045.0345
#> 79   1045.4062         1045.4062                     1045.4062
#> 80   1042.4586         1042.4586                     1042.4586
#> 81   1039.6153         1039.6153                     1039.6153
#> 82   1038.9394         1038.9394                     1038.9394
#> 83   1044.8298         1044.8298                     1044.8298
#> 84   1037.2120         1037.2120                     1037.2120
#> 85   1040.1817         1040.1817                     1040.1817
#> 86   1041.8465         1041.8465                     1041.8465
#> 87   1047.1619         1047.1619                     1047.1619
#> 88   1045.6410         1045.6410                     1045.6410
#> 89   1047.4911         1047.4911                     1047.4911
#> 90   1048.8266         1048.8266                     1048.8266
#> 91   1046.1140         1046.1140                     1046.1140
#> 92   1052.1534         1052.1534                     1052.1534
#> 93   1057.9554         1057.9554                     1057.9554
#> 94   1061.4564         1061.4564                     1061.4564
#> 95   1069.3906         1069.3906                     1069.3906
#> 96   1072.1830         1072.1830                     1072.1830
#> 97   1065.8001         1065.8001                     1065.8001
#> 98   1062.9337         1062.9337                     1062.9337
#> 99   1056.8107         1056.8107                     1056.8107
#> 100  1054.4437         1054.4437                     1054.4437
#> 101  1051.3419         1051.3419                     1051.3419
#> 102  1051.5524         1051.5524                     1051.5524
#> 103  1046.9978         1046.9978                     1046.9978
#> 104  1047.7880         1047.7880                     1047.7880
#> 105  1044.5150         1044.5150                     1044.5150
#> 106  1053.3515         1053.3515                     1053.3515
#> 107  1056.9350         1056.9350                     1056.9350
#> 108  1061.4859         1061.4859                     1061.4859
#> 109  1063.4068         1063.4068                     1063.4068
#> 110  1071.8177         1071.8177                     1071.8177
#> 111  1068.6390         1068.6390                     1068.6390
#> 112  1066.3308         1066.3308                     1066.3308
#> 113  1073.4922         1073.4922                     1073.4922
#> 114  1070.2387         1070.2387                     1070.2387
#> 115  1069.2018         1069.2018                     1069.2018
#> 116  1067.2378         1067.2378                     1067.2378
#> 117  1065.6378         1065.6378                     1065.6378
#> 118  1064.2422         1064.2422                     1064.2422
#> 119  1066.7132         1066.7132                     1066.7132
#> 120  1065.8265         1065.8265                     1065.8265
#> 121  1063.2967         1063.2967                     1063.2967
#> 122  1070.0119         1070.0119                     1070.0119
#> 123  1068.9390         1068.9390                     1068.9390
#> 124  1068.0413         1068.0413                     1068.0413
#> 125  1067.5403         1067.5403                     1067.5403
#> 126  1071.1036         1071.1036                     1071.1036
#> 127  1070.7358         1070.7358                     1070.7358
#> 128  1070.5476         1070.5476                     1070.5476
#> 129  1067.1393         1067.1393                     1067.1393
#> 130  1065.5180         1065.5180                     1065.5180
#> 131  1065.8188         1065.8188                     1065.8188
#> 132  1062.8743         1062.8743                     1062.8743
#> 133  1065.5318         1065.5318                     1065.5318
#> 134  1057.9398         1057.9398                     1057.9398
#> 135  1059.4726         1059.4726                     1059.4726
#> 136  1051.7904         1051.7904                     1051.7904
#> 137  1050.2855         1050.2855                     1050.2855
#> 138  1047.6441         1047.6441                     1047.6441
#> 139  1044.3836         1044.3836                     1044.3836
#> 140  1044.0991         1044.0991                     1044.0991
#> 141  1034.5273         1034.5273                     1034.5273
#> 142  1040.4103         1040.4103                     1040.4103
#> 143  1032.0854         1032.0854                     1032.0854
#> 144  1029.7677         1029.7677                     1029.7677
#> 145  1024.1881         1024.1881                     1024.1881
#> 146  1020.4340         1020.4340                     1020.4340
#> 147  1030.8699         1030.8699                     1030.8699
#> 148  1030.9569         1030.9569                     1030.9569
#> 149  1024.5254         1024.5254                     1024.5254
#> 150  1016.3223         1016.3223                     1016.3223
#> 151  1018.5733         1018.5733                     1018.5733
#> 152  1018.4805         1018.4805                     1018.4805
#> 153  1016.8901         1016.8901                     1016.8901
#> 154  1012.2433         1012.2433                     1012.2433
#> 155  1004.8060         1004.8060                     1004.8060
#> 156   999.4300          999.4300                      999.4300
#> 157  1004.4302         1004.4302                     1004.4302
#> 158  1001.3239         1001.3239                     1001.3239
#> 159   994.4017          994.4017                      994.4017
#> 160  1003.7482         1003.7482                     1003.7482
#> 161  1005.8737         1005.8737                     1005.8737
#> 162  1004.6804         1004.6804                     1004.6804
#> 163  1009.9729         1009.9729                     1009.9729
#> 164  1014.4050         1014.4050                     1014.4050
#> 165  1011.3088         1011.3088                     1011.3088
#> 166  1022.3393         1022.3393                     1022.3393
#> 167  1021.0641         1021.0641                     1021.0641
#> 168  1013.9417         1013.9417                     1013.9417
#> 169  1013.2197         1013.2197                     1013.2197
#> 170  1014.2574         1014.2574                     1014.2574
#> 171  1025.7972         1025.7972                     1025.7972
#> 172  1026.3263         1026.3263                     1026.3263
#> 173  1028.6112         1028.6112                     1028.6112
#> 174  1028.2255         1028.2255                     1028.2255
#> 175  1026.5555         1026.5555                     1026.5555
#> 176  1026.3818         1026.3818                     1026.3818
#> 177  1030.3200         1030.3200                     1030.3200
#> 178  1040.6963         1040.6963                     1040.6963
#> 179  1045.8332         1045.8332                     1045.8332
#> 180  1051.8728         1051.8728                     1051.8728
#> 181  1045.7162         1045.7162                     1045.7162
#> 182  1050.6356         1050.6356                     1050.6356
#> 183  1051.7353         1051.7353                     1051.7353
#> 184  1044.3990         1044.3990                     1044.3990
#> 185  1047.0041         1047.0041                     1047.0041
#> 186  1046.2104         1046.2104                     1046.2104
#> 187  1053.5333         1053.5333                     1053.5333
#> 188  1049.7029         1049.7029                     1049.7029
#> 189  1047.5518         1047.5518                     1047.5518
#> 190  1042.9213         1042.9213                     1042.9213
#> 191  1042.0358         1042.0358                     1042.0358
#> 192  1044.0458         1044.0458                     1044.0458
#> 193  1040.3871         1040.3871                     1040.3871
#> 194  1044.5389         1044.5389                     1044.5389
#> 195  1038.4985         1038.4985                     1038.4985
#> 196  1033.2586         1033.2586                     1033.2586
#> 197  1040.4644         1040.4644                     1040.4644
#> 198  1035.3852         1035.3852                     1035.3852
#> 199  1037.4450         1037.4450                     1037.4450
#> 200  1035.5396         1035.5396                     1035.5396
#> 201  1037.5867         1037.5867                     1037.5867
#> 202  1046.0310         1046.0310                     1046.0310
#> 203  1053.9640         1053.9640                     1053.9640
#> 204  1052.3094         1052.3094                     1052.3094
#> 205  1040.8832         1040.8832                     1040.8832
#> 206  1053.3716         1053.3716                     1053.3716
#> 207  1056.7069         1056.7069                     1056.7069
#> 208  1059.4135         1059.4135                     1059.4135
#> 209  1059.3465         1059.3465                     1059.3465
#> 210  1061.8971         1061.8971                     1061.8971
#> 211  1061.0752         1061.0752                     1061.0752
#> 212  1063.1787         1063.1787                     1063.1787
#> 213  1061.1774         1061.1774                     1061.1774
#> 214  1054.3264         1054.3264                     1054.3264
#> 215  1059.2656         1059.2656                     1059.2656
#> 216  1066.8643         1066.8643                     1066.8643
#> 217  1065.3206         1065.3206                     1065.3206
#> 218  1059.0542         1059.0542                     1059.0542
#> 219  1062.2654         1062.2654                     1062.2654
#> 220  1062.0418         1062.0418                     1062.0418
#> 221  1053.3757         1053.3757                     1053.3757
#> 222  1053.3864         1053.3864                     1053.3864
#> 223  1050.2349         1050.2349                     1050.2349
#> 224  1048.5300         1048.5300                     1048.5300
#> 225  1042.7472         1042.7472                     1042.7472
#> 226  1051.7629         1051.7629                     1051.7629
#> 227  1050.1072         1050.1072                     1050.1072
#> 228  1042.0797         1042.0797                     1042.0797
#> 229  1043.0656         1043.0656                     1043.0656
#> 230  1044.3815         1044.3815                     1044.3815
#> 231  1039.4524         1039.4524                     1039.4524
#> 232  1025.0078         1025.0078                     1025.0078
#> 233  1021.8054         1021.8054                     1021.8054
#> 234  1024.6579         1024.6579                     1024.6579
#> 235  1024.3593         1024.3593                     1024.3593
#> 236  1023.8684         1023.8684                     1023.8684
#> 237  1026.6725         1026.6725                     1026.6725
#> 238  1020.7402         1020.7402                     1020.7402
#> 239  1026.2241         1026.2241                     1026.2241
#> 240  1026.1974         1026.1974                     1026.1974
#> 241  1029.7339         1029.7339                     1029.7339
#> 242  1034.9045         1034.9045                     1034.9045
#> 243  1036.0219         1036.0219                     1036.0219
#> 244  1031.6283         1031.6283                     1031.6283
#> 245  1037.4431         1037.4431                     1037.4431
#> 246  1027.4423         1027.4423                     1027.4423
#> 247  1024.7184         1024.7184                     1024.7184
#> 248  1023.4400         1023.4400                     1023.4400
#> 249  1022.6094         1022.6094                     1022.6094
#> 250  1027.7117         1027.7117                     1027.7117
#> 251  1028.3928         1028.3928                     1028.3928
#> 252  1030.4287         1030.4287                     1030.4287
#> 253  1030.0804         1030.0804                     1030.0804
#> 254  1028.8421         1028.8421                     1028.8421
#> 255  1032.3198         1032.3198                     1032.3198
#> 256  1038.0510         1038.0510                     1038.0510
#> 257  1026.0355         1026.0355                     1026.0355
#> 258  1028.8992         1028.8992                     1028.8992
#> 259  1030.7728         1030.7728                     1030.7728
#> 260  1028.6465         1028.6465                     1028.6465
#> 261  1033.4015         1033.4015                     1033.4015
#> 262  1031.4553         1031.4553                     1031.4553
#> 263  1030.0337         1030.0337                     1030.0337
#> 264  1034.3207         1034.3207                     1034.3207
#> 265  1042.9189         1042.9189                     1042.9189
#> 266  1044.2692         1044.2692                     1044.2692
#> 267  1042.1582         1042.1582                     1042.1582
#> 268  1036.2127         1036.2127                     1036.2127
#> 269  1034.5575         1034.5575                     1034.5575
#> 270  1029.8584         1029.8584                     1029.8584
#> 271  1028.5637         1028.5637                     1028.5637
#> 272  1030.5356         1030.5356                     1030.5356
#> 273  1026.2763         1026.2763                     1026.2763
#> 274  1039.5221         1039.5221                     1039.5221
#> 275  1040.3022         1040.3022                     1040.3022
#> 276  1045.9532         1045.9532                     1045.9532
#> 277  1034.5076         1034.5076                     1034.5076
#> 278  1038.2126         1038.2126                     1038.2126
#> 279  1031.6314         1031.6314                     1031.6314
#> 280  1036.2304         1036.2304                     1036.2304
#> 281  1038.2211         1038.2211                     1038.2211
#> 282  1036.1834         1036.1834                     1036.1834
#> 283  1042.8047         1042.8047                     1042.8047
#> 284  1039.2986         1039.2986                     1039.2986
#> 285  1036.3955         1036.3955                     1036.3955
#> 286  1031.3901         1031.3901                     1031.3901
#> 287  1028.0492         1028.0492                     1028.0492
#> 288  1032.7752         1032.7752                     1032.7752
#> 289  1034.9437         1034.9437                     1034.9437
#> 290  1039.9695         1039.9695                     1039.9695
#> 291  1038.0189         1038.0189                     1038.0189
#> 292  1039.9007         1039.9007                     1039.9007
#> 293  1041.1215         1041.1215                     1041.1215
#> 294  1033.9903         1033.9903                     1033.9903
#> 295  1042.8824         1042.8824                     1042.8824
#> 296  1043.5546         1043.5546                     1043.5546
#> 297  1047.3826         1047.3826                     1047.3826
#> 298  1052.1583         1052.1583                     1052.1583
#> 299  1051.9055         1051.9055                     1051.9055
#> 300  1050.3764         1050.3764                     1050.3764
#> 301  1054.8448         1054.8448                     1054.8448
#> 302  1049.6083         1049.6083                     1049.6083
#> 303  1059.4650         1059.4650                     1059.4650
#> 304  1057.5468         1057.5468                     1057.5468
#> 305  1065.8175         1065.8175                     1065.8175
#> 306  1073.3786         1073.3786                     1073.3786
#> 307  1073.7934         1073.7934                     1073.7934
#> 308  1076.6295         1076.6295                     1076.6295
#> 309  1071.5068         1071.5068                     1071.5068
#> 310  1073.1218         1073.1218                     1073.1218
#> 311  1078.3399         1078.3399                     1078.3399
#> 312  1078.8353         1078.8353                     1078.8353
#> 313  1076.5646         1076.5646                     1076.5646
#> 314  1073.2857         1073.2857                     1073.2857
#> 315  1073.1061         1073.1061                     1073.1061
#> 316  1078.4519         1078.4519                     1078.4519
#> 317  1076.0320         1076.0320                     1076.0320
#> 318  1075.4270         1075.4270                     1075.4270
#> 319  1068.9563         1068.9563                     1068.9563
#> 320  1071.4278         1071.4278                     1071.4278
#> 321  1077.9673         1077.9673                     1077.9673
#> 322  1085.4525         1085.4525                     1085.4525
#> 323  1089.5261         1089.5261                     1089.5261
#> 324  1080.1771         1080.1771                     1080.1771
#> 325  1082.5873         1082.5873                     1082.5873
#> 326  1084.8679         1084.8679                     1084.8679
#> 327  1083.1009         1083.1009                     1083.1009
#> 328  1083.9534         1083.9534                     1083.9534
#> 329  1079.6332         1079.6332                     1079.6332
#> 330  1083.0294         1083.0294                     1083.0294
#> 331  1081.3938         1081.3938                     1081.3938
#> 332  1073.5484         1073.5484                     1073.5484
#> 333  1071.7112         1071.7112                     1071.7112
#> 334  1078.5334         1078.5334                     1078.5334
#> 335  1076.8620         1076.8620                     1076.8620
#> 336  1080.5257         1080.5257                     1080.5257
#> 337  1085.2586         1085.2586                     1085.2586
#> 338  1085.2806         1085.2806                     1085.2806
#> 339  1083.5190         1083.5190                     1083.5190
#> 340  1080.8705         1080.8705                     1080.8705
#> 341  1084.5685         1084.5685                     1084.5685
#> 342  1079.2512         1079.2512                     1079.2512
#> 343  1080.4822         1080.4822                     1080.4822
#> 344  1079.0348         1079.0348                     1079.0348
#> 345  1067.7103         1067.7103                     1067.7103
#> 346  1060.6661         1060.6661                     1060.6661
#> 347  1065.2461         1065.2461                     1065.2461
#> 348  1064.2898         1064.2898                     1064.2898
#> 349  1068.3062         1068.3062                     1068.3062
#> 350  1077.7435         1077.7435                     1077.7435
#> 351  1085.1129         1085.1129                     1085.1129
#> 352  1088.4993         1088.4993                     1088.4993
#> 353  1090.3991         1090.3991                     1090.3991
#> 354  1089.4351         1089.4351                     1089.4351
#> 355  1097.3246         1097.3246                     1097.3246
#> 356  1100.3057         1100.3057                     1100.3057
#> 357  1094.4379         1094.4379                     1094.4379
#> 358  1093.6596         1093.6596                     1093.6596
#> 359  1084.0651         1084.0651                     1084.0651
#> 360  1083.0888         1083.0888                     1083.0888
#> 361  1070.1272         1070.1272                     1070.1272
#> 362  1076.6972         1076.6972                     1076.6972
#> 363  1073.5195         1073.5195                     1073.5195
#> 364  1071.3696         1071.3696                     1071.3696
#> 365  1070.5230         1070.5230                     1070.5230
#> 366  1073.5841         1073.5841                     1073.5841
#> 367  1076.9758         1076.9758                     1076.9758
#> 368  1079.8155         1079.8155                     1079.8155
#> 369  1076.9528         1076.9528                     1076.9528
#> 370  1070.1364         1070.1364                     1070.1364
#> 371  1068.1927         1068.1927                     1068.1927
#> 372  1069.5823         1069.5823                     1069.5823
#> 373  1065.4669         1065.4669                     1065.4669
#> 374  1065.1227         1065.1227                     1065.1227
#> 375  1059.2844         1059.2844                     1059.2844
#> 376  1059.2428         1059.2428                     1059.2428
#> 377  1059.8871         1059.8871                     1059.8871
#> 378  1059.1577         1059.1577                     1059.1577
#> 379  1058.3382         1058.3382                     1058.3382
#> 380  1067.1560         1067.1560                     1067.1560
#> 381  1070.9689         1070.9689                     1070.9689
#> 382  1076.5260         1076.5260                     1076.5260
#> 383  1071.9100         1071.9100                     1071.9100
#> 384  1072.7317         1072.7317                     1072.7317
#> 385  1078.5058         1078.5058                     1078.5058
#> 386  1078.2232         1078.2232                     1078.2232
#> 387  1067.5764         1067.5764                     1067.5764
#> 388  1069.3007         1069.3007                     1069.3007
#> 389  1059.7759         1059.7759                     1059.7759
#> 390  1055.7200         1055.7200                     1055.7200
#> 391  1062.3401         1062.3401                     1062.3401
#> 392  1065.4182         1065.4182                     1065.4182
#> 393  1070.8766         1070.8766                     1070.8766
#> 394  1072.4096         1072.4096                     1072.4096
#> 395  1071.8588         1071.8588                     1071.8588
#> 396  1067.2372         1067.2372                     1067.2372
#> 397  1075.2018         1075.2018                     1075.2018
#> 398  1075.4269         1075.4269                     1075.4269
#> 399  1071.8512         1071.8512                     1071.8512
#> 400  1076.1773         1076.1773                     1076.1773
#> 401  1081.5495         1081.5495                     1081.5495
#> 402  1091.0278         1091.0278                     1091.0278
#> 403  1088.0128         1088.0128                     1088.0128
#> 404  1086.0585         1086.0585                     1086.0585
#> 405  1083.9774         1083.9774                     1083.9774
#> 406  1082.0991         1082.0991                     1082.0991
#> 407  1080.2659         1080.2659                     1080.2659
#> 408  1078.7876         1078.7876                     1078.7876
#> 409  1085.9967         1085.9967                     1085.9967
#> 410  1082.5090         1082.5090                     1082.5090
#> 411  1080.5681         1080.5681                     1080.5681
#> 412  1083.8308         1083.8308                     1083.8308
#> 413  1089.4547         1089.4547                     1089.4547
#> 414  1085.5941         1085.5941                     1085.5941
#> 415  1083.0537         1083.0537                     1083.0537
#> 416  1085.6718         1085.6718                     1085.6718
#> 417  1090.7606         1090.7606                     1090.7606
#> 418  1089.5047         1089.5047                     1089.5047
#> 419  1082.3548         1082.3548                     1082.3548
#> 420  1090.9004         1090.9004                     1090.9004
#> 421  1098.0757         1098.0757                     1098.0757
#> 422  1094.5239         1094.5239                     1094.5239
#> 423  1094.1985         1094.1985                     1094.1985
#> 424  1085.4012         1085.4012                     1085.4012
#> 425  1088.2498         1088.2498                     1088.2498
#> 426  1096.3115         1096.3115                     1096.3115
#> 427  1088.1251         1088.1251                     1088.1251
#> 428  1084.2273         1084.2273                     1084.2273
#> 429  1081.0214         1081.0214                     1081.0214
#> 430  1077.6157         1077.6157                     1077.6157
#> 431  1067.4493         1067.4493                     1067.4493
#> 432  1069.9541         1069.9541                     1069.9541
#> 433  1062.2951         1062.2951                     1062.2951
#> 434  1062.1702         1062.1702                     1062.1702
#> 435  1065.1351         1065.1351                     1065.1351
#> 436  1064.1441         1064.1441                     1064.1441
#> 437  1068.6041         1068.6041                     1068.6041
#> 438  1068.4756         1068.4756                     1068.4756
#> 439  1065.2373         1065.2373                     1065.2373
#> 440  1068.4691         1068.4691                     1068.4691
#> 441  1066.2999         1066.2999                     1066.2999
#> 442  1075.1630         1075.1630                     1075.1630
#> 443  1075.0717         1075.0717                     1075.0717
#> 444  1079.3357         1079.3357                     1079.3357
#> 445  1080.3616         1080.3616                     1080.3616
#> 446  1065.3213         1065.3213                     1065.3213
#> 447  1058.4907         1058.4907                     1058.4907
#> 448  1056.3702         1056.3702                     1056.3702
#> 449  1057.5543         1057.5543                     1057.5543
#> 450  1045.8406         1045.8406                     1045.8406
#> 451  1050.6491         1050.6491                     1050.6491
#> 452  1047.6270         1047.6270                     1047.6270
#> 453  1043.8626         1043.8626                     1043.8626
#> 454  1036.0846         1036.0846                     1036.0846
#> 455  1028.8151         1028.8151                     1028.8151
#> 456  1029.0967         1029.0967                     1029.0967
#> 457  1031.6436         1031.6436                     1031.6436
#> 458  1021.1542         1021.1542                     1021.1542
#> 459  1016.1324         1016.1324                     1016.1324
#> 460  1018.8112         1018.8112                     1018.8112
#> 461  1016.5460         1016.5460                     1016.5460
#> 462  1027.3729         1027.3729                     1027.3729
#> 463  1033.6016         1033.6016                     1033.6016
#> 464  1036.5791         1036.5791                     1036.5791
#> 465  1036.6035         1036.6035                     1036.6035
#> 466  1038.0003         1038.0003                     1038.0003
#> 467  1034.4708         1034.4708                     1034.4708
#> 468  1037.6109         1037.6109                     1037.6109
#> 469  1045.0120         1045.0120                     1045.0120
#> 470  1050.4291         1050.4291                     1050.4291
#> 471  1046.3629         1046.3629                     1046.3629
#> 472  1038.2685         1038.2685                     1038.2685
#> 473  1037.7202         1037.7202                     1037.7202
#> 474  1039.9247         1039.9247                     1039.9247
#> 475  1046.6796         1046.6796                     1046.6796
#> 476  1040.0866         1040.0866                     1040.0866
#> 477  1041.9085         1041.9085                     1041.9085
#> 478  1043.0760         1043.0760                     1043.0760
#> 479  1049.0458         1049.0458                     1049.0458
#> 480  1048.9062         1048.9062                     1048.9062
#> 481  1047.1197         1047.1197                     1047.1197
#> 482  1041.3857         1041.3857                     1041.3857
#> 483  1038.7986         1038.7986                     1038.7986
#> 484  1036.9880         1036.9880                     1036.9880
#> 485  1048.7407         1048.7407                     1048.7407
#> 486  1060.9734         1060.9734                     1060.9734
#> 487  1060.1399         1060.1399                     1060.1399
#> 488  1054.9215         1054.9215                     1054.9215
#> 489  1045.0569         1045.0569                     1045.0569
#> 490  1047.6302         1047.6302                     1047.6302
#> 491  1042.1773         1042.1773                     1042.1773
#> 492  1053.6006         1053.6006                     1053.6006
#> 493  1049.1725         1049.1725                     1049.1725
#> 494  1049.7281         1049.7281                     1049.7281
#> 495  1068.7795         1068.7795                     1068.7795
#> 496  1063.2349         1063.2349                     1063.2349
#> 497  1064.7727         1064.7727                     1064.7727
#> 498  1059.2383         1059.2383                     1059.2383
#> 499  1060.9765         1060.9765                     1060.9765
#> 500  1056.6102         1056.6102                     1056.6102
#> 501  1056.9967         1056.9967                     1056.9967
#> 502  1055.5124         1055.5124                     1055.5124
#> 503  1049.5962         1049.5962                     1049.5962
#> 504  1049.6526         1049.6526                     1049.6526
#> 505  1054.6107         1054.6107                     1054.6107
#> 506  1062.5805         1062.5805                     1062.5805
#> 507  1055.7169         1055.7169                     1055.7169
#> 508  1054.4689         1054.4689                     1054.4689
#> 509  1060.2660         1060.2660                     1060.2660
#> 510  1054.6949         1054.6949                     1054.6949
#> 511  1042.0524         1042.0524                     1042.0524
#> 512  1037.3729         1037.3729                     1037.3729
#> 513  1032.5367         1032.5367                     1032.5367
#> 514  1032.7741         1032.7741                     1032.7741
#> 515  1030.7554         1030.7554                     1030.7554
#> 516  1031.9129         1031.9129                     1031.9129
#> 517  1029.8011         1029.8011                     1029.8011
#> 518  1031.6716         1031.6716                     1031.6716
#> 519  1029.8416         1029.8416                     1029.8416
#> 520  1035.7921         1035.7921                     1035.7921
#> 521  1032.1055         1032.1055                     1032.1055
#> 522  1033.5588         1033.5588                     1033.5588
#> 523  1029.1346         1029.1346                     1029.1346
#> 524  1030.1746         1030.1746                     1030.1746
#> 525  1029.9360         1029.9360                     1029.9360
#> 526  1021.5133         1021.5133                     1021.5133
#> 527  1020.7922         1020.7922                     1020.7922
#> 528  1026.6933         1026.6933                     1026.6933
#> 529  1030.1003         1030.1003                     1030.1003
#> 530  1030.8165         1030.8165                     1030.8165
#> 531  1024.8549         1024.8549                     1024.8549
#> 532  1030.7011         1030.7011                     1030.7011
#> 533  1031.0971         1031.0971                     1031.0971
#> 534  1028.8382         1028.8382                     1028.8382
#> 535  1037.0484         1037.0484                     1037.0484
#> 536  1033.2004         1033.2004                     1033.2004
#> 537  1034.7172         1034.7172                     1034.7172
#> 538  1041.1259         1041.1259                     1041.1259
#> 539  1044.1370         1044.1370                     1044.1370
#> 540  1042.6019         1042.6019                     1042.6019
#> 541  1040.5098         1040.5098                     1040.5098
#> 542  1042.2855         1042.2855                     1042.2855
#> 543  1044.8529         1044.8529                     1044.8529
#> 544  1044.9459         1044.9459                     1044.9459
#> 545  1051.5382         1051.5382                     1051.5382
#> 546  1051.2090         1051.2090                     1051.2090
#> 547  1047.7075         1047.7075                     1047.7075
#> 548  1050.3942         1050.3942                     1050.3942
#> 549  1039.3853         1039.3853                     1039.3853
#> 550  1041.3451         1041.3451                     1041.3451
#> 551  1043.8299         1043.8299                     1043.8299
#> 552  1042.7056         1042.7056                     1042.7056
#> 553  1037.1198         1037.1198                     1037.1198
#> 554  1035.1449         1035.1449                     1035.1449
#> 555  1042.8940         1042.8940                     1042.8940
#> 556  1039.1764         1039.1764                     1039.1764
#> 557  1027.5179         1027.5179                     1027.5179
#> 558  1031.5791         1031.5791                     1031.5791
#> 559  1029.0726         1029.0726                     1029.0726
#> 560  1026.5181         1026.5181                     1026.5181
#> 561  1020.4413         1020.4413                     1020.4413
#> 562  1020.3285         1020.3285                     1020.3285
#> 563  1023.8347         1023.8347                     1023.8347
#> 564  1020.8973         1020.8973                     1020.8973
#> 565  1017.8637         1017.8637                     1017.8637
#> 566  1023.3469         1023.3469                     1023.3469
#> 567  1022.1093         1022.1093                     1022.1093
#> 568  1021.3098         1021.3098                     1021.3098
#> 569  1018.1809         1018.1809                     1018.1809
#> 570  1022.6831         1022.6831                     1022.6831
#> 571  1017.7121         1017.7121                     1017.7121
#> 572  1021.9584         1021.9584                     1021.9584
#> 573  1025.9869         1025.9869                     1025.9869
#> 574  1023.6489         1023.6489                     1023.6489
#> 575  1027.8910         1027.8910                     1027.8910
#> 576  1032.8248         1032.8248                     1032.8248
#> 577  1035.7029         1035.7029                     1035.7029
#> 578  1045.8271         1045.8271                     1045.8271
#> 579  1036.0154         1036.0154                     1036.0154
#> 580  1030.1908         1030.1908                     1030.1908
#> 581  1023.3082         1023.3082                     1023.3082
#> 582  1024.1466         1024.1466                     1024.1466
#> 583  1032.0697         1032.0697                     1032.0697
#> 584  1040.4592         1040.4592                     1040.4592
#> 585  1042.9006         1042.9006                     1042.9006
#> 586  1047.2940         1047.2940                     1047.2940
#> 587  1046.5696         1046.5696                     1046.5696
#> 588  1048.9145         1048.9145                     1048.9145
#> 589  1050.7957         1050.7957                     1050.7957
#> 590  1046.9905         1046.9905                     1046.9905
#> 591  1045.5240         1045.5240                     1045.5240
#> 592  1044.8498         1044.8498                     1044.8498
#> 593  1051.8190         1051.8190                     1051.8190
#> 594  1046.6341         1046.6341                     1046.6341
#> 595  1036.0624         1036.0624                     1036.0624
#> 596  1039.9038         1039.9038                     1039.9038
#> 597  1035.8230         1035.8230                     1035.8230
#> 598  1033.6424         1033.6424                     1033.6424
#> 599  1038.1660         1038.1660                     1038.1660
#> 600  1034.3505         1034.3505                     1034.3505
#> 601  1032.6452         1032.6452                     1032.6452
#> 602  1040.1573         1040.1573                     1040.1573
#> 603  1042.7989         1042.7989                     1042.7989
#> 604  1045.5098         1045.5098                     1045.5098
#> 605  1044.8265         1044.8265                     1044.8265
#> 606  1039.1428         1039.1428                     1039.1428
#> 607  1031.6597         1031.6597                     1031.6597
#> 608  1030.5427         1030.5427                     1030.5427
#> 609  1040.5513         1040.5513                     1040.5513
#> 610  1041.6598         1041.6598                     1041.6598
#> 611  1042.4817         1042.4817                     1042.4817
#> 612  1044.1448         1044.1448                     1044.1448
#> 613  1042.2188         1042.2188                     1042.2188
#> 614  1035.2250         1035.2250                     1035.2250
#> 615  1048.6037         1048.6037                     1048.6037
#> 616  1046.4853         1046.4853                     1046.4853
#> 617  1044.9923         1044.9923                     1044.9923
#> 618  1036.0306         1036.0306                     1036.0306
#> 619  1034.7905         1034.7905                     1034.7905
#> 620  1033.5540         1033.5540                     1033.5540
#> 621  1032.2765         1032.2765                     1032.2765
#> 622  1023.3418         1023.3418                     1023.3418
#> 623  1032.2651         1032.2651                     1032.2651
#> 624  1041.0830         1041.0830                     1041.0830
#> 625  1044.5310         1044.5310                     1044.5310
#> 626  1039.0273         1039.0273                     1039.0273
#> 627  1042.5999         1042.5999                     1042.5999
#> 628  1041.3675         1041.3675                     1041.3675
#> 629  1039.7686         1039.7686                     1039.7686
#> 630  1046.5818         1046.5818                     1046.5818
#> 631  1040.4424         1040.4424                     1040.4424
#> 632  1037.8863         1037.8863                     1037.8863
#> 633  1034.2303         1034.2303                     1034.2303
#> 634  1034.3291         1034.3291                     1034.3291
#> 635  1026.4648         1026.4648                     1026.4648
#> 636  1022.9481         1022.9481                     1022.9481
#> 637  1026.5277         1026.5277                     1026.5277
#> 638  1028.8538         1028.8538                     1028.8538
#> 639  1023.9843         1023.9843                     1023.9843
#> 640  1026.7804         1026.7804                     1026.7804
#> 641  1014.6172         1014.6172                     1014.6172
#> 642  1012.9148         1012.9148                     1012.9148
#> 643  1016.4799         1016.4799                     1016.4799
#> 644  1013.1848         1013.1848                     1013.1848
#> 645  1013.0027         1013.0027                     1013.0027
#> 646  1005.0363         1005.0363                     1005.0363
#> 647  1009.2753         1009.2753                     1009.2753
#> 648  1000.0233         1000.0233                     1000.0233
#> 649   998.4051          998.4051                      998.4051
#> 650   997.1288          997.1288                      997.1288
#> 651   997.4334          997.4334                      997.4334
#> 652   993.3160          993.3160                      993.3160
#> 653  1002.4646         1002.4646                     1002.4646
#> 654   995.3151          995.3151                      995.3151
#> 655   996.5857          996.5857                      996.5857
#> 656   981.8869          981.8869                      981.8869
#> 657   981.8990          981.8990                      981.8990
#> 658   984.4473          984.4473                      984.4473
#> 659   979.0237          979.0237                      979.0237
#> 660   982.5478          982.5478                      982.5478
#> 661   984.2027          984.2027                      984.2027
#> 662   989.0844          989.0844                      989.0844
#> 663   984.8677          984.8677                      984.8677
#> 664   980.0148          980.0148                      980.0148
#> 665   971.1571          971.1571                      971.1571
#> 666   969.5448          969.5448                      969.5448
#> 667   962.8508          962.8508                      962.8508
#> 668   966.2915          966.2915                      966.2915
#> 669   966.6479          966.6479                      966.6479
#> 670   977.5967          977.5967                      977.5967
#> 671   971.8082          971.8082                      971.8082
#> 672   977.7166          977.7166                      977.7166
#> 673   975.0798          975.0798                      975.0798
#> 674   967.7966          967.7966                      967.7966
#> 675   970.6615          970.6615                      970.6615
#> 676   963.4946          963.4946                      963.4946
#> 677   958.2186          958.2186                      958.2186
#> 678   954.5531          954.5531                      954.5531
#> 679   955.6076          955.6076                      955.6076
#> 680   950.6130          950.6130                      950.6130
#> 681   956.0023          956.0023                      956.0023
#> 682   950.0074          950.0074                      950.0074
#> 683   951.0906          951.0906                      951.0906
#> 684   951.8060          951.8060                      951.8060
#> 685   946.4773          946.4773                      946.4773
#> 686   944.3341          944.3341                      944.3341
#> 687   941.0533          941.0533                      941.0533
#> 688   945.8502          945.8502                      945.8502
#> 689   953.6305          953.6305                      953.6305
#> 690   948.4265          948.4265                      948.4265
#> 691   953.0794          953.0794                      953.0794
#> 692   952.7021          952.7021                      952.7021
#> 693   942.8662          942.8662                      942.8662
#> 694   939.0866          939.0866                      939.0866
#> 695   941.3924          941.3924                      941.3924
#> 696   942.1179          942.1179                      942.1179
#> 697   929.9064          929.9064                      929.9064
#> 698   932.8080          932.8080                      932.8080
#> 699   936.0832          936.0832                      936.0832
#> 700   934.5607          934.5607                      934.5607
#> 701   931.0228          931.0228                      931.0228
#> 702   940.8807          940.8807                      940.8807
#> 703   940.4307          940.4307                      940.4307
#> 704   940.3606          940.3606                      940.3606
#> 705   934.7433          934.7433                      934.7433
#> 706   928.0227          928.0227                      928.0227
#> 707   920.4069          920.4069                      920.4069
#> 708   918.2971          918.2971                      918.2971
#> 709   925.1017          925.1017                      925.1017
#> 710   933.8707          933.8707                      933.8707
#> 711   941.7125          941.7125                      941.7125
#> 712   948.1963          948.1963                      948.1963
#> 713   947.0083          947.0083                      947.0083
#> 714   940.8875          940.8875                      940.8875
#> 715   939.2485          939.2485                      939.2485
#> 716   927.1862          927.1862                      927.1862
#> 717   925.6172          925.6172                      925.6172
#> 718   933.9166          933.9166                      933.9166
#> 719   934.5714          934.5714                      934.5714
#> 720   940.0508          940.0508                      940.0508
#> 721   942.4976          942.4976                      942.4976
#> 722   938.6030          938.6030                      938.6030
#> 723   947.3208          947.3208                      947.3208
#> 724   946.9289          946.9289                      946.9289
#> 725   942.0511          942.0511                      942.0511
#> 726   942.4044          942.4044                      942.4044
#> 727   934.8114          934.8114                      934.8114
#> 728   939.1303          939.1303                      939.1303
#> 729   941.6381          941.6381                      941.6381
#> 730   939.8642          939.8642                      939.8642
#> 731   937.4221          937.4221                      937.4221
#> 732   942.1035          942.1035                      942.1035
#> 733   936.7915          936.7915                      936.7915
#> 734   931.8724          931.8724                      931.8724
#> 735   933.9936          933.9936                      933.9936
#> 736   931.7371          931.7371                      931.7371
#> 737   936.3625          936.3625                      936.3625
#> 738   935.3694          935.3694                      935.3694
#> 739   941.3436          941.3436                      941.3436
#> 740   943.8214          943.8214                      943.8214
#> 741   932.5956          932.5956                      932.5956
#> 742   925.9188          925.9188                      925.9188
#> 743   932.3326          932.3326                      932.3326
#> 744   935.7866          935.7866                      935.7866
#> 745   930.9513          930.9513                      930.9513
#> 746   924.2223          924.2223                      924.2223
#> 747   929.3907          929.3907                      929.3907
#> 748   925.3318          925.3318                      925.3318
#> 749   934.3404          934.3404                      934.3404
#> 750   943.1981          943.1981                      943.1981
#> 751   935.9247          935.9247                      935.9247
#> 752   931.6964          931.6964                      931.6964
#> 753   925.4440          925.4440                      925.4440
#> 754   928.7804          928.7804                      928.7804
#> 755   922.3266          922.3266                      922.3266
#> 756   912.1516          912.1516                      912.1516
#> 757   922.2583          922.2583                      922.2583
#> 758   927.2882          927.2882                      927.2882
#> 759   931.3738          931.3738                      931.3738
#> 760   928.0538          928.0538                      928.0538
#> 761   927.9974          927.9974                      927.9974
#> 762   931.0958          931.0958                      931.0958
#> 763   924.6896          924.6896                      924.6896
#> 764   924.0683          924.0683                      924.0683
#> 765   924.9470          924.9470                      924.9470
#> 766   933.4109          933.4109                      933.4109
#> 767   936.6216          936.6216                      936.6216
#> 768   943.0327          943.0327                      943.0327
#> 769   943.7355          943.7355                      943.7355
#> 770   938.1729          938.1729                      938.1729
#> 771   936.4746          936.4746                      936.4746
#> 772   928.1507          928.1507                      928.1507
#> 773   932.7950          932.7950                      932.7950
#> 774   939.8791          939.8791                      939.8791
#> 775   939.5655          939.5655                      939.5655
#> 776   934.6610          934.6610                      934.6610
#> 777   940.0968          940.0968                      940.0968
#> 778   940.7934          940.7934                      940.7934
#> 779   938.8620          938.8620                      938.8620
#> 780   944.4800          944.4800                      944.4800
#> 781   940.6807          940.6807                      940.6807
#> 782   946.4255          946.4255                      946.4255
#> 783   942.2132          942.2132                      942.2132
#> 784   944.1702          944.1702                      944.1702
#> 785   948.6271          948.6271                      948.6271
#> 786   941.9508          941.9508                      941.9508
#> 787   943.9414          943.9414                      943.9414
#> 788   943.3835          943.3835                      943.3835
#> 789   946.7622          946.7622                      946.7622
#> 790   942.8192          942.8192                      942.8192
#> 791   942.3843          942.3843                      942.3843
#> 792   949.2957          949.2957                      949.2957
#> 793   950.1382          950.1382                      950.1382
#> 794   954.2541          954.2541                      954.2541
#> 795   953.1496          953.1496                      953.1496
#> 796   948.0027          948.0027                      948.0027
#> 797   947.9481          947.9481                      947.9481
#> 798   941.8231          941.8231                      941.8231
#> 799   928.8425          928.8425                      928.8425
#> 800   934.6882          934.6882                      934.6882
#> 801   929.2536          929.2536                      929.2536
#> 802   920.1232          920.1232                      920.1232
#> 803   925.0996          925.0996                      925.0996
#> 804   925.0403          925.0403                      925.0403
#> 805   922.0422          922.0422                      922.0422
#> 806   921.1524          921.1524                      921.1524
#> 807   919.0225          919.0225                      919.0225
#> 808   924.0058          924.0058                      924.0058
#> 809   927.6441          927.6441                      927.6441
#> 810   919.0110          919.0110                      919.0110
#> 811   920.7779          920.7779                      920.7779
#> 812   924.4120          924.4120                      924.4120
#> 813   927.7533          927.7533                      927.7533
#> 814   915.6317          915.6317                      915.6317
#> 815   914.4549          914.4549                      914.4549
#> 816   924.3531          924.3531                      924.3531
#> 817   928.3371          928.3371                      928.3371
#> 818   919.7907          919.7907                      919.7907
#> 819   911.4724          911.4724                      911.4724
#> 820   913.9279          913.9279                      913.9279
#> 821   913.0576          913.0576                      913.0576
#> 822   917.8641          917.8641                      917.8641
#> 823   919.3332          919.3332                      919.3332
#> 824   919.7382          919.7382                      919.7382
#> 825   920.6565          920.6565                      920.6565
#> 826   921.4878          921.4878                      921.4878
#> 827   915.1398          915.1398                      915.1398
#> 828   926.8873          926.8873                      926.8873
#> 829   919.8272          919.8272                      919.8272
#> 830   919.7424          919.7424                      919.7424
#> 831   917.0208          917.0208                      917.0208
#> 832   926.0214          926.0214                      926.0214
#> 833   931.0786          931.0786                      931.0786
#> 834   928.2600          928.2600                      928.2600
#> 835   929.2871          929.2871                      929.2871
#> 836   935.1144          935.1144                      935.1144
#> 837   946.2960          946.2960                      946.2960
#> 838   947.8074          947.8074                      947.8074
#> 839   942.5948          942.5948                      942.5948
#> 840   937.6771          937.6771                      937.6771
#> 841   947.7057          947.7057                      947.7057
#> 842   937.3529          937.3529                      937.3529
#> 843   952.6316          952.6316                      952.6316
#> 844   951.3248          951.3248                      951.3248
#> 845   949.0529          949.0529                      949.0529
#> 846   949.8407          949.8407                      949.8407
#> 847   954.5076          954.5076                      954.5076
#> 848   956.0217          956.0217                      956.0217
#> 849   946.2410          946.2410                      946.2410
#> 850   948.0087          948.0087                      948.0087
#> 851   950.2608          950.2608                      950.2608
#> 852   953.5586          953.5586                      953.5586
#> 853   948.4014          948.4014                      948.4014
#> 854   936.5463          936.5463                      936.5463
#> 855   934.9235          934.9235                      934.9235
#> 856   930.2020          930.2020                      930.2020
#> 857   926.3725          926.3725                      926.3725
#> 858   921.6036          921.6036                      921.6036
#> 859   919.6136          919.6136                      919.6136
#> 860   918.0575          918.0575                      918.0575
#> 861   922.0380          922.0380                      922.0380
#> 862   926.9701          926.9701                      926.9701
#> 863   922.9975          922.9975                      922.9975
#> 864   921.4534          921.4534                      921.4534
#> 865   923.2606          923.2606                      923.2606
#> 866   930.2545          930.2545                      930.2545
#> 867   929.9742          929.9742                      929.9742
#> 868   921.4798          921.4798                      921.4798
#> 869   922.6391          922.6391                      922.6391
#> 870   922.0436          922.0436                      922.0436
#> 871   930.9061          930.9061                      930.9061
#> 872   932.6232          932.6232                      932.6232
#> 873   929.5080          929.5080                      929.5080
#> 874   927.3103          927.3103                      927.3103
#> 875   924.7839          924.7839                      924.7839
#> 876   925.7140          925.7140                      925.7140
#> 877   926.5961          926.5961                      926.5961
#> 878   931.1754          931.1754                      931.1754
#> 879   932.7763          932.7763                      932.7763
#> 880   930.9428          930.9428                      930.9428
#> 881   926.2398          926.2398                      926.2398
#> 882   929.4133          929.4133                      929.4133
#> 883   929.1008          929.1008                      929.1008
#> 884   930.0150          930.0150                      930.0150
#> 885   935.5332          935.5332                      935.5332
#> 886   944.2934          944.2934                      944.2934
#> 887   939.5243          939.5243                      939.5243
#> 888   947.7447          947.7447                      947.7447
#> 889   943.4111          943.4111                      943.4111
#> 890   944.7428          944.7428                      944.7428
#> 891   945.8547          945.8547                      945.8547
#> 892   944.4701          944.4701                      944.4701
#> 893   951.4414          951.4414                      951.4414
#> 894   948.1468          948.1468                      948.1468
#> 895   951.4495          951.4495                      951.4495
#> 896   951.3832          951.3832                      951.3832
#> 897   946.7258          946.7258                      946.7258
#> 898   952.7993          952.7993                      952.7993
#> 899   942.3576          942.3576                      942.3576
#> 900   939.7268          939.7268                      939.7268
#> 901   932.0198          932.0198                      932.0198
#> 902   932.9914          932.9914                      932.9914
#> 903   934.3135          934.3135                      934.3135
#> 904   928.7198          928.7198                      928.7198
#> 905   931.9746          931.9746                      931.9746
#> 906   926.8101          926.8101                      926.8101
#> 907   930.1061          930.1061                      930.1061
#> 908   931.2952          931.2952                      931.2952
#> 909   934.8716          934.8716                      934.8716
#> 910   930.1792          930.1792                      930.1792
#> 911   930.6560          930.6560                      930.6560
#> 912   928.3419          928.3419                      928.3419
#> 913   920.9975          920.9975                      920.9975
#> 914   921.7609          921.7609                      921.7609
#> 915   930.6297          930.6297                      930.6297
#> 916   927.3894          927.3894                      927.3894
#> 917   926.3903          926.3903                      926.3903
#> 918   929.8365          929.8365                      929.8365
#> 919   930.0172          930.0172                      930.0172
#> 920   939.7349          939.7349                      939.7349
#> 921   943.4210          943.4210                      943.4210
#> 922   955.0276          955.0276                      955.0276
#> 923   956.7722          956.7722                      956.7722
#> 924   951.1026          951.1026                      951.1026
#> 925   953.2093          953.2093                      953.2093
#> 926   948.5865          948.5865                      948.5865
#> 927   943.5512          943.5512                      943.5512
#> 928   942.6038          942.6038                      942.6038
#> 929   947.2734          947.2734                      947.2734
#> 930   948.9929          948.9929                      948.9929
#> 931   953.0630          953.0630                      953.0630
#> 932   957.6397          957.6397                      957.6397
#> 933   956.7805          956.7805                      956.7805
#> 934   944.7693          944.7693                      944.7693
#> 935   948.7489          948.7489                      948.7489
#> 936   959.5944          959.5944                      959.5944
#> 937   959.8864          959.8864                      959.8864
#> 938   953.1118          953.1118                      953.1118
#> 939   951.2740          951.2740                      951.2740
#> 940   946.6014          946.6014                      946.6014
#> 941   946.3933          946.3933                      946.3933
#> 942   949.7738          949.7738                      949.7738
#> 943   954.1060          954.1060                      954.1060
#> 944   955.2819          955.2819                      955.2819
#> 945   950.6120          950.6120                      950.6120
#> 946   954.6783          954.6783                      954.6783
#> 947   961.4199          961.4199                      961.4199
#> 948   972.6793          972.6793                      972.6793
#> 949   970.2110          970.2110                      970.2110
#> 950   972.5815          972.5815                      972.5815
#> 951   978.5499          978.5499                      978.5499
#> 952   977.9680          977.9680                      977.9680
#> 953   980.5923          980.5923                      980.5923
#> 954   981.6644          981.6644                      981.6644
#> 955   980.9921          980.9921                      980.9921
#> 956   981.8347          981.8347                      981.8347
#> 957   986.6583          986.6583                      986.6583
#> 958   988.7022          988.7022                      988.7022
#> 959   986.3700          986.3700                      986.3700
#> 960   975.1711          975.1711                      975.1711
#> 961   971.1958          971.1958                      971.1958
#> 962   971.0960          971.0960                      971.0960
#> 963   958.5239          958.5239                      958.5239
#> 964   969.5787          969.5787                      969.5787
#> 965   962.1349          962.1349                      962.1349
#> 966   956.3311          956.3311                      956.3311
#> 967   963.6198          963.6198                      963.6198
#> 968   952.6704          952.6704                      952.6704
#> 969   956.3657          956.3657                      956.3657
#> 970   954.6457          954.6457                      954.6457
#> 971   956.9251          956.9251                      956.9251
#> 972   961.3156          961.3156                      961.3156
#> 973   956.5233          956.5233                      956.5233
#> 974   952.9941          952.9941                      952.9941
#> 975   938.0093          938.0093                      938.0093
#> 976   933.2041          933.2041                      933.2041
#> 977   935.1050          935.1050                      935.1050
#> 978   937.6303          937.6303                      937.6303
#> 979   947.7656          947.7656                      947.7656
#> 980   948.0887          948.0887                      948.0887
#> 981   950.4070          950.4070                      950.4070
#> 982   950.7809          950.7809                      950.7809
#> 983   948.3468          948.3468                      948.3468
#> 984   952.0913          952.0913                      952.0913
#> 985   954.4125          954.4125                      954.4125
#> 986   955.0596          955.0596                      955.0596
#> 987   950.9823          950.9823                      950.9823
#> 988   950.7815          950.7815                      950.7815
#> 989   954.6886          954.6886                      954.6886
#> 990   958.0709          958.0709                      958.0709
#> 991   955.6576          955.6576                      955.6576
#> 992   952.3120          952.3120                      952.3120
#> 993   954.8761          954.8761                      954.8761
#> 994   960.1206          960.1206                      960.1206
#> 995   960.7259          960.7259                      960.7259
#> 996   959.1594          959.1594                      959.1594
#> 997   954.7561          954.7561                      954.7561
#> 998   952.6596          952.6596                      952.6596
#> 999   945.2459          945.2459                      945.2459
#> 1000  941.7593          941.7593                      941.7593
#>      pupil_raw_deblink_detransient_interpolate
#> 1                                     996.8677
#> 2                                     997.7859
#> 3                                     993.6078
#> 4                                    1001.5842
#> 5                                    1003.2317
#> 6                                     999.1294
#> 7                                    1001.5666
#> 8                                    1005.2582
#> 9                                    1008.1371
#> 10                                   1006.6101
#> 11                                   1014.1690
#> 12                                   1016.1183
#> 13                                   1013.0121
#> 14                                   1001.9386
#> 15                                   1007.5632
#> 16                                   1007.3385
#> 17                                   1007.2576
#> 18                                   1011.9768
#> 19                                   1016.0829
#> 20                                   1019.0524
#> 21                                   1023.6473
#> 22                                   1027.5580
#> 23                                   1027.9308
#> 24                                   1017.9840
#> 25                                   1021.0832
#> 26                                   1020.8025
#> 27                                   1020.0235
#> 28                                   1012.6698
#> 29                                   1010.2790
#> 30                                   1012.3687
#> 31                                   1019.1621
#> 32                                   1018.6482
#> 33                                   1020.5865
#> 34                                   1020.3175
#> 35                                   1013.4322
#> 36                                   1011.3572
#> 37                                   1009.3858
#> 38                                   1009.0892
#> 39                                   1014.5894
#> 40                                   1018.4052
#> 41                                   1017.5826
#> 42                                   1016.3158
#> 43                                   1019.8006
#> 44                                   1022.5839
#> 45                                   1019.1402
#> 46                                   1015.6027
#> 47                                   1017.4256
#> 48                                   1021.2683
#> 49                                   1020.7065
#> 50                                   1025.1121
#> 51                                   1027.1026
#> 52                                   1024.0425
#> 53                                   1025.7481
#> 54                                   1020.1013
#> 55                                   1027.2664
#> 56                                   1037.1684
#> 57                                   1035.3323
#> 58                                   1030.1116
#> 59                                   1032.9602
#> 60                                   1032.2849
#> 61                                   1044.2930
#> 62                                   1044.0968
#> 63                                   1047.5455
#> 64                                   1047.6855
#> 65                                   1043.9691
#> 66                                   1044.9131
#> 67                                   1035.8883
#> 68                                   1043.2161
#> 69                                   1043.9824
#> 70                                   1054.8454
#> 71                                   1057.2230
#> 72                                   1053.6732
#> 73                                   1056.7269
#> 74                                   1052.0564
#> 75                                   1045.7882
#> 76                                   1047.2454
#> 77                                   1045.0290
#> 78                                   1045.0345
#> 79                                   1045.4062
#> 80                                   1042.4586
#> 81                                   1039.6153
#> 82                                   1038.9394
#> 83                                   1044.8298
#> 84                                   1037.2120
#> 85                                   1040.1817
#> 86                                   1041.8465
#> 87                                   1047.1619
#> 88                                   1045.6410
#> 89                                   1047.4911
#> 90                                   1048.8266
#> 91                                   1046.1140
#> 92                                   1052.1534
#> 93                                   1057.9554
#> 94                                   1061.4564
#> 95                                   1069.3906
#> 96                                   1072.1830
#> 97                                   1065.8001
#> 98                                   1062.9337
#> 99                                   1056.8107
#> 100                                  1054.4437
#> 101                                  1051.3419
#> 102                                  1051.5524
#> 103                                  1046.9978
#> 104                                  1047.7880
#> 105                                  1044.5150
#> 106                                  1053.3515
#> 107                                  1056.9350
#> 108                                  1061.4859
#> 109                                  1063.4068
#> 110                                  1071.8177
#> 111                                  1068.6390
#> 112                                  1066.3308
#> 113                                  1073.4922
#> 114                                  1070.2387
#> 115                                  1069.2018
#> 116                                  1067.2378
#> 117                                  1065.6378
#> 118                                  1064.2422
#> 119                                  1066.7132
#> 120                                  1065.8265
#> 121                                  1063.2967
#> 122                                  1070.0119
#> 123                                  1068.9390
#> 124                                  1068.0413
#> 125                                  1067.5403
#> 126                                  1071.1036
#> 127                                  1070.7358
#> 128                                  1070.5476
#> 129                                  1067.1393
#> 130                                  1065.5180
#> 131                                  1065.8188
#> 132                                  1062.8743
#> 133                                  1065.5318
#> 134                                  1057.9398
#> 135                                  1059.4726
#> 136                                  1051.7904
#> 137                                  1050.2855
#> 138                                  1047.6441
#> 139                                  1044.3836
#> 140                                  1044.0991
#> 141                                  1034.5273
#> 142                                  1040.4103
#> 143                                  1032.0854
#> 144                                  1029.7677
#> 145                                  1024.1881
#> 146                                  1020.4340
#> 147                                  1030.8699
#> 148                                  1030.9569
#> 149                                  1024.5254
#> 150                                  1016.3223
#> 151                                  1018.5733
#> 152                                  1018.4805
#> 153                                  1016.8901
#> 154                                  1012.2433
#> 155                                  1004.8060
#> 156                                   999.4300
#> 157                                  1004.4302
#> 158                                  1001.3239
#> 159                                   994.4017
#> 160                                  1003.7482
#> 161                                  1005.8737
#> 162                                  1004.6804
#> 163                                  1009.9729
#> 164                                  1014.4050
#> 165                                  1011.3088
#> 166                                  1022.3393
#> 167                                  1021.0641
#> 168                                  1013.9417
#> 169                                  1013.2197
#> 170                                  1014.2574
#> 171                                  1025.7972
#> 172                                  1026.3263
#> 173                                  1028.6112
#> 174                                  1028.2255
#> 175                                  1026.5555
#> 176                                  1026.3818
#> 177                                  1030.3200
#> 178                                  1040.6963
#> 179                                  1045.8332
#> 180                                  1051.8728
#> 181                                  1045.7162
#> 182                                  1050.6356
#> 183                                  1051.7353
#> 184                                  1044.3990
#> 185                                  1047.0041
#> 186                                  1046.2104
#> 187                                  1053.5333
#> 188                                  1049.7029
#> 189                                  1047.5518
#> 190                                  1042.9213
#> 191                                  1042.0358
#> 192                                  1044.0458
#> 193                                  1040.3871
#> 194                                  1044.5389
#> 195                                  1038.4985
#> 196                                  1033.2586
#> 197                                  1040.4644
#> 198                                  1035.3852
#> 199                                  1037.4450
#> 200                                  1035.5396
#> 201                                  1037.5867
#> 202                                  1046.0310
#> 203                                  1053.9640
#> 204                                  1052.3094
#> 205                                  1040.8832
#> 206                                  1053.3716
#> 207                                  1056.7069
#> 208                                  1059.4135
#> 209                                  1059.3465
#> 210                                  1061.8971
#> 211                                  1061.0752
#> 212                                  1063.1787
#> 213                                  1061.1774
#> 214                                  1054.3264
#> 215                                  1059.2656
#> 216                                  1066.8643
#> 217                                  1065.3206
#> 218                                  1059.0542
#> 219                                  1062.2654
#> 220                                  1062.0418
#> 221                                  1053.3757
#> 222                                  1053.3864
#> 223                                  1050.2349
#> 224                                  1048.5300
#> 225                                  1042.7472
#> 226                                  1051.7629
#> 227                                  1050.1072
#> 228                                  1042.0797
#> 229                                  1043.0656
#> 230                                  1044.3815
#> 231                                  1039.4524
#> 232                                  1025.0078
#> 233                                  1021.8054
#> 234                                  1024.6579
#> 235                                  1024.3593
#> 236                                  1023.8684
#> 237                                  1026.6725
#> 238                                  1020.7402
#> 239                                  1026.2241
#> 240                                  1026.1974
#> 241                                  1029.7339
#> 242                                  1034.9045
#> 243                                  1036.0219
#> 244                                  1031.6283
#> 245                                  1037.4431
#> 246                                  1027.4423
#> 247                                  1024.7184
#> 248                                  1023.4400
#> 249                                  1022.6094
#> 250                                  1027.7117
#> 251                                  1028.3928
#> 252                                  1030.4287
#> 253                                  1030.0804
#> 254                                  1028.8421
#> 255                                  1032.3198
#> 256                                  1038.0510
#> 257                                  1026.0355
#> 258                                  1028.8992
#> 259                                  1030.7728
#> 260                                  1028.6465
#> 261                                  1033.4015
#> 262                                  1031.4553
#> 263                                  1030.0337
#> 264                                  1034.3207
#> 265                                  1042.9189
#> 266                                  1044.2692
#> 267                                  1042.1582
#> 268                                  1036.2127
#> 269                                  1034.5575
#> 270                                  1029.8584
#> 271                                  1028.5637
#> 272                                  1030.5356
#> 273                                  1026.2763
#> 274                                  1039.5221
#> 275                                  1040.3022
#> 276                                  1045.9532
#> 277                                  1034.5076
#> 278                                  1038.2126
#> 279                                  1031.6314
#> 280                                  1036.2304
#> 281                                  1038.2211
#> 282                                  1036.1834
#> 283                                  1042.8047
#> 284                                  1039.2986
#> 285                                  1036.3955
#> 286                                  1031.3901
#> 287                                  1028.0492
#> 288                                  1032.7752
#> 289                                  1034.9437
#> 290                                  1039.9695
#> 291                                  1038.0189
#> 292                                  1039.9007
#> 293                                  1041.1215
#> 294                                  1033.9903
#> 295                                  1042.8824
#> 296                                  1043.5546
#> 297                                  1047.3826
#> 298                                  1052.1583
#> 299                                  1051.9055
#> 300                                  1050.3764
#> 301                                  1054.8448
#> 302                                  1049.6083
#> 303                                  1059.4650
#> 304                                  1057.5468
#> 305                                  1065.8175
#> 306                                  1073.3786
#> 307                                  1073.7934
#> 308                                  1076.6295
#> 309                                  1071.5068
#> 310                                  1073.1218
#> 311                                  1078.3399
#> 312                                  1078.8353
#> 313                                  1076.5646
#> 314                                  1073.2857
#> 315                                  1073.1061
#> 316                                  1078.4519
#> 317                                  1076.0320
#> 318                                  1075.4270
#> 319                                  1068.9563
#> 320                                  1071.4278
#> 321                                  1077.9673
#> 322                                  1085.4525
#> 323                                  1089.5261
#> 324                                  1080.1771
#> 325                                  1082.5873
#> 326                                  1084.8679
#> 327                                  1083.1009
#> 328                                  1083.9534
#> 329                                  1079.6332
#> 330                                  1083.0294
#> 331                                  1081.3938
#> 332                                  1073.5484
#> 333                                  1071.7112
#> 334                                  1078.5334
#> 335                                  1076.8620
#> 336                                  1080.5257
#> 337                                  1085.2586
#> 338                                  1085.2806
#> 339                                  1083.5190
#> 340                                  1080.8705
#> 341                                  1084.5685
#> 342                                  1079.2512
#> 343                                  1080.4822
#> 344                                  1079.0348
#> 345                                  1067.7103
#> 346                                  1060.6661
#> 347                                  1065.2461
#> 348                                  1064.2898
#> 349                                  1068.3062
#> 350                                  1077.7435
#> 351                                  1085.1129
#> 352                                  1088.4993
#> 353                                  1090.3991
#> 354                                  1089.4351
#> 355                                  1097.3246
#> 356                                  1100.3057
#> 357                                  1094.4379
#> 358                                  1093.6596
#> 359                                  1084.0651
#> 360                                  1083.0888
#> 361                                  1070.1272
#> 362                                  1076.6972
#> 363                                  1073.5195
#> 364                                  1071.3696
#> 365                                  1070.5230
#> 366                                  1073.5841
#> 367                                  1076.9758
#> 368                                  1079.8155
#> 369                                  1076.9528
#> 370                                  1070.1364
#> 371                                  1068.1927
#> 372                                  1069.5823
#> 373                                  1065.4669
#> 374                                  1065.1227
#> 375                                  1059.2844
#> 376                                  1059.2428
#> 377                                  1059.8871
#> 378                                  1059.1577
#> 379                                  1058.3382
#> 380                                  1067.1560
#> 381                                  1070.9689
#> 382                                  1076.5260
#> 383                                  1071.9100
#> 384                                  1072.7317
#> 385                                  1078.5058
#> 386                                  1078.2232
#> 387                                  1067.5764
#> 388                                  1069.3007
#> 389                                  1059.7759
#> 390                                  1055.7200
#> 391                                  1062.3401
#> 392                                  1065.4182
#> 393                                  1070.8766
#> 394                                  1072.4096
#> 395                                  1071.8588
#> 396                                  1067.2372
#> 397                                  1075.2018
#> 398                                  1075.4269
#> 399                                  1071.8512
#> 400                                  1076.1773
#> 401                                  1081.5495
#> 402                                  1091.0278
#> 403                                  1088.0128
#> 404                                  1086.0585
#> 405                                  1083.9774
#> 406                                  1082.0991
#> 407                                  1080.2659
#> 408                                  1078.7876
#> 409                                  1085.9967
#> 410                                  1082.5090
#> 411                                  1080.5681
#> 412                                  1083.8308
#> 413                                  1089.4547
#> 414                                  1085.5941
#> 415                                  1083.0537
#> 416                                  1085.6718
#> 417                                  1090.7606
#> 418                                  1089.5047
#> 419                                  1082.3548
#> 420                                  1090.9004
#> 421                                  1098.0757
#> 422                                  1094.5239
#> 423                                  1094.1985
#> 424                                  1085.4012
#> 425                                  1088.2498
#> 426                                  1096.3115
#> 427                                  1088.1251
#> 428                                  1084.2273
#> 429                                  1081.0214
#> 430                                  1077.6157
#> 431                                  1067.4493
#> 432                                  1069.9541
#> 433                                  1062.2951
#> 434                                  1062.1702
#> 435                                  1065.1351
#> 436                                  1064.1441
#> 437                                  1068.6041
#> 438                                  1068.4756
#> 439                                  1065.2373
#> 440                                  1068.4691
#> 441                                  1066.2999
#> 442                                  1075.1630
#> 443                                  1075.0717
#> 444                                  1079.3357
#> 445                                  1080.3616
#> 446                                  1065.3213
#> 447                                  1058.4907
#> 448                                  1056.3702
#> 449                                  1057.5543
#> 450                                  1045.8406
#> 451                                  1050.6491
#> 452                                  1047.6270
#> 453                                  1043.8626
#> 454                                  1036.0846
#> 455                                  1028.8151
#> 456                                  1029.0967
#> 457                                  1031.6436
#> 458                                  1021.1542
#> 459                                  1016.1324
#> 460                                  1018.8112
#> 461                                  1016.5460
#> 462                                  1027.3729
#> 463                                  1033.6016
#> 464                                  1036.5791
#> 465                                  1036.6035
#> 466                                  1038.0003
#> 467                                  1034.4708
#> 468                                  1037.6109
#> 469                                  1045.0120
#> 470                                  1050.4291
#> 471                                  1046.3629
#> 472                                  1038.2685
#> 473                                  1037.7202
#> 474                                  1039.9247
#> 475                                  1046.6796
#> 476                                  1040.0866
#> 477                                  1041.9085
#> 478                                  1043.0760
#> 479                                  1049.0458
#> 480                                  1048.9062
#> 481                                  1047.1197
#> 482                                  1041.3857
#> 483                                  1038.7986
#> 484                                  1036.9880
#> 485                                  1048.7407
#> 486                                  1060.9734
#> 487                                  1060.1399
#> 488                                  1054.9215
#> 489                                  1045.0569
#> 490                                  1047.6302
#> 491                                  1042.1773
#> 492                                  1053.6006
#> 493                                  1049.1725
#> 494                                  1049.7281
#> 495                                  1068.7795
#> 496                                  1063.2349
#> 497                                  1064.7727
#> 498                                  1059.2383
#> 499                                  1060.9765
#> 500                                  1056.6102
#> 501                                  1056.9967
#> 502                                  1055.5124
#> 503                                  1049.5962
#> 504                                  1049.6526
#> 505                                  1054.6107
#> 506                                  1062.5805
#> 507                                  1055.7169
#> 508                                  1054.4689
#> 509                                  1060.2660
#> 510                                  1054.6949
#> 511                                  1042.0524
#> 512                                  1037.3729
#> 513                                  1032.5367
#> 514                                  1032.7741
#> 515                                  1030.7554
#> 516                                  1031.9129
#> 517                                  1029.8011
#> 518                                  1031.6716
#> 519                                  1029.8416
#> 520                                  1035.7921
#> 521                                  1032.1055
#> 522                                  1033.5588
#> 523                                  1029.1346
#> 524                                  1030.1746
#> 525                                  1029.9360
#> 526                                  1021.5133
#> 527                                  1020.7922
#> 528                                  1026.6933
#> 529                                  1030.1003
#> 530                                  1030.8165
#> 531                                  1024.8549
#> 532                                  1030.7011
#> 533                                  1031.0971
#> 534                                  1028.8382
#> 535                                  1037.0484
#> 536                                  1033.2004
#> 537                                  1034.7172
#> 538                                  1041.1259
#> 539                                  1044.1370
#> 540                                  1042.6019
#> 541                                  1040.5098
#> 542                                  1042.2855
#> 543                                  1044.8529
#> 544                                  1044.9459
#> 545                                  1051.5382
#> 546                                  1051.2090
#> 547                                  1047.7075
#> 548                                  1050.3942
#> 549                                  1039.3853
#> 550                                  1041.3451
#> 551                                  1043.8299
#> 552                                  1042.7056
#> 553                                  1037.1198
#> 554                                  1035.1449
#> 555                                  1042.8940
#> 556                                  1039.1764
#> 557                                  1027.5179
#> 558                                  1031.5791
#> 559                                  1029.0726
#> 560                                  1026.5181
#> 561                                  1020.4413
#> 562                                  1020.3285
#> 563                                  1023.8347
#> 564                                  1020.8973
#> 565                                  1017.8637
#> 566                                  1023.3469
#> 567                                  1022.1093
#> 568                                  1021.3098
#> 569                                  1018.1809
#> 570                                  1022.6831
#> 571                                  1017.7121
#> 572                                  1021.9584
#> 573                                  1025.9869
#> 574                                  1023.6489
#> 575                                  1027.8910
#> 576                                  1032.8248
#> 577                                  1035.7029
#> 578                                  1045.8271
#> 579                                  1036.0154
#> 580                                  1030.1908
#> 581                                  1023.3082
#> 582                                  1024.1466
#> 583                                  1032.0697
#> 584                                  1040.4592
#> 585                                  1042.9006
#> 586                                  1047.2940
#> 587                                  1046.5696
#> 588                                  1048.9145
#> 589                                  1050.7957
#> 590                                  1046.9905
#> 591                                  1045.5240
#> 592                                  1044.8498
#> 593                                  1051.8190
#> 594                                  1046.6341
#> 595                                  1036.0624
#> 596                                  1039.9038
#> 597                                  1035.8230
#> 598                                  1033.6424
#> 599                                  1038.1660
#> 600                                  1034.3505
#> 601                                  1032.6452
#> 602                                  1040.1573
#> 603                                  1042.7989
#> 604                                  1045.5098
#> 605                                  1044.8265
#> 606                                  1039.1428
#> 607                                  1031.6597
#> 608                                  1030.5427
#> 609                                  1040.5513
#> 610                                  1041.6598
#> 611                                  1042.4817
#> 612                                  1044.1448
#> 613                                  1042.2188
#> 614                                  1035.2250
#> 615                                  1048.6037
#> 616                                  1046.4853
#> 617                                  1044.9923
#> 618                                  1036.0306
#> 619                                  1034.7905
#> 620                                  1033.5540
#> 621                                  1032.2765
#> 622                                  1023.3418
#> 623                                  1032.2651
#> 624                                  1041.0830
#> 625                                  1044.5310
#> 626                                  1039.0273
#> 627                                  1042.5999
#> 628                                  1041.3675
#> 629                                  1039.7686
#> 630                                  1046.5818
#> 631                                  1040.4424
#> 632                                  1037.8863
#> 633                                  1034.2303
#> 634                                  1034.3291
#> 635                                  1026.4648
#> 636                                  1022.9481
#> 637                                  1026.5277
#> 638                                  1028.8538
#> 639                                  1023.9843
#> 640                                  1026.7804
#> 641                                  1014.6172
#> 642                                  1012.9148
#> 643                                  1016.4799
#> 644                                  1013.1848
#> 645                                  1013.0027
#> 646                                  1005.0363
#> 647                                  1009.2753
#> 648                                  1000.0233
#> 649                                   998.4051
#> 650                                   997.1288
#> 651                                   997.4334
#> 652                                   993.3160
#> 653                                  1002.4646
#> 654                                   995.3151
#> 655                                   996.5857
#> 656                                   981.8869
#> 657                                   981.8990
#> 658                                   984.4473
#> 659                                   979.0237
#> 660                                   982.5478
#> 661                                   984.2027
#> 662                                   989.0844
#> 663                                   984.8677
#> 664                                   980.0148
#> 665                                   971.1571
#> 666                                   969.5448
#> 667                                   962.8508
#> 668                                   966.2915
#> 669                                   966.6479
#> 670                                   977.5967
#> 671                                   971.8082
#> 672                                   977.7166
#> 673                                   975.0798
#> 674                                   967.7966
#> 675                                   970.6615
#> 676                                   963.4946
#> 677                                   958.2186
#> 678                                   954.5531
#> 679                                   955.6076
#> 680                                   950.6130
#> 681                                   956.0023
#> 682                                   950.0074
#> 683                                   951.0906
#> 684                                   951.8060
#> 685                                   946.4773
#> 686                                   944.3341
#> 687                                   941.0533
#> 688                                   945.8502
#> 689                                   953.6305
#> 690                                   948.4265
#> 691                                   953.0794
#> 692                                   952.7021
#> 693                                   942.8662
#> 694                                   939.0866
#> 695                                   941.3924
#> 696                                   942.1179
#> 697                                   929.9064
#> 698                                   932.8080
#> 699                                   936.0832
#> 700                                   934.5607
#> 701                                   931.0228
#> 702                                   940.8807
#> 703                                   940.4307
#> 704                                   940.3606
#> 705                                   934.7433
#> 706                                   928.0227
#> 707                                   920.4069
#> 708                                   918.2971
#> 709                                   925.1017
#> 710                                   933.8707
#> 711                                   941.7125
#> 712                                   948.1963
#> 713                                   947.0083
#> 714                                   940.8875
#> 715                                   939.2485
#> 716                                   927.1862
#> 717                                   925.6172
#> 718                                   933.9166
#> 719                                   934.5714
#> 720                                   940.0508
#> 721                                   942.4976
#> 722                                   938.6030
#> 723                                   947.3208
#> 724                                   946.9289
#> 725                                   942.0511
#> 726                                   942.4044
#> 727                                   934.8114
#> 728                                   939.1303
#> 729                                   941.6381
#> 730                                   939.8642
#> 731                                   937.4221
#> 732                                   942.1035
#> 733                                   936.7915
#> 734                                   931.8724
#> 735                                   933.9936
#> 736                                   931.7371
#> 737                                   936.3625
#> 738                                   935.3694
#> 739                                   941.3436
#> 740                                   943.8214
#> 741                                   932.5956
#> 742                                   925.9188
#> 743                                   932.3326
#> 744                                   935.7866
#> 745                                   930.9513
#> 746                                   924.2223
#> 747                                   929.3907
#> 748                                   925.3318
#> 749                                   934.3404
#> 750                                   943.1981
#> 751                                   935.9247
#> 752                                   931.6964
#> 753                                   925.4440
#> 754                                   928.7804
#> 755                                   922.3266
#> 756                                   912.1516
#> 757                                   922.2583
#> 758                                   927.2882
#> 759                                   931.3738
#> 760                                   928.0538
#> 761                                   927.9974
#> 762                                   931.0958
#> 763                                   924.6896
#> 764                                   924.0683
#> 765                                   924.9470
#> 766                                   933.4109
#> 767                                   936.6216
#> 768                                   943.0327
#> 769                                   943.7355
#> 770                                   938.1729
#> 771                                   936.4746
#> 772                                   928.1507
#> 773                                   932.7950
#> 774                                   939.8791
#> 775                                   939.5655
#> 776                                   934.6610
#> 777                                   940.0968
#> 778                                   940.7934
#> 779                                   938.8620
#> 780                                   944.4800
#> 781                                   940.6807
#> 782                                   946.4255
#> 783                                   942.2132
#> 784                                   944.1702
#> 785                                   948.6271
#> 786                                   941.9508
#> 787                                   943.9414
#> 788                                   943.3835
#> 789                                   946.7622
#> 790                                   942.8192
#> 791                                   942.3843
#> 792                                   949.2957
#> 793                                   950.1382
#> 794                                   954.2541
#> 795                                   953.1496
#> 796                                   948.0027
#> 797                                   947.9481
#> 798                                   941.8231
#> 799                                   928.8425
#> 800                                   934.6882
#> 801                                   929.2536
#> 802                                   920.1232
#> 803                                   925.0996
#> 804                                   925.0403
#> 805                                   922.0422
#> 806                                   921.1524
#> 807                                   919.0225
#> 808                                   924.0058
#> 809                                   927.6441
#> 810                                   919.0110
#> 811                                   920.7779
#> 812                                   924.4120
#> 813                                   927.7533
#> 814                                   915.6317
#> 815                                   914.4549
#> 816                                   924.3531
#> 817                                   928.3371
#> 818                                   919.7907
#> 819                                   911.4724
#> 820                                   913.9279
#> 821                                   913.0576
#> 822                                   917.8641
#> 823                                   919.3332
#> 824                                   919.7382
#> 825                                   920.6565
#> 826                                   921.4878
#> 827                                   915.1398
#> 828                                   926.8873
#> 829                                   919.8272
#> 830                                   919.7424
#> 831                                   917.0208
#> 832                                   926.0214
#> 833                                   931.0786
#> 834                                   928.2600
#> 835                                   929.2871
#> 836                                   935.1144
#> 837                                   946.2960
#> 838                                   947.8074
#> 839                                   942.5948
#> 840                                   937.6771
#> 841                                   947.7057
#> 842                                   937.3529
#> 843                                   952.6316
#> 844                                   951.3248
#> 845                                   949.0529
#> 846                                   949.8407
#> 847                                   954.5076
#> 848                                   956.0217
#> 849                                   946.2410
#> 850                                   948.0087
#> 851                                   950.2608
#> 852                                   953.5586
#> 853                                   948.4014
#> 854                                   936.5463
#> 855                                   934.9235
#> 856                                   930.2020
#> 857                                   926.3725
#> 858                                   921.6036
#> 859                                   919.6136
#> 860                                   918.0575
#> 861                                   922.0380
#> 862                                   926.9701
#> 863                                   922.9975
#> 864                                   921.4534
#> 865                                   923.2606
#> 866                                   930.2545
#> 867                                   929.9742
#> 868                                   921.4798
#> 869                                   922.6391
#> 870                                   922.0436
#> 871                                   930.9061
#> 872                                   932.6232
#> 873                                   929.5080
#> 874                                   927.3103
#> 875                                   924.7839
#> 876                                   925.7140
#> 877                                   926.5961
#> 878                                   931.1754
#> 879                                   932.7763
#> 880                                   930.9428
#> 881                                   926.2398
#> 882                                   929.4133
#> 883                                   929.1008
#> 884                                   930.0150
#> 885                                   935.5332
#> 886                                   944.2934
#> 887                                   939.5243
#> 888                                   947.7447
#> 889                                   943.4111
#> 890                                   944.7428
#> 891                                   945.8547
#> 892                                   944.4701
#> 893                                   951.4414
#> 894                                   948.1468
#> 895                                   951.4495
#> 896                                   951.3832
#> 897                                   946.7258
#> 898                                   952.7993
#> 899                                   942.3576
#> 900                                   939.7268
#> 901                                   932.0198
#> 902                                   932.9914
#> 903                                   934.3135
#> 904                                   928.7198
#> 905                                   931.9746
#> 906                                   926.8101
#> 907                                   930.1061
#> 908                                   931.2952
#> 909                                   934.8716
#> 910                                   930.1792
#> 911                                   930.6560
#> 912                                   928.3419
#> 913                                   920.9975
#> 914                                   921.7609
#> 915                                   930.6297
#> 916                                   927.3894
#> 917                                   926.3903
#> 918                                   929.8365
#> 919                                   930.0172
#> 920                                   939.7349
#> 921                                   943.4210
#> 922                                   955.0276
#> 923                                   956.7722
#> 924                                   951.1026
#> 925                                   953.2093
#> 926                                   948.5865
#> 927                                   943.5512
#> 928                                   942.6038
#> 929                                   947.2734
#> 930                                   948.9929
#> 931                                   953.0630
#> 932                                   957.6397
#> 933                                   956.7805
#> 934                                   944.7693
#> 935                                   948.7489
#> 936                                   959.5944
#> 937                                   959.8864
#> 938                                   953.1118
#> 939                                   951.2740
#> 940                                   946.6014
#> 941                                   946.3933
#> 942                                   949.7738
#> 943                                   954.1060
#> 944                                   955.2819
#> 945                                   950.6120
#> 946                                   954.6783
#> 947                                   961.4199
#> 948                                   972.6793
#> 949                                   970.2110
#> 950                                   972.5815
#> 951                                   978.5499
#> 952                                   977.9680
#> 953                                   980.5923
#> 954                                   981.6644
#> 955                                   980.9921
#> 956                                   981.8347
#> 957                                   986.6583
#> 958                                   988.7022
#> 959                                   986.3700
#> 960                                   975.1711
#> 961                                   971.1958
#> 962                                   971.0960
#> 963                                   958.5239
#> 964                                   969.5787
#> 965                                   962.1349
#> 966                                   956.3311
#> 967                                   963.6198
#> 968                                   952.6704
#> 969                                   956.3657
#> 970                                   954.6457
#> 971                                   956.9251
#> 972                                   961.3156
#> 973                                   956.5233
#> 974                                   952.9941
#> 975                                   938.0093
#> 976                                   933.2041
#> 977                                   935.1050
#> 978                                   937.6303
#> 979                                   947.7656
#> 980                                   948.0887
#> 981                                   950.4070
#> 982                                   950.7809
#> 983                                   948.3468
#> 984                                   952.0913
#> 985                                   954.4125
#> 986                                   955.0596
#> 987                                   950.9823
#> 988                                   950.7815
#> 989                                   954.6886
#> 990                                   958.0709
#> 991                                   955.6576
#> 992                                   952.3120
#> 993                                   954.8761
#> 994                                   960.1206
#> 995                                   960.7259
#> 996                                   959.1594
#> 997                                   954.7561
#> 998                                   952.6596
#> 999                                   945.2459
#> 1000                                  941.7593
#>      pupil_raw_deblink_detransient_interpolate_lpfilt
#> 1                                            999.2793
#> 2                                            999.8841
#> 3                                           1000.4925
#> 4                                           1001.1043
#> 5                                           1001.7194
#> 6                                           1002.3377
#> 7                                           1002.9589
#> 8                                           1003.5829
#> 9                                           1004.2095
#> 10                                          1004.8385
#> 11                                          1005.4698
#> 12                                          1006.1032
#> 13                                          1006.7384
#> 14                                          1007.3754
#> 15                                          1008.0138
#> 16                                          1008.6536
#> 17                                          1009.2945
#> 18                                          1009.9364
#> 19                                          1010.5791
#> 20                                          1011.2223
#> 21                                          1011.8659
#> 22                                          1012.5096
#> 23                                          1013.1534
#> 24                                          1013.7969
#> 25                                          1014.4401
#> 26                                          1015.0826
#> 27                                          1015.7244
#> 28                                          1016.3651
#> 29                                          1017.0047
#> 30                                          1017.6429
#> 31                                          1018.2795
#> 32                                          1018.9143
#> 33                                          1019.5471
#> 34                                          1020.1777
#> 35                                          1020.8060
#> 36                                          1021.4317
#> 37                                          1022.0546
#> 38                                          1022.6746
#> 39                                          1023.2914
#> 40                                          1023.9049
#> 41                                          1024.5148
#> 42                                          1025.1210
#> 43                                          1025.7233
#> 44                                          1026.3215
#> 45                                          1026.9154
#> 46                                          1027.5048
#> 47                                          1028.0895
#> 48                                          1028.6695
#> 49                                          1029.2443
#> 50                                          1029.8140
#> 51                                          1030.3784
#> 52                                          1030.9372
#> 53                                          1031.4902
#> 54                                          1032.0374
#> 55                                          1032.5785
#> 56                                          1033.1134
#> 57                                          1033.6420
#> 58                                          1034.1640
#> 59                                          1034.6793
#> 60                                          1035.1877
#> 61                                          1035.6892
#> 62                                          1036.1835
#> 63                                          1036.6706
#> 64                                          1037.1502
#> 65                                          1037.6223
#> 66                                          1038.0867
#> 67                                          1038.5432
#> 68                                          1038.9919
#> 69                                          1039.4324
#> 70                                          1039.8648
#> 71                                          1040.2888
#> 72                                          1040.7045
#> 73                                          1041.1116
#> 74                                          1041.5101
#> 75                                          1041.8999
#> 76                                          1042.2808
#> 77                                          1042.6529
#> 78                                          1043.0159
#> 79                                          1043.3699
#> 80                                          1043.7147
#> 81                                          1044.0503
#> 82                                          1044.3765
#> 83                                          1044.6934
#> 84                                          1045.0009
#> 85                                          1045.2989
#> 86                                          1045.5874
#> 87                                          1045.8663
#> 88                                          1046.1356
#> 89                                          1046.3952
#> 90                                          1046.6451
#> 91                                          1046.8853
#> 92                                          1047.1158
#> 93                                          1047.3365
#> 94                                          1047.5474
#> 95                                          1047.7486
#> 96                                          1047.9399
#> 97                                          1048.1215
#> 98                                          1048.2933
#> 99                                          1048.4553
#> 100                                         1048.6076
#> 101                                         1048.7501
#> 102                                         1048.8829
#> 103                                         1049.0060
#> 104                                         1049.1195
#> 105                                         1049.2234
#> 106                                         1049.3177
#> 107                                         1049.4025
#> 108                                         1049.4779
#> 109                                         1049.5438
#> 110                                         1049.6004
#> 111                                         1049.6478
#> 112                                         1049.6859
#> 113                                         1049.7149
#> 114                                         1049.7349
#> 115                                         1049.7459
#> 116                                         1049.7480
#> 117                                         1049.7414
#> 118                                         1049.7261
#> 119                                         1049.7022
#> 120                                         1049.6699
#> 121                                         1049.6292
#> 122                                         1049.5802
#> 123                                         1049.5232
#> 124                                         1049.4582
#> 125                                         1049.3853
#> 126                                         1049.3046
#> 127                                         1049.2164
#> 128                                         1049.1207
#> 129                                         1049.0177
#> 130                                         1048.9076
#> 131                                         1048.7904
#> 132                                         1048.6663
#> 133                                         1048.5356
#> 134                                         1048.3983
#> 135                                         1048.2546
#> 136                                         1048.1047
#> 137                                         1047.9487
#> 138                                         1047.7869
#> 139                                         1047.6194
#> 140                                         1047.4464
#> 141                                         1047.2680
#> 142                                         1047.0845
#> 143                                         1046.8959
#> 144                                         1046.7027
#> 145                                         1046.5048
#> 146                                         1046.3025
#> 147                                         1046.0960
#> 148                                         1045.8855
#> 149                                         1045.6712
#> 150                                         1045.4533
#> 151                                         1045.2320
#> 152                                         1045.0075
#> 153                                         1044.7800
#> 154                                         1044.5497
#> 155                                         1044.3168
#> 156                                         1044.0815
#> 157                                         1043.8441
#> 158                                         1043.6047
#> 159                                         1043.3636
#> 160                                         1043.1209
#> 161                                         1042.8770
#> 162                                         1042.6319
#> 163                                         1042.3859
#> 164                                         1042.1392
#> 165                                         1041.8921
#> 166                                         1041.6447
#> 167                                         1041.3973
#> 168                                         1041.1501
#> 169                                         1040.9032
#> 170                                         1040.6570
#> 171                                         1040.4115
#> 172                                         1040.1671
#> 173                                         1039.9239
#> 174                                         1039.6821
#> 175                                         1039.4420
#> 176                                         1039.2038
#> 177                                         1038.9676
#> 178                                         1038.7336
#> 179                                         1038.5022
#> 180                                         1038.2734
#> 181                                         1038.0474
#> 182                                         1037.8246
#> 183                                         1037.6050
#> 184                                         1037.3888
#> 185                                         1037.1763
#> 186                                         1036.9676
#> 187                                         1036.7630
#> 188                                         1036.5625
#> 189                                         1036.3664
#> 190                                         1036.1749
#> 191                                         1035.9882
#> 192                                         1035.8063
#> 193                                         1035.6296
#> 194                                         1035.4581
#> 195                                         1035.2920
#> 196                                         1035.1315
#> 197                                         1034.9767
#> 198                                         1034.8278
#> 199                                         1034.6850
#> 200                                         1034.5484
#> 201                                         1034.4180
#> 202                                         1034.2942
#> 203                                         1034.1770
#> 204                                         1034.0665
#> 205                                         1033.9629
#> 206                                         1033.8663
#> 207                                         1033.7768
#> 208                                         1033.6945
#> 209                                         1033.6196
#> 210                                         1033.5521
#> 211                                         1033.4922
#> 212                                         1033.4400
#> 213                                         1033.3955
#> 214                                         1033.3588
#> 215                                         1033.3301
#> 216                                         1033.3094
#> 217                                         1033.2968
#> 218                                         1033.2923
#> 219                                         1033.2961
#> 220                                         1033.3081
#> 221                                         1033.3285
#> 222                                         1033.3573
#> 223                                         1033.3945
#> 224                                         1033.4403
#> 225                                         1033.4945
#> 226                                         1033.5573
#> 227                                         1033.6287
#> 228                                         1033.7088
#> 229                                         1033.7974
#> 230                                         1033.8947
#> 231                                         1034.0007
#> 232                                         1034.1153
#> 233                                         1034.2386
#> 234                                         1034.3705
#> 235                                         1034.5111
#> 236                                         1034.6603
#> 237                                         1034.8181
#> 238                                         1034.9845
#> 239                                         1035.1594
#> 240                                         1035.3429
#> 241                                         1035.5348
#> 242                                         1035.7351
#> 243                                         1035.9438
#> 244                                         1036.1608
#> 245                                         1036.3861
#> 246                                         1036.6196
#> 247                                         1036.8612
#> 248                                         1037.1108
#> 249                                         1037.3684
#> 250                                         1037.6339
#> 251                                         1037.9072
#> 252                                         1038.1882
#> 253                                         1038.4768
#> 254                                         1038.7729
#> 255                                         1039.0764
#> 256                                         1039.3872
#> 257                                         1039.7051
#> 258                                         1040.0301
#> 259                                         1040.3621
#> 260                                         1040.7008
#> 261                                         1041.0462
#> 262                                         1041.3981
#> 263                                         1041.7564
#> 264                                         1042.1210
#> 265                                         1042.4917
#> 266                                         1042.8684
#> 267                                         1043.2508
#> 268                                         1043.6389
#> 269                                         1044.0325
#> 270                                         1044.4313
#> 271                                         1044.8354
#> 272                                         1045.2444
#> 273                                         1045.6583
#> 274                                         1046.0767
#> 275                                         1046.4996
#> 276                                         1046.9268
#> 277                                         1047.3581
#> 278                                         1047.7933
#> 279                                         1048.2322
#> 280                                         1048.6746
#> 281                                         1049.1204
#> 282                                         1049.5692
#> 283                                         1050.0210
#> 284                                         1050.4756
#> 285                                         1050.9327
#> 286                                         1051.3921
#> 287                                         1051.8536
#> 288                                         1052.3171
#> 289                                         1052.7822
#> 290                                         1053.2489
#> 291                                         1053.7169
#> 292                                         1054.1859
#> 293                                         1054.6558
#> 294                                         1055.1264
#> 295                                         1055.5974
#> 296                                         1056.0687
#> 297                                         1056.5400
#> 298                                         1057.0111
#> 299                                         1057.4817
#> 300                                         1057.9518
#> 301                                         1058.4210
#> 302                                         1058.8892
#> 303                                         1059.3561
#> 304                                         1059.8216
#> 305                                         1060.2854
#> 306                                         1060.7472
#> 307                                         1061.2070
#> 308                                         1061.6644
#> 309                                         1062.1194
#> 310                                         1062.5715
#> 311                                         1063.0208
#> 312                                         1063.4669
#> 313                                         1063.9097
#> 314                                         1064.3489
#> 315                                         1064.7843
#> 316                                         1065.2159
#> 317                                         1065.6432
#> 318                                         1066.0663
#> 319                                         1066.4848
#> 320                                         1066.8986
#> 321                                         1067.3075
#> 322                                         1067.7113
#> 323                                         1068.1099
#> 324                                         1068.5030
#> 325                                         1068.8905
#> 326                                         1069.2722
#> 327                                         1069.6479
#> 328                                         1070.0175
#> 329                                         1070.3808
#> 330                                         1070.7376
#> 331                                         1071.0878
#> 332                                         1071.4313
#> 333                                         1071.7678
#> 334                                         1072.0973
#> 335                                         1072.4196
#> 336                                         1072.7345
#> 337                                         1073.0420
#> 338                                         1073.3418
#> 339                                         1073.6339
#> 340                                         1073.9181
#> 341                                         1074.1944
#> 342                                         1074.4626
#> 343                                         1074.7225
#> 344                                         1074.9742
#> 345                                         1075.2174
#> 346                                         1075.4522
#> 347                                         1075.6783
#> 348                                         1075.8958
#> 349                                         1076.1045
#> 350                                         1076.3043
#> 351                                         1076.4952
#> 352                                         1076.6771
#> 353                                         1076.8500
#> 354                                         1077.0137
#> 355                                         1077.1683
#> 356                                         1077.3137
#> 357                                         1077.4498
#> 358                                         1077.5766
#> 359                                         1077.6940
#> 360                                         1077.8021
#> 361                                         1077.9008
#> 362                                         1077.9901
#> 363                                         1078.0699
#> 364                                         1078.1403
#> 365                                         1078.2013
#> 366                                         1078.2528
#> 367                                         1078.2949
#> 368                                         1078.3275
#> 369                                         1078.3507
#> 370                                         1078.3646
#> 371                                         1078.3690
#> 372                                         1078.3641
#> 373                                         1078.3499
#> 374                                         1078.3264
#> 375                                         1078.2937
#> 376                                         1078.2518
#> 377                                         1078.2008
#> 378                                         1078.1407
#> 379                                         1078.0716
#> 380                                         1077.9936
#> 381                                         1077.9067
#> 382                                         1077.8110
#> 383                                         1077.7066
#> 384                                         1077.5935
#> 385                                         1077.4720
#> 386                                         1077.3419
#> 387                                         1077.2036
#> 388                                         1077.0570
#> 389                                         1076.9022
#> 390                                         1076.7394
#> 391                                         1076.5687
#> 392                                         1076.3902
#> 393                                         1076.2040
#> 394                                         1076.0102
#> 395                                         1075.8090
#> 396                                         1075.6006
#> 397                                         1075.3849
#> 398                                         1075.1623
#> 399                                         1074.9327
#> 400                                         1074.6965
#> 401                                         1074.4536
#> 402                                         1074.2043
#> 403                                         1073.9487
#> 404                                         1073.6870
#> 405                                         1073.4193
#> 406                                         1073.1458
#> 407                                         1072.8666
#> 408                                         1072.5820
#> 409                                         1072.2921
#> 410                                         1071.9970
#> 411                                         1071.6970
#> 412                                         1071.3921
#> 413                                         1071.0827
#> 414                                         1070.7687
#> 415                                         1070.4506
#> 416                                         1070.1283
#> 417                                         1069.8022
#> 418                                         1069.4723
#> 419                                         1069.1389
#> 420                                         1068.8021
#> 421                                         1068.4621
#> 422                                         1068.1192
#> 423                                         1067.7735
#> 424                                         1067.4251
#> 425                                         1067.0743
#> 426                                         1066.7213
#> 427                                         1066.3663
#> 428                                         1066.0093
#> 429                                         1065.6507
#> 430                                         1065.2906
#> 431                                         1064.9292
#> 432                                         1064.5667
#> 433                                         1064.2032
#> 434                                         1063.8389
#> 435                                         1063.4741
#> 436                                         1063.1089
#> 437                                         1062.7434
#> 438                                         1062.3780
#> 439                                         1062.0126
#> 440                                         1061.6476
#> 441                                         1061.2830
#> 442                                         1060.9192
#> 443                                         1060.5561
#> 444                                         1060.1940
#> 445                                         1059.8331
#> 446                                         1059.4735
#> 447                                         1059.1154
#> 448                                         1058.7589
#> 449                                         1058.4042
#> 450                                         1058.0515
#> 451                                         1057.7009
#> 452                                         1057.3525
#> 453                                         1057.0064
#> 454                                         1056.6629
#> 455                                         1056.3221
#> 456                                         1055.9841
#> 457                                         1055.6490
#> 458                                         1055.3169
#> 459                                         1054.9880
#> 460                                         1054.6624
#> 461                                         1054.3403
#> 462                                         1054.0216
#> 463                                         1053.7066
#> 464                                         1053.3953
#> 465                                         1053.0878
#> 466                                         1052.7843
#> 467                                         1052.4848
#> 468                                         1052.1894
#> 469                                         1051.8982
#> 470                                         1051.6112
#> 471                                         1051.3286
#> 472                                         1051.0504
#> 473                                         1050.7767
#> 474                                         1050.5075
#> 475                                         1050.2429
#> 476                                         1049.9830
#> 477                                         1049.7277
#> 478                                         1049.4772
#> 479                                         1049.2314
#> 480                                         1048.9905
#> 481                                         1048.7543
#> 482                                         1048.5230
#> 483                                         1048.2966
#> 484                                         1048.0751
#> 485                                         1047.8584
#> 486                                         1047.6466
#> 487                                         1047.4397
#> 488                                         1047.2377
#> 489                                         1047.0405
#> 490                                         1046.8482
#> 491                                         1046.6608
#> 492                                         1046.4781
#> 493                                         1046.3002
#> 494                                         1046.1270
#> 495                                         1045.9585
#> 496                                         1045.7946
#> 497                                         1045.6354
#> 498                                         1045.4807
#> 499                                         1045.3304
#> 500                                         1045.1846
#> 501                                         1045.0431
#> 502                                         1044.9059
#> 503                                         1044.7729
#> 504                                         1044.6439
#> 505                                         1044.5190
#> 506                                         1044.3980
#> 507                                         1044.2809
#> 508                                         1044.1675
#> 509                                         1044.0577
#> 510                                         1043.9514
#> 511                                         1043.8486
#> 512                                         1043.7490
#> 513                                         1043.6526
#> 514                                         1043.5592
#> 515                                         1043.4688
#> 516                                         1043.3811
#> 517                                         1043.2961
#> 518                                         1043.2136
#> 519                                         1043.1335
#> 520                                         1043.0556
#> 521                                         1042.9798
#> 522                                         1042.9060
#> 523                                         1042.8339
#> 524                                         1042.7634
#> 525                                         1042.6944
#> 526                                         1042.6266
#> 527                                         1042.5600
#> 528                                         1042.4944
#> 529                                         1042.4296
#> 530                                         1042.3654
#> 531                                         1042.3017
#> 532                                         1042.2382
#> 533                                         1042.1749
#> 534                                         1042.1115
#> 535                                         1042.0478
#> 536                                         1041.9837
#> 537                                         1041.9190
#> 538                                         1041.8535
#> 539                                         1041.7870
#> 540                                         1041.7193
#> 541                                         1041.6503
#> 542                                         1041.5797
#> 543                                         1041.5074
#> 544                                         1041.4332
#> 545                                         1041.3569
#> 546                                         1041.2783
#> 547                                         1041.1972
#> 548                                         1041.1135
#> 549                                         1041.0269
#> 550                                         1040.9372
#> 551                                         1040.8444
#> 552                                         1040.7481
#> 553                                         1040.6483
#> 554                                         1040.5446
#> 555                                         1040.4370
#> 556                                         1040.3253
#> 557                                         1040.2093
#> 558                                         1040.0887
#> 559                                         1039.9635
#> 560                                         1039.8334
#> 561                                         1039.6984
#> 562                                         1039.5581
#> 563                                         1039.4124
#> 564                                         1039.2613
#> 565                                         1039.1044
#> 566                                         1038.9418
#> 567                                         1038.7731
#> 568                                         1038.5982
#> 569                                         1038.4170
#> 570                                         1038.2294
#> 571                                         1038.0351
#> 572                                         1037.8341
#> 573                                         1037.6262
#> 574                                         1037.4112
#> 575                                         1037.1891
#> 576                                         1036.9597
#> 577                                         1036.7229
#> 578                                         1036.4785
#> 579                                         1036.2264
#> 580                                         1035.9666
#> 581                                         1035.6989
#> 582                                         1035.4232
#> 583                                         1035.1394
#> 584                                         1034.8474
#> 585                                         1034.5472
#> 586                                         1034.2385
#> 587                                         1033.9214
#> 588                                         1033.5958
#> 589                                         1033.2616
#> 590                                         1032.9186
#> 591                                         1032.5670
#> 592                                         1032.2065
#> 593                                         1031.8372
#> 594                                         1031.4589
#> 595                                         1031.0717
#> 596                                         1030.6755
#> 597                                         1030.2703
#> 598                                         1029.8560
#> 599                                         1029.4326
#> 600                                         1029.0001
#> 601                                         1028.5586
#> 602                                         1028.1078
#> 603                                         1027.6480
#> 604                                         1027.1790
#> 605                                         1026.7009
#> 606                                         1026.2136
#> 607                                         1025.7173
#> 608                                         1025.2119
#> 609                                         1024.6975
#> 610                                         1024.1741
#> 611                                         1023.6416
#> 612                                         1023.1003
#> 613                                         1022.5501
#> 614                                         1021.9910
#> 615                                         1021.4232
#> 616                                         1020.8467
#> 617                                         1020.2615
#> 618                                         1019.6678
#> 619                                         1019.0656
#> 620                                         1018.4550
#> 621                                         1017.8361
#> 622                                         1017.2090
#> 623                                         1016.5738
#> 624                                         1015.9306
#> 625                                         1015.2795
#> 626                                         1014.6206
#> 627                                         1013.9540
#> 628                                         1013.2799
#> 629                                         1012.5983
#> 630                                         1011.9095
#> 631                                         1011.2136
#> 632                                         1010.5106
#> 633                                         1009.8007
#> 634                                         1009.0842
#> 635                                         1008.3611
#> 636                                         1007.6315
#> 637                                         1006.8958
#> 638                                         1006.1539
#> 639                                         1005.4061
#> 640                                         1004.6526
#> 641                                         1003.8935
#> 642                                         1003.1290
#> 643                                         1002.3593
#> 644                                         1001.5846
#> 645                                         1000.8050
#> 646                                         1000.0207
#> 647                                          999.2320
#> 648                                          998.4391
#> 649                                          997.6420
#> 650                                          996.8411
#> 651                                          996.0366
#> 652                                          995.2285
#> 653                                          994.4172
#> 654                                          993.6029
#> 655                                          992.7857
#> 656                                          991.9659
#> 657                                          991.1437
#> 658                                          990.3192
#> 659                                          989.4928
#> 660                                          988.6646
#> 661                                          987.8349
#> 662                                          987.0039
#> 663                                          986.1717
#> 664                                          985.3386
#> 665                                          984.5049
#> 666                                          983.6707
#> 667                                          982.8363
#> 668                                          982.0019
#> 669                                          981.1677
#> 670                                          980.3339
#> 671                                          979.5008
#> 672                                          978.6685
#> 673                                          977.8374
#> 674                                          977.0076
#> 675                                          976.1793
#> 676                                          975.3527
#> 677                                          974.5282
#> 678                                          973.7058
#> 679                                          972.8858
#> 680                                          972.0684
#> 681                                          971.2538
#> 682                                          970.4423
#> 683                                          969.6340
#> 684                                          968.8291
#> 685                                          968.0279
#> 686                                          967.2306
#> 687                                          966.4372
#> 688                                          965.6482
#> 689                                          964.8636
#> 690                                          964.0836
#> 691                                          963.3084
#> 692                                          962.5382
#> 693                                          961.7732
#> 694                                          961.0136
#> 695                                          960.2596
#> 696                                          959.5113
#> 697                                          958.7688
#> 698                                          958.0325
#> 699                                          957.3024
#> 700                                          956.5786
#> 701                                          955.8614
#> 702                                          955.1510
#> 703                                          954.4474
#> 704                                          953.7507
#> 705                                          953.0613
#> 706                                          952.3791
#> 707                                          951.7044
#> 708                                          951.0372
#> 709                                          950.3776
#> 710                                          949.7259
#> 711                                          949.0822
#> 712                                          948.4464
#> 713                                          947.8188
#> 714                                          947.1995
#> 715                                          946.5885
#> 716                                          945.9860
#> 717                                          945.3921
#> 718                                          944.8068
#> 719                                          944.2302
#> 720                                          943.6624
#> 721                                          943.1036
#> 722                                          942.5537
#> 723                                          942.0128
#> 724                                          941.4809
#> 725                                          940.9583
#> 726                                          940.4448
#> 727                                          939.9406
#> 728                                          939.4457
#> 729                                          938.9601
#> 730                                          938.4839
#> 731                                          938.0171
#> 732                                          937.5597
#> 733                                          937.1118
#> 734                                          936.6734
#> 735                                          936.2444
#> 736                                          935.8249
#> 737                                          935.4150
#> 738                                          935.0145
#> 739                                          934.6235
#> 740                                          934.2420
#> 741                                          933.8700
#> 742                                          933.5075
#> 743                                          933.1544
#> 744                                          932.8107
#> 745                                          932.4764
#> 746                                          932.1515
#> 747                                          931.8359
#> 748                                          931.5295
#> 749                                          931.2325
#> 750                                          930.9446
#> 751                                          930.6658
#> 752                                          930.3961
#> 753                                          930.1355
#> 754                                          929.8838
#> 755                                          929.6410
#> 756                                          929.4071
#> 757                                          929.1819
#> 758                                          928.9654
#> 759                                          928.7576
#> 760                                          928.5582
#> 761                                          928.3673
#> 762                                          928.1848
#> 763                                          928.0105
#> 764                                          927.8444
#> 765                                          927.6864
#> 766                                          927.5364
#> 767                                          927.3942
#> 768                                          927.2599
#> 769                                          927.1332
#> 770                                          927.0141
#> 771                                          926.9024
#> 772                                          926.7982
#> 773                                          926.7011
#> 774                                          926.6112
#> 775                                          926.5283
#> 776                                          926.4523
#> 777                                          926.3831
#> 778                                          926.3205
#> 779                                          926.2644
#> 780                                          926.2148
#> 781                                          926.1714
#> 782                                          926.1343
#> 783                                          926.1031
#> 784                                          926.0778
#> 785                                          926.0583
#> 786                                          926.0445
#> 787                                          926.0362
#> 788                                          926.0332
#> 789                                          926.0355
#> 790                                          926.0430
#> 791                                          926.0554
#> 792                                          926.0727
#> 793                                          926.0947
#> 794                                          926.1213
#> 795                                          926.1524
#> 796                                          926.1878
#> 797                                          926.2274
#> 798                                          926.2711
#> 799                                          926.3187
#> 800                                          926.3701
#> 801                                          926.4251
#> 802                                          926.4838
#> 803                                          926.5458
#> 804                                          926.6112
#> 805                                          926.6797
#> 806                                          926.7512
#> 807                                          926.8256
#> 808                                          926.9028
#> 809                                          926.9827
#> 810                                          927.0651
#> 811                                          927.1500
#> 812                                          927.2371
#> 813                                          927.3264
#> 814                                          927.4177
#> 815                                          927.5110
#> 816                                          927.6062
#> 817                                          927.7030
#> 818                                          927.8015
#> 819                                          927.9014
#> 820                                          928.0027
#> 821                                          928.1053
#> 822                                          928.2091
#> 823                                          928.3140
#> 824                                          928.4198
#> 825                                          928.5265
#> 826                                          928.6339
#> 827                                          928.7421
#> 828                                          928.8508
#> 829                                          928.9600
#> 830                                          929.0696
#> 831                                          929.1795
#> 832                                          929.2897
#> 833                                          929.4000
#> 834                                          929.5103
#> 835                                          929.6207
#> 836                                          929.7309
#> 837                                          929.8410
#> 838                                          929.9508
#> 839                                          930.0603
#> 840                                          930.1695
#> 841                                          930.2782
#> 842                                          930.3863
#> 843                                          930.4939
#> 844                                          930.6009
#> 845                                          930.7072
#> 846                                          930.8127
#> 847                                          930.9174
#> 848                                          931.0213
#> 849                                          931.1242
#> 850                                          931.2262
#> 851                                          931.3272
#> 852                                          931.4272
#> 853                                          931.5261
#> 854                                          931.6238
#> 855                                          931.7205
#> 856                                          931.8159
#> 857                                          931.9101
#> 858                                          932.0030
#> 859                                          932.0946
#> 860                                          932.1850
#> 861                                          932.2740
#> 862                                          932.3616
#> 863                                          932.4479
#> 864                                          932.5328
#> 865                                          932.6162
#> 866                                          932.6982
#> 867                                          932.7788
#> 868                                          932.8579
#> 869                                          932.9355
#> 870                                          933.0117
#> 871                                          933.0863
#> 872                                          933.1595
#> 873                                          933.2312
#> 874                                          933.3013
#> 875                                          933.3700
#> 876                                          933.4372
#> 877                                          933.5028
#> 878                                          933.5670
#> 879                                          933.6296
#> 880                                          933.6908
#> 881                                          933.7505
#> 882                                          933.8087
#> 883                                          933.8654
#> 884                                          933.9207
#> 885                                          933.9745
#> 886                                          934.0269
#> 887                                          934.0779
#> 888                                          934.1274
#> 889                                          934.1756
#> 890                                          934.2224
#> 891                                          934.2678
#> 892                                          934.3118
#> 893                                          934.3546
#> 894                                          934.3960
#> 895                                          934.4361
#> 896                                          934.4750
#> 897                                          934.5126
#> 898                                          934.5489
#> 899                                          934.5841
#> 900                                          934.6180
#> 901                                          934.6508
#> 902                                          934.6825
#> 903                                          934.7130
#> 904                                          934.7424
#> 905                                          934.7707
#> 906                                          934.7980
#> 907                                          934.8243
#> 908                                          934.8495
#> 909                                          934.8738
#> 910                                          934.8971
#> 911                                          934.9194
#> 912                                          934.9409
#> 913                                          934.9614
#> 914                                          934.9811
#> 915                                          935.0000
#> 916                                          935.0180
#> 917                                          935.0352
#> 918                                          935.0517
#> 919                                          935.0674
#> 920                                          935.0824
#> 921                                          935.0967
#> 922                                          935.1103
#> 923                                          935.1232
#> 924                                          935.1356
#> 925                                          935.1473
#> 926                                          935.1584
#> 927                                          935.1689
#> 928                                          935.1789
#> 929                                          935.1884
#> 930                                          935.1974
#> 931                                          935.2058
#> 932                                          935.2138
#> 933                                          935.2214
#> 934                                          935.2285
#> 935                                          935.2352
#> 936                                          935.2415
#> 937                                          935.2475
#> 938                                          935.2531
#> 939                                          935.2583
#> 940                                          935.2632
#> 941                                          935.2678
#> 942                                          935.2721
#> 943                                          935.2761
#> 944                                          935.2799
#> 945                                          935.2834
#> 946                                          935.2866
#> 947                                          935.2896
#> 948                                          935.2925
#> 949                                          935.2951
#> 950                                          935.2975
#> 951                                          935.2997
#> 952                                          935.3018
#> 953                                          935.3037
#> 954                                          935.3054
#> 955                                          935.3070
#> 956                                          935.3085
#> 957                                          935.3099
#> 958                                          935.3111
#> 959                                          935.3123
#> 960                                          935.3133
#> 961                                          935.3142
#> 962                                          935.3151
#> 963                                          935.3159
#> 964                                          935.3166
#> 965                                          935.3172
#> 966                                          935.3178
#> 967                                          935.3183
#> 968                                          935.3188
#> 969                                          935.3192
#> 970                                          935.3196
#> 971                                          935.3199
#> 972                                          935.3202
#> 973                                          935.3204
#> 974                                          935.3207
#> 975                                          935.3209
#> 976                                          935.3211
#> 977                                          935.3212
#> 978                                          935.3214
#> 979                                          935.3215
#> 980                                          935.3216
#> 981                                          935.3217
#> 982                                          935.3217
#> 983                                          935.3218
#> 984                                          935.3219
#> 985                                          935.3219
#> 986                                          935.3220
#> 987                                          935.3220
#> 988                                          935.3220
#> 989                                          935.3220
#> 990                                          935.3221
#> 991                                          935.3221
#> 992                                          935.3221
#> 993                                          935.3221
#> 994                                          935.3221
#> 995                                          935.3221
#> 996                                          935.3221
#> 997                                          935.3221
#> 998                                          935.3221
#> 999                                          935.3221
#> 1000                                         935.3221
#>      pupil_raw_deblink_detransient_interpolate_lpfilt_z
#> 1                                          -0.162638846
#> 2                                          -0.151291869
#> 3                                          -0.139876895
#> 4                                          -0.128397000
#> 5                                          -0.116855306
#> 6                                          -0.105254975
#> 7                                          -0.093599214
#> 8                                          -0.081891269
#> 9                                          -0.070134424
#> 10                                         -0.058332001
#> 11                                         -0.046487356
#> 12                                         -0.034603882
#> 13                                         -0.022685002
#> 14                                         -0.010734170
#> 15                                          0.001245130
#> 16                                          0.013249387
#> 17                                          0.025275065
#> 18                                          0.037318603
#> 19                                          0.049376419
#> 20                                          0.061444910
#> 21                                          0.073520454
#> 22                                          0.085599413
#> 23                                          0.097678134
#> 24                                          0.109752951
#> 25                                          0.121820186
#> 26                                          0.133876152
#> 27                                          0.145917155
#> 28                                          0.157939496
#> 29                                          0.169939471
#> 30                                          0.181913376
#> 31                                          0.193857505
#> 32                                          0.205768157
#> 33                                          0.217641632
#> 34                                          0.229474239
#> 35                                          0.241262293
#> 36                                          0.253002120
#> 37                                          0.264690059
#> 38                                          0.276322461
#> 39                                          0.287895693
#> 40                                          0.299406142
#> 41                                          0.310850212
#> 42                                          0.322224331
#> 43                                          0.333524950
#> 44                                          0.344748544
#> 45                                          0.355891618
#> 46                                          0.366950706
#> 47                                          0.377922372
#> 48                                          0.388803215
#> 49                                          0.399589867
#> 50                                          0.410279000
#> 51                                          0.420867322
#> 52                                          0.431351584
#> 53                                          0.441728578
#> 54                                          0.451995140
#> 55                                          0.462148154
#> 56                                          0.472184551
#> 57                                          0.482101311
#> 58                                          0.491895466
#> 59                                          0.501564100
#> 60                                          0.511104354
#> 61                                          0.520513423
#> 62                                          0.529788560
#> 63                                          0.538927080
#> 64                                          0.547926355
#> 65                                          0.556783824
#> 66                                          0.565496987
#> 67                                          0.574063410
#> 68                                          0.582480727
#> 69                                          0.590746640
#> 70                                          0.598858919
#> 71                                          0.606815408
#> 72                                          0.614614022
#> 73                                          0.622252747
#> 74                                          0.629729648
#> 75                                          0.637042863
#> 76                                          0.644190608
#> 77                                          0.651171176
#> 78                                          0.657982942
#> 79                                          0.664624358
#> 80                                          0.671093958
#> 81                                          0.677390359
#> 82                                          0.683512260
#> 83                                          0.689458444
#> 84                                          0.695227778
#> 85                                          0.700819214
#> 86                                          0.706231792
#> 87                                          0.711464635
#> 88                                          0.716516957
#> 89                                          0.721388056
#> 90                                          0.726077321
#> 91                                          0.730584228
#> 92                                          0.734908342
#> 93                                          0.739049318
#> 94                                          0.743006901
#> 95                                          0.746780925
#> 96                                          0.750371314
#> 97                                          0.753778082
#> 98                                          0.757001336
#> 99                                          0.760041269
#> 100                                         0.762898168
#> 101                                         0.765572408
#> 102                                         0.768064457
#> 103                                         0.770374869
#> 104                                         0.772504292
#> 105                                         0.774453460
#> 106                                         0.776223200
#> 107                                         0.777814424
#> 108                                         0.779228136
#> 109                                         0.780465426
#> 110                                         0.781527472
#> 111                                         0.782415540
#> 112                                         0.783130981
#> 113                                         0.783675233
#> 114                                         0.784049818
#> 115                                         0.784256343
#> 116                                         0.784296498
#> 117                                         0.784172057
#> 118                                         0.783884875
#> 119                                         0.783436887
#> 120                                         0.782830108
#> 121                                         0.782066634
#> 122                                         0.781148638
#> 123                                         0.780078367
#> 124                                         0.778858147
#> 125                                         0.777490377
#> 126                                         0.775977528
#> 127                                         0.774322145
#> 128                                         0.772526843
#> 129                                         0.770594304
#> 130                                         0.768527280
#> 131                                         0.766328588
#> 132                                         0.764001112
#> 133                                         0.761547796
#> 134                                         0.758971648
#> 135                                         0.756275737
#> 136                                         0.753463187
#> 137                                         0.750537183
#> 138                                         0.747500963
#> 139                                         0.744357819
#> 140                                         0.741111094
#> 141                                         0.737764183
#> 142                                         0.734320527
#> 143                                         0.730783614
#> 144                                         0.727156978
#> 145                                         0.723444193
#> 146                                         0.719648877
#> 147                                         0.715774684
#> 148                                         0.711825306
#> 149                                         0.707804471
#> 150                                         0.703715937
#> 151                                         0.699563496
#> 152                                         0.695350967
#> 153                                         0.691082196
#> 154                                         0.686761056
#> 155                                         0.682391439
#> 156                                         0.677977260
#> 157                                         0.673522453
#> 158                                         0.669030966
#> 159                                         0.664506765
#> 160                                         0.659953824
#> 161                                         0.655376130
#> 162                                         0.650777677
#> 163                                         0.646162466
#> 164                                         0.641534498
#> 165                                         0.636897780
#> 166                                         0.632256316
#> 167                                         0.627614106
#> 168                                         0.622975149
#> 169                                         0.618343432
#> 170                                         0.613722937
#> 171                                         0.609117632
#> 172                                         0.604531473
#> 173                                         0.599968399
#> 174                                         0.595432332
#> 175                                         0.590927174
#> 176                                         0.586456806
#> 177                                         0.582025082
#> 178                                         0.577635834
#> 179                                         0.573292862
#> 180                                         0.568999939
#> 181                                         0.564760803
#> 182                                         0.560579159
#> 183                                         0.556458675
#> 184                                         0.552402983
#> 185                                         0.548415672
#> 186                                         0.544500290
#> 187                                         0.540660341
#> 188                                         0.536899282
#> 189                                         0.533220524
#> 190                                         0.529627428
#> 191                                         0.526123301
#> 192                                         0.522711400
#> 193                                         0.519394924
#> 194                                         0.516177019
#> 195                                         0.513060769
#> 196                                         0.510049199
#> 197                                         0.507145272
#> 198                                         0.504351889
#> 199                                         0.501671885
#> 200                                         0.499108027
#> 201                                         0.496663016
#> 202                                         0.494339484
#> 203                                         0.492139990
#> 204                                         0.490067022
#> 205                                         0.488122994
#> 206                                         0.486310245
#> 207                                         0.484631038
#> 208                                         0.483087558
#> 209                                         0.481681913
#> 210                                         0.480416128
#> 211                                         0.479292149
#> 212                                         0.478311842
#> 213                                         0.477476985
#> 214                                         0.476789276
#> 215                                         0.476250328
#> 216                                         0.475861665
#> 217                                         0.475624727
#> 218                                         0.475540866
#> 219                                         0.475611345
#> 220                                         0.475837340
#> 221                                         0.476219934
#> 222                                         0.476760124
#> 223                                         0.477458812
#> 224                                         0.478316812
#> 225                                         0.479334845
#> 226                                         0.480513539
#> 227                                         0.481853432
#> 228                                         0.483354966
#> 229                                         0.485018493
#> 230                                         0.486844268
#> 231                                         0.488832457
#> 232                                         0.490983129
#> 233                                         0.493296260
#> 234                                         0.495771735
#> 235                                         0.498409341
#> 236                                         0.501208775
#> 237                                         0.504169639
#> 238                                         0.507291443
#> 239                                         0.510573603
#> 240                                         0.514015442
#> 241                                         0.517616192
#> 242                                         0.521374994
#> 243                                         0.525290894
#> 244                                         0.529362850
#> 245                                         0.533589729
#> 246                                         0.537970309
#> 247                                         0.542503276
#> 248                                         0.547187231
#> 249                                         0.552020685
#> 250                                         0.557002064
#> 251                                         0.562129706
#> 252                                         0.567401865
#> 253                                         0.572816710
#> 254                                         0.578372328
#> 255                                         0.584066723
#> 256                                         0.589897817
#> 257                                         0.595863454
#> 258                                         0.601961398
#> 259                                         0.608189335
#> 260                                         0.614544875
#> 261                                         0.621025554
#> 262                                         0.627628832
#> 263                                         0.634352099
#> 264                                         0.641192673
#> 265                                         0.648147803
#> 266                                         0.655214668
#> 267                                         0.662390385
#> 268                                         0.669672001
#> 269                                         0.677056503
#> 270                                         0.684540817
#> 271                                         0.692121806
#> 272                                         0.699796279
#> 273                                         0.707560985
#> 274                                         0.715412620
#> 275                                         0.723347827
#> 276                                         0.731363199
#> 277                                         0.739455279
#> 278                                         0.747620563
#> 279                                         0.755855501
#> 280                                         0.764156503
#> 281                                         0.772519934
#> 282                                         0.780942121
#> 283                                         0.789419356
#> 284                                         0.797947892
#> 285                                         0.806523954
#> 286                                         0.815143731
#> 287                                         0.823803386
#> 288                                         0.832499056
#> 289                                         0.841226851
#> 290                                         0.849982861
#> 291                                         0.858763155
#> 292                                         0.867563784
#> 293                                         0.876380783
#> 294                                         0.885210174
#> 295                                         0.894047968
#> 296                                         0.902890166
#> 297                                         0.911732763
#> 298                                         0.920571750
#> 299                                         0.929403113
#> 300                                         0.938222841
#> 301                                         0.947026924
#> 302                                         0.955811356
#> 303                                         0.964572138
#> 304                                         0.973305281
#> 305                                         0.982006806
#> 306                                         0.990672747
#> 307                                         0.999299155
#> 308                                         1.007882098
#> 309                                         1.016417665
#> 310                                         1.024901965
#> 311                                         1.033331133
#> 312                                         1.041701333
#> 313                                         1.050008753
#> 314                                         1.058249616
#> 315                                         1.066420175
#> 316                                         1.074516722
#> 317                                         1.082535582
#> 318                                         1.090473121
#> 319                                         1.098325748
#> 320                                         1.106089914
#> 321                                         1.113762115
#> 322                                         1.121338895
#> 323                                         1.128816846
#> 324                                         1.136192614
#> 325                                         1.143462896
#> 326                                         1.150624444
#> 327                                         1.157674067
#> 328                                         1.164608634
#> 329                                         1.171425071
#> 330                                         1.178120370
#> 331                                         1.184691584
#> 332                                         1.191135832
#> 333                                         1.197450300
#> 334                                         1.203632242
#> 335                                         1.209678985
#> 336                                         1.215587922
#> 337                                         1.221356525
#> 338                                         1.226982335
#> 339                                         1.232462973
#> 340                                         1.237796135
#> 341                                         1.242979595
#> 342                                         1.248011207
#> 343                                         1.252888905
#> 344                                         1.257610707
#> 345                                         1.262174712
#> 346                                         1.266579102
#> 347                                         1.270822145
#> 348                                         1.274902196
#> 349                                         1.278817695
#> 350                                         1.282567169
#> 351                                         1.286149235
#> 352                                         1.289562597
#> 353                                         1.292806050
#> 354                                         1.295878478
#> 355                                         1.298778856
#> 356                                         1.301506252
#> 357                                         1.304059823
#> 358                                         1.306438820
#> 359                                         1.308642584
#> 360                                         1.310670550
#> 361                                         1.312522247
#> 362                                         1.314197295
#> 363                                         1.315695408
#> 364                                         1.317016392
#> 365                                         1.318160148
#> 366                                         1.319126668
#> 367                                         1.319916037
#> 368                                         1.320528435
#> 369                                         1.320964132
#> 370                                         1.321223491
#> 371                                         1.321306966
#> 372                                         1.321215104
#> 373                                         1.320948540
#> 374                                         1.320508002
#> 375                                         1.319894307
#> 376                                         1.319108360
#> 377                                         1.318151155
#> 378                                         1.317023775
#> 379                                         1.315727388
#> 380                                         1.314263249
#> 381                                         1.312632696
#> 382                                         1.310837154
#> 383                                         1.308878130
#> 384                                         1.306757211
#> 385                                         1.304476068
#> 386                                         1.302036450
#> 387                                         1.299440183
#> 388                                         1.296689173
#> 389                                         1.293785401
#> 390                                         1.290730921
#> 391                                         1.287527861
#> 392                                         1.284178421
#> 393                                         1.280684871
#> 394                                         1.277049548
#> 395                                         1.273274858
#> 396                                         1.269363270
#> 397                                         1.265317320
#> 398                                         1.261139601
#> 399                                         1.256832771
#> 400                                         1.252399543
#> 401                                         1.247842688
#> 402                                         1.243165030
#> 403                                         1.238369448
#> 404                                         1.233458870
#> 405                                         1.228436272
#> 406                                         1.223304679
#> 407                                         1.218067159
#> 408                                         1.212726823
#> 409                                         1.207286823
#> 410                                         1.201750349
#> 411                                         1.196120626
#> 412                                         1.190400916
#> 413                                         1.184594510
#> 414                                         1.178704730
#> 415                                         1.172734925
#> 416                                         1.166688469
#> 417                                         1.160568761
#> 418                                         1.154379217
#> 419                                         1.148123274
#> 420                                         1.141804384
#> 421                                         1.135426012
#> 422                                         1.128991636
#> 423                                         1.122504743
#> 424                                         1.115968824
#> 425                                         1.109387377
#> 426                                         1.102763900
#> 427                                         1.096101893
#> 428                                         1.089404852
#> 429                                         1.082676266
#> 430                                         1.075919620
#> 431                                         1.069138386
#> 432                                         1.062336026
#> 433                                         1.055515987
#> 434                                         1.048681699
#> 435                                         1.041836571
#> 436                                         1.034983994
#> 437                                         1.028127333
#> 438                                         1.021269927
#> 439                                         1.014415088
#> 440                                         1.007566097
#> 441                                         1.000726200
#> 442                                         0.993898612
#> 443                                         0.987086508
#> 444                                         0.980293024
#> 445                                         0.973521256
#> 446                                         0.966774255
#> 447                                         0.960055027
#> 448                                         0.953366531
#> 449                                         0.946711675
#> 450                                         0.940093316
#> 451                                         0.933514258
#> 452                                         0.926977248
#> 453                                         0.920484978
#> 454                                         0.914040078
#> 455                                         0.907645119
#> 456                                         0.901302608
#> 457                                         0.895014988
#> 458                                         0.888784636
#> 459                                         0.882613861
#> 460                                         0.876504903
#> 461                                         0.870459930
#> 462                                         0.864481039
#> 463                                         0.858570251
#> 464                                         0.852729515
#> 465                                         0.846960699
#> 466                                         0.841265597
#> 467                                         0.835645923
#> 468                                         0.830103308
#> 469                                         0.824639305
#> 470                                         0.819255382
#> 471                                         0.813952924
#> 472                                         0.808733233
#> 473                                         0.803597522
#> 474                                         0.798546922
#> 475                                         0.793582473
#> 476                                         0.788705130
#> 477                                         0.783915757
#> 478                                         0.779215131
#> 479                                         0.774603937
#> 480                                         0.770082772
#> 481                                         0.765652141
#> 482                                         0.761312458
#> 483                                         0.757064045
#> 484                                         0.752907135
#> 485                                         0.748841866
#> 486                                         0.744868285
#> 487                                         0.740986349
#> 488                                         0.737195920
#> 489                                         0.733496770
#> 490                                         0.729888579
#> 491                                         0.726370934
#> 492                                         0.722943332
#> 493                                         0.719605179
#> 494                                         0.716355789
#> 495                                         0.713194387
#> 496                                         0.710120108
#> 497                                         0.707131998
#> 498                                         0.704229013
#> 499                                         0.701410024
#> 500                                         0.698673812
#> 501                                         0.696019073
#> 502                                         0.693444417
#> 503                                         0.690948371
#> 504                                         0.688529378
#> 505                                         0.686185797
#> 506                                         0.683915907
#> 507                                         0.681717906
#> 508                                         0.679589915
#> 509                                         0.677529974
#> 510                                         0.675536051
#> 511                                         0.673606036
#> 512                                         0.671737745
#> 513                                         0.669928924
#> 514                                         0.668177248
#> 515                                         0.666480322
#> 516                                         0.664835686
#> 517                                         0.663240812
#> 518                                         0.661693109
#> 519                                         0.660189926
#> 520                                         0.658728548
#> 521                                         0.657306206
#> 522                                         0.655920071
#> 523                                         0.654567262
#> 524                                         0.653244844
#> 525                                         0.651949832
#> 526                                         0.650679194
#> 527                                         0.649429851
#> 528                                         0.648198678
#> 529                                         0.646982512
#> 530                                         0.645778147
#> 531                                         0.644582341
#> 532                                         0.643391817
#> 533                                         0.642203265
#> 534                                         0.641013344
#> 535                                         0.639818685
#> 536                                         0.638615895
#> 537                                         0.637401554
#> 538                                         0.636172224
#> 539                                         0.634924448
#> 540                                         0.633654752
#> 541                                         0.632359648
#> 542                                         0.631035638
#> 543                                         0.629679215
#> 544                                         0.628286865
#> 545                                         0.626855072
#> 546                                         0.625380317
#> 547                                         0.623859083
#> 548                                         0.622287856
#> 549                                         0.620663130
#> 550                                         0.618981407
#> 551                                         0.617239199
#> 552                                         0.615433034
#> 553                                         0.613559456
#> 554                                         0.611615026
#> 555                                         0.609596329
#> 556                                         0.607499970
#> 557                                         0.605322585
#> 558                                         0.603060834
#> 559                                         0.600711411
#> 560                                         0.598271044
#> 561                                         0.595736493
#> 562                                         0.593104561
#> 563                                         0.590372089
#> 564                                         0.587535960
#> 565                                         0.584593104
#> 566                                         0.581540499
#> 567                                         0.578375169
#> 568                                         0.575094194
#> 569                                         0.571694706
#> 570                                         0.568173893
#> 571                                         0.564529002
#> 572                                         0.560757340
#> 573                                         0.556856275
#> 574                                         0.552823242
#> 575                                         0.548655739
#> 576                                         0.544351334
#> 577                                         0.539907665
#> 578                                         0.535322442
#> 579                                         0.530593446
#> 580                                         0.525718536
#> 581                                         0.520695647
#> 582                                         0.515522791
#> 583                                         0.510198062
#> 584                                         0.504719634
#> 585                                         0.499085764
#> 586                                         0.493294795
#> 587                                         0.487345154
#> 588                                         0.481235354
#> 589                                         0.474963998
#> 590                                         0.468529777
#> 591                                         0.461931474
#> 592                                         0.455167962
#> 593                                         0.448238205
#> 594                                         0.441141263
#> 595                                         0.433876289
#> 596                                         0.426442531
#> 597                                         0.418839330
#> 598                                         0.411066128
#> 599                                         0.403122461
#> 600                                         0.395007961
#> 601                                         0.386722362
#> 602                                         0.378265491
#> 603                                         0.369637278
#> 604                                         0.360837750
#> 605                                         0.351867032
#> 606                                         0.342725349
#> 607                                         0.333413026
#> 608                                         0.323930486
#> 609                                         0.314278250
#> 610                                         0.304456941
#> 611                                         0.294467278
#> 612                                         0.284310078
#> 613                                         0.273986260
#> 614                                         0.263496836
#> 615                                         0.252842918
#> 616                                         0.242025713
#> 617                                         0.231046525
#> 618                                         0.219906754
#> 619                                         0.208607892
#> 620                                         0.197151528
#> 621                                         0.185539341
#> 622                                         0.173773104
#> 623                                         0.161854682
#> 624                                         0.149786025
#> 625                                         0.137569179
#> 626                                         0.125206271
#> 627                                         0.112699519
#> 628                                         0.100051223
#> 629                                         0.087263770
#> 630                                         0.074339628
#> 631                                         0.061281344
#> 632                                         0.048091548
#> 633                                         0.034772945
#> 634                                         0.021328318
#> 635                                         0.007760525
#> 636                                        -0.005927505
#> 637                                        -0.019732771
#> 638                                        -0.033652200
#> 639                                        -0.047682656
#> 640                                        -0.061820933
#> 641                                        -0.076063765
#> 642                                        -0.090407821
#> 643                                        -0.104849712
#> 644                                        -0.119385992
#> 645                                        -0.134013157
#> 646                                        -0.148727650
#> 647                                        -0.163525865
#> 648                                        -0.178404144
#> 649                                        -0.193358784
#> 650                                        -0.208386034
#> 651                                        -0.223482105
#> 652                                        -0.238643164
#> 653                                        -0.253865343
#> 654                                        -0.269144735
#> 655                                        -0.284477402
#> 656                                        -0.299859375
#> 657                                        -0.315286656
#> 658                                        -0.330755221
#> 659                                        -0.346261023
#> 660                                        -0.361799991
#> 661                                        -0.377368039
#> 662                                        -0.392961061
#> 663                                        -0.408574940
#> 664                                        -0.424205544
#> 665                                        -0.439848736
#> 666                                        -0.455500368
#> 667                                        -0.471156290
#> 668                                        -0.486812352
#> 669                                        -0.502464400
#> 670                                        -0.518108286
#> 671                                        -0.533739868
#> 672                                        -0.549355010
#> 673                                        -0.564949587
#> 674                                        -0.580519486
#> 675                                        -0.596060611
#> 676                                        -0.611568880
#> 677                                        -0.627040233
#> 678                                        -0.642470632
#> 679                                        -0.657856062
#> 680                                        -0.673192534
#> 681                                        -0.688476090
#> 682                                        -0.703702800
#> 683                                        -0.718868770
#> 684                                        -0.733970139
#> 685                                        -0.749003084
#> 686                                        -0.763963822
#> 687                                        -0.778848610
#> 688                                        -0.793653750
#> 689                                        -0.808375589
#> 690                                        -0.823010521
#> 691                                        -0.837554989
#> 692                                        -0.852005488
#> 693                                        -0.866358565
#> 694                                        -0.880610822
#> 695                                        -0.894758919
#> 696                                        -0.908799572
#> 697                                        -0.922729557
#> 698                                        -0.936545713
#> 699                                        -0.950244940
#> 700                                        -0.963824204
#> 701                                        -0.977280535
#> 702                                        -0.990611032
#> 703                                        -1.003812861
#> 704                                        -1.016883260
#> 705                                        -1.029819535
#> 706                                        -1.042619068
#> 707                                        -1.055279312
#> 708                                        -1.067797794
#> 709                                        -1.080172118
#> 710                                        -1.092399964
#> 711                                        -1.104479089
#> 712                                        -1.116407330
#> 713                                        -1.128182600
#> 714                                        -1.139802894
#> 715                                        -1.151266287
#> 716                                        -1.162570934
#> 717                                        -1.173715074
#> 718                                        -1.184697026
#> 719                                        -1.195515193
#> 720                                        -1.206168059
#> 721                                        -1.216654193
#> 722                                        -1.226972248
#> 723                                        -1.237120961
#> 724                                        -1.247099150
#> 725                                        -1.256905720
#> 726                                        -1.266539661
#> 727                                        -1.276000044
#> 728                                        -1.285286026
#> 729                                        -1.294396849
#> 730                                        -1.303331835
#> 731                                        -1.312090395
#> 732                                        -1.320672019
#> 733                                        -1.329076282
#> 734                                        -1.337302841
#> 735                                        -1.345351435
#> 736                                        -1.353221885
#> 737                                        -1.360914093
#> 738                                        -1.368428042
#> 739                                        -1.375763795
#> 740                                        -1.382921492
#> 741                                        -1.389901355
#> 742                                        -1.396703681
#> 743                                        -1.403328844
#> 744                                        -1.409777297
#> 745                                        -1.416049564
#> 746                                        -1.422146245
#> 747                                        -1.428068014
#> 748                                        -1.433815615
#> 749                                        -1.439389865
#> 750                                        -1.444791650
#> 751                                        -1.450021923
#> 752                                        -1.455081708
#> 753                                        -1.459972092
#> 754                                        -1.464694229
#> 755                                        -1.469249336
#> 756                                        -1.473638692
#> 757                                        -1.477863638
#> 758                                        -1.481925573
#> 759                                        -1.485825956
#> 760                                        -1.489566301
#> 761                                        -1.493148178
#> 762                                        -1.496573213
#> 763                                        -1.499843079
#> 764                                        -1.502959505
#> 765                                        -1.505924266
#> 766                                        -1.508739186
#> 767                                        -1.511406134
#> 768                                        -1.513927024
#> 769                                        -1.516303812
#> 770                                        -1.518538497
#> 771                                        -1.520633114
#> 772                                        -1.522589740
#> 773                                        -1.524410484
#> 774                                        -1.526097491
#> 775                                        -1.527652941
#> 776                                        -1.529079041
#> 777                                        -1.530378031
#> 778                                        -1.531552175
#> 779                                        -1.532603766
#> 780                                        -1.533535120
#> 781                                        -1.534348576
#> 782                                        -1.535046492
#> 783                                        -1.535631247
#> 784                                        -1.536105238
#> 785                                        -1.536470875
#> 786                                        -1.536730585
#> 787                                        -1.536886806
#> 788                                        -1.536941986
#> 789                                        -1.536898584
#> 790                                        -1.536759066
#> 791                                        -1.536525901
#> 792                                        -1.536201567
#> 793                                        -1.535788542
#> 794                                        -1.535289304
#> 795                                        -1.534706332
#> 796                                        -1.534042104
#> 797                                        -1.533299093
#> 798                                        -1.532479768
#> 799                                        -1.531586589
#> 800                                        -1.530622012
#> 801                                        -1.529588480
#> 802                                        -1.528488427
#> 803                                        -1.527324276
#> 804                                        -1.526098434
#> 805                                        -1.524813295
#> 806                                        -1.523471236
#> 807                                        -1.522074616
#> 808                                        -1.520625778
#> 809                                        -1.519127042
#> 810                                        -1.517580709
#> 811                                        -1.515989057
#> 812                                        -1.514354341
#> 813                                        -1.512678793
#> 814                                        -1.510964616
#> 815                                        -1.509213991
#> 816                                        -1.507429067
#> 817                                        -1.505611970
#> 818                                        -1.503764791
#> 819                                        -1.501889595
#> 820                                        -1.499988415
#> 821                                        -1.498063252
#> 822                                        -1.496116073
#> 823                                        -1.494148814
#> 824                                        -1.492163375
#> 825                                        -1.490161623
#> 826                                        -1.488145389
#> 827                                        -1.486116468
#> 828                                        -1.484076618
#> 829                                        -1.482027561
#> 830                                        -1.479970980
#> 831                                        -1.477908523
#> 832                                        -1.475841797
#> 833                                        -1.473772370
#> 834                                        -1.471701773
#> 835                                        -1.469631497
#> 836                                        -1.467562993
#> 837                                        -1.465497671
#> 838                                        -1.463436903
#> 839                                        -1.461382020
#> 840                                        -1.459334312
#> 841                                        -1.457295030
#> 842                                        -1.455265383
#> 843                                        -1.453246539
#> 844                                        -1.451239629
#> 845                                        -1.449245739
#> 846                                        -1.447265918
#> 847                                        -1.445301172
#> 848                                        -1.443352469
#> 849                                        -1.441420737
#> 850                                        -1.439506862
#> 851                                        -1.437611692
#> 852                                        -1.435736036
#> 853                                        -1.433880663
#> 854                                        -1.432046303
#> 855                                        -1.430233650
#> 856                                        -1.428443357
#> 857                                        -1.426676041
#> 858                                        -1.424932281
#> 859                                        -1.423212620
#> 860                                        -1.421517563
#> 861                                        -1.419847582
#> 862                                        -1.418203111
#> 863                                        -1.416584551
#> 864                                        -1.414992267
#> 865                                        -1.413426592
#> 866                                        -1.411887824
#> 867                                        -1.410376231
#> 868                                        -1.408892046
#> 869                                        -1.407435474
#> 870                                        -1.406006687
#> 871                                        -1.404605828
#> 872                                        -1.403233011
#> 873                                        -1.401888320
#> 874                                        -1.400571814
#> 875                                        -1.399283521
#> 876                                        -1.398023445
#> 877                                        -1.396791565
#> 878                                        -1.395587833
#> 879                                        -1.394412178
#> 880                                        -1.393264505
#> 881                                        -1.392144697
#> 882                                        -1.391052615
#> 883                                        -1.389988098
#> 884                                        -1.388950965
#> 885                                        -1.387941017
#> 886                                        -1.386958033
#> 887                                        -1.386001776
#> 888                                        -1.385071993
#> 889                                        -1.384168412
#> 890                                        -1.383290746
#> 891                                        -1.382438694
#> 892                                        -1.381611940
#> 893                                        -1.380810155
#> 894                                        -1.380032997
#> 895                                        -1.379280112
#> 896                                        -1.378551137
#> 897                                        -1.377845695
#> 898                                        -1.377163402
#> 899                                        -1.376503864
#> 900                                        -1.375866679
#> 901                                        -1.375251439
#> 902                                        -1.374657726
#> 903                                        -1.374085119
#> 904                                        -1.373533189
#> 905                                        -1.373001505
#> 906                                        -1.372489629
#> 907                                        -1.371997122
#> 908                                        -1.371523541
#> 909                                        -1.371068440
#> 910                                        -1.370631373
#> 911                                        -1.370211892
#> 912                                        -1.369809549
#> 913                                        -1.369423894
#> 914                                        -1.369054482
#> 915                                        -1.368700864
#> 916                                        -1.368362596
#> 917                                        -1.368039235
#> 918                                        -1.367730341
#> 919                                        -1.367435476
#> 920                                        -1.367154206
#> 921                                        -1.366886102
#> 922                                        -1.366630738
#> 923                                        -1.366387692
#> 924                                        -1.366156549
#> 925                                        -1.365936898
#> 926                                        -1.365728333
#> 927                                        -1.365530456
#> 928                                        -1.365342874
#> 929                                        -1.365165200
#> 930                                        -1.364997056
#> 931                                        -1.364838067
#> 932                                        -1.364687870
#> 933                                        -1.364546107
#> 934                                        -1.364412427
#> 935                                        -1.364286488
#> 936                                        -1.364167955
#> 937                                        -1.364056503
#> 938                                        -1.363951813
#> 939                                        -1.363853575
#> 940                                        -1.363761488
#> 941                                        -1.363675258
#> 942                                        -1.363594601
#> 943                                        -1.363519241
#> 944                                        -1.363448910
#> 945                                        -1.363383349
#> 946                                        -1.363322307
#> 947                                        -1.363265543
#> 948                                        -1.363212822
#> 949                                        -1.363163920
#> 950                                        -1.363118619
#> 951                                        -1.363076712
#> 952                                        -1.363037996
#> 953                                        -1.363002281
#> 954                                        -1.362969381
#> 955                                        -1.362939121
#> 956                                        -1.362911330
#> 957                                        -1.362885849
#> 958                                        -1.362862523
#> 959                                        -1.362841206
#> 960                                        -1.362821758
#> 961                                        -1.362804048
#> 962                                        -1.362787948
#> 963                                        -1.362773342
#> 964                                        -1.362760115
#> 965                                        -1.362748163
#> 966                                        -1.362737384
#> 967                                        -1.362727684
#> 968                                        -1.362718975
#> 969                                        -1.362711174
#> 970                                        -1.362704202
#> 971                                        -1.362697987
#> 972                                        -1.362692461
#> 973                                        -1.362687561
#> 974                                        -1.362683228
#> 975                                        -1.362679407
#> 976                                        -1.362676048
#> 977                                        -1.362673104
#> 978                                        -1.362670532
#> 979                                        -1.362668293
#> 980                                        -1.362666351
#> 981                                        -1.362664673
#> 982                                        -1.362663227
#> 983                                        -1.362661988
#> 984                                        -1.362660930
#> 985                                        -1.362660031
#> 986                                        -1.362659271
#> 987                                        -1.362658630
#> 988                                        -1.362658094
#> 989                                        -1.362657647
#> 990                                        -1.362657278
#> 991                                        -1.362656974
#> 992                                        -1.362656725
#> 993                                        -1.362656523
#> 994                                        -1.362656360
#> 995                                        -1.362656230
#> 996                                        -1.362656127
#> 997                                        -1.362656046
#> 998                                        -1.362655983
#> 999                                        -1.362655935
#> 1000                                       -1.362655898
#> 
#> 
#> $events
#> $events$block_1
#> # A tibble: 2 × 4
#>    time text      block text_unique
#>   <dbl> <chr>     <dbl> <chr>      
#> 1   100 TRIALID 1     1 TRIALID 1  
#> 2   500 TRIALID 2     1 TRIALID 2  
#> 
#> 
#> $blinks
#> $blinks$block_1
#> [1] stime etime block
#> <0 rows> (or 0-length row.names)
#> 
#> 
#> $info
#> $info$sample.rate
#> [1] 1000
#> 
#> $info$mono
#> [1] TRUE
#> 
#> $info$left
#> [1] TRUE
#> 
#> $info$right
#> [1] FALSE
#> 
#> $info$pupil.dtype
#> [1] "area"
#> 
#> $info$version
#> [1] "my-tracker"
#> 
#> $info$model
#> [1] NA
#> 
#> $info$screen.x
#> [1] 1920
#> 
#> $info$screen.y
#> [1] 1080
#> 
#> 
#> $latest
#> $latest$block_1
#> [1] "pupil_raw_deblink_detransient_interpolate_lpfilt_z"
#> 
#> 
#> $binocular
#> [1] FALSE
#> 
#> $decimated.sample.rate
#> [1] NA
#> 
#> $params
#> $params$load_generic
#> $params$load_generic$call
#> eyeris::load_generic(pupil = samples, events = events, sample_rate = 1000, 
#>     screen_width = 1920, screen_height = 1080, tracker = "my-tracker")
#> 
#> $params$load_generic$parameters
#> $params$load_generic$parameters$sample_rate
#> [1] 1000
#> 
#> $params$load_generic$parameters$time_unit
#> [1] "ms"
#> 
#> $params$load_generic$parameters$block
#> [1] "auto"
#> 
#> $params$load_generic$parameters$eye
#> [1] "L"
#> 
#> $params$load_generic$parameters$pupil_type
#> [1] "area"
#> 
#> $params$load_generic$parameters$tracker
#> [1] "my-tracker"
#> 
#> 
#> 
#> $params$deblink
#> $params$deblink$call
#> eyeris::glassbox(file = eye, lpfilt = list(plot_freqz = FALSE))
#> 
#> $params$deblink$parameters
#> $params$deblink$parameters$extend
#> [1] 50
#> 
#> 
#> 
#> $params$detransient
#> $params$detransient$call
#> eyeris::glassbox(file = eye, lpfilt = list(plot_freqz = FALSE))
#> 
#> $params$detransient$parameters
#> $params$detransient$parameters$n
#> [1] 16
#> 
#> $params$detransient$parameters$mad_thresh
#> NULL
#> 
#> 
#> 
#> $params$interpolate
#> $params$interpolate$call
#> eyeris::glassbox(file = eye, lpfilt = list(plot_freqz = FALSE))
#> 
#> $params$interpolate$parameters
#> $params$interpolate$parameters$max_gap_ms
#> [1] 250
#> 
#> $params$interpolate$parameters$verbose
#> [1] TRUE
#> 
#> 
#> 
#> $params$lpfilt
#> $params$lpfilt$call
#> eyeris::glassbox(file = eye, lpfilt = list(plot_freqz = FALSE))
#> 
#> $params$lpfilt$parameters
#> $params$lpfilt$parameters$wp
#> [1] 4
#> 
#> $params$lpfilt$parameters$ws
#> [1] 8
#> 
#> $params$lpfilt$parameters$rp
#> [1] 1
#> 
#> $params$lpfilt$parameters$rs
#> [1] 35
#> 
#> $params$lpfilt$parameters$plot_freqz
#> [1] FALSE
#> 
#> 
#> 
#> $params$z
#> $params$z$call
#> eyeris::glassbox(file = eye, lpfilt = list(plot_freqz = FALSE))
#> 
#> $params$z$parameters
#> list()
#> 
#> 
#> 
#> $confounds
#> $confounds$unepoched_timeseries
#> $confounds$unepoched_timeseries$block_1
#> $confounds$unepoched_timeseries$block_1$pupil_raw
#>   sampling_rate_hz total_time_ms n_samples n_missing prop_missing n_invalid
#> 1             1000           999      1000         0            0         0
#>   prop_invalid n_gaps max_gap_n_samples max_gap_duration_ms min_gap_n_samples
#> 1            0      0                 0                   0                 0
#>   min_gap_duration_ms mean_gap_n_samples mean_gap_duration_ms screen_width
#> 1                   0                  0                    0         1920
#>   screen_height gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1          1080      108.1561      106.3182                          12.99303
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                           0.0191156        0.002        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $confounds$unepoched_timeseries$block_1$pupil_raw_deblink
#>   sampling_rate_hz total_time_ms n_samples n_missing prop_missing n_invalid
#> 1             1000           999      1000         0            0         0
#>   prop_invalid n_gaps max_gap_n_samples max_gap_duration_ms min_gap_n_samples
#> 1            0      0                 0                   0                 0
#>   min_gap_duration_ms mean_gap_n_samples mean_gap_duration_ms screen_width
#> 1                   0                  0                    0         1920
#>   screen_height gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1          1080      108.1561      106.3182                          12.99303
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                           0.0191156        0.002        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $confounds$unepoched_timeseries$block_1$pupil_raw_deblink_detransient
#>   sampling_rate_hz total_time_ms n_samples n_missing prop_missing n_invalid
#> 1             1000           999      1000         0            0         0
#>   prop_invalid n_gaps max_gap_n_samples max_gap_duration_ms min_gap_n_samples
#> 1            0      0                 0                   0                 0
#>   min_gap_duration_ms mean_gap_n_samples mean_gap_duration_ms screen_width
#> 1                   0                  0                    0         1920
#>   screen_height gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1          1080      108.1561      106.3182                          12.99303
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                           0.0191156        0.002        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $confounds$unepoched_timeseries$block_1$pupil_raw_deblink_detransient_interpolate
#>   sampling_rate_hz total_time_ms n_samples n_missing prop_missing n_invalid
#> 1             1000           999      1000         0            0         0
#>   prop_invalid n_gaps max_gap_n_samples max_gap_duration_ms min_gap_n_samples
#> 1            0      0                 0                   0                 0
#>   min_gap_duration_ms mean_gap_n_samples mean_gap_duration_ms screen_width
#> 1                   0                  0                    0         1920
#>   screen_height gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1          1080      108.1561      106.3182                          12.99303
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                           0.0191156        0.002        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $confounds$unepoched_timeseries$block_1$pupil_raw_deblink_detransient_interpolate_lpfilt
#>   sampling_rate_hz total_time_ms n_samples n_missing prop_missing n_invalid
#> 1             1000           999      1000         0            0         0
#>   prop_invalid n_gaps max_gap_n_samples max_gap_duration_ms min_gap_n_samples
#> 1            0      0                 0                   0                 0
#>   min_gap_duration_ms mean_gap_n_samples mean_gap_duration_ms screen_width
#> 1                   0                  0                    0         1920
#>   screen_height gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1          1080      108.1561      106.3182                          12.99303
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                           0.0191156        0.002        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $confounds$unepoched_timeseries$block_1$pupil_raw_deblink_detransient_interpolate_lpfilt_z
#>   sampling_rate_hz total_time_ms n_samples n_missing prop_missing n_invalid
#> 1             1000           999      1000         0            0         0
#>   prop_invalid n_gaps max_gap_n_samples max_gap_duration_ms min_gap_n_samples
#> 1            0      0                 0                   0                 0
#>   min_gap_duration_ms mean_gap_n_samples mean_gap_duration_ms screen_width
#> 1                   0                  0                    0         1920
#>   screen_height gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1          1080      108.1561      106.3182                          12.99303
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                           0.0191156        0.002        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> 
#> 
#> 
#> attr(,"class")
#> [1] "eyeris"
```
