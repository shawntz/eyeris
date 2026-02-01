# Extract confounding variables calculated separately for each pupil data file

Calculates various confounding variables for pupil data, including blink
statistics, gaze position metrics, and pupil size characteristics. These
confounds are calculated separately for each preprocessing step,
recording block, and epoched time series in the `eyeris` object.

## Usage

``` r
summarize_confounds(eyeris)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)

## Value

An `eyeris` object with a new nested list of data frames: `$confounds`
The confounds are organized hierarchically by block and preprocessing
step. Each step contains metrics such as:

- Blink rate and duration statistics

- Gaze position (x,y) mean and standard deviation

- Pupil size mean, standard deviation, and range

- Missing data percentage

## Examples

``` r
# load demo dataset
demo_data <- eyelink_asc_demo_dataset()

# calculate confounds for all blocks and preprocessing steps
confounds <- demo_data |>
  eyeris::glassbox() |>
  eyeris::epoch(
    events = "PROBE_{type}_{trial}",
    limits = c(-1, 1), # grab 1 second prior to and 1 second post event
    label = "prePostProbe" # custom epoch label name
  ) |>
  eyeris::summarize_confounds()
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-02-01 01:13:36] [INFO] Processing block: block_1
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::lpfilt() for block_1

#> ! [2026-02-01 01:13:36] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-02-01 01:13:36] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-02-01 01:13:36] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-02-01 01:13:36] [INFO] Block processing summary:
#> ℹ [2026-02-01 01:13:36] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-02-01 01:13:36] [OKAY] Running eyeris::summarize_confounds()
#> ℹ [2026-02-01 01:13:36] [INFO] Epoching pupil data...
#> ℹ [2026-02-01 01:13:36] [INFO] Block 1: found 10 matching events for
#> PROBEtypetrial
#> ✔ [2026-02-01 01:13:36] [OKAY] Done!
#> ✔ [2026-02-01 01:13:36] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-02-01 01:13:36] [OKAY] Pupil epoching completed in 0.12 seconds
#> ℹ [2026-02-01 01:13:36] [INFO] Recalculating epoched confounds for new
#> epochs...

# access confounds for entire time series for a specific block and step
confounds$confounds$unepoched_timeseries
#> $block_1
#> $block_1$pupil_raw
#>   sampling_rate_hz total_time_ms n_samples n_invalid prop_invalid n_gaps
#> 1             1000         20766     20767        74  0.003563346      1
#>   max_gap_n_samples max_gap_duration_ms min_gap_n_samples min_gap_duration_ms
#> 1                74                  74                74                  74
#>   mean_gap_n_samples mean_gap_duration_ms screen_width screen_height
#> 1                 74                   74         1920          1080
#>   gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1      197.3489      780.2944                          21.01572
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                          0.02866172   0.00014446        1    0.04815564
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    56                    56                     56
#>   total_blink_time_ms prop_blink_time
#> 1                  56     0.002696716
#> 
#> $block_1$pupil_raw_deblink
#>   sampling_rate_hz total_time_ms n_samples n_invalid prop_invalid n_gaps
#> 1             1000         20766     20767       156  0.007511918      1
#>   max_gap_n_samples max_gap_duration_ms min_gap_n_samples min_gap_duration_ms
#> 1               156                 156               156                 156
#>   mean_gap_n_samples mean_gap_duration_ms screen_width screen_height
#> 1                156                  156         1920          1080
#>   gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1      197.3489      780.2944                          21.01572
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                          0.02866172 9.630664e-05        1    0.04815564
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                   156                   156                    156
#>   total_blink_time_ms prop_blink_time
#> 1                 156      0.00751228
#> 
#> $block_1$pupil_raw_deblink_detransient
#>   sampling_rate_hz total_time_ms n_samples n_invalid prop_invalid n_gaps
#> 1             1000         20766     20767       156  0.007511918      1
#>   max_gap_n_samples max_gap_duration_ms min_gap_n_samples min_gap_duration_ms
#> 1               156                 156               156                 156
#>   mean_gap_n_samples mean_gap_duration_ms screen_width screen_height
#> 1                156                  156         1920          1080
#>   gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1      197.3489      780.2944                          21.01572
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                          0.02866172 9.630664e-05        1    0.04815564
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                   156                   156                    156
#>   total_blink_time_ms prop_blink_time
#> 1                 156      0.00751228
#> 
#> $block_1$pupil_raw_deblink_detransient_interpolate
#>   sampling_rate_hz total_time_ms n_samples n_invalid prop_invalid n_gaps
#> 1             1000         20766     20767        NA           NA      0
#>   max_gap_n_samples max_gap_duration_ms min_gap_n_samples min_gap_duration_ms
#> 1                 0                   0                 0                   0
#>   mean_gap_n_samples mean_gap_duration_ms screen_width screen_height
#> 1                  0                    0         1920          1080
#>   gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1      197.3489      780.2944                          21.01572
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                          0.02866172 9.630664e-05        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $block_1$pupil_raw_deblink_detransient_interpolate_lpfilt
#>   sampling_rate_hz total_time_ms n_samples n_invalid prop_invalid n_gaps
#> 1             1000         20766     20767        NA           NA      0
#>   max_gap_n_samples max_gap_duration_ms min_gap_n_samples min_gap_duration_ms
#> 1                 0                   0                 0                   0
#>   mean_gap_n_samples mean_gap_duration_ms screen_width screen_height
#> 1                  0                    0         1920          1080
#>   gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1      197.3489      780.2944                          21.01572
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                          0.02866172 9.630664e-05        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> $block_1$pupil_raw_deblink_detransient_interpolate_lpfilt_z
#>   sampling_rate_hz total_time_ms n_samples n_invalid prop_invalid n_gaps
#> 1             1000         20766     20767        NA           NA      0
#>   max_gap_n_samples max_gap_duration_ms min_gap_n_samples min_gap_duration_ms
#> 1                 0                   0                 0                   0
#>   mean_gap_n_samples mean_gap_duration_ms screen_width screen_height
#> 1                  0                    0         1920          1080
#>   gaze_x_var_px gaze_y_var_px mean_gaze_distance_from_center_px
#> 1      197.3489      780.2944                          21.01572
#>   mean_gaze_distance_from_center_norm prop_clipped n_blinks blink_rate_hz
#> 1                          0.02866172 9.630664e-05        0             0
#>   min_blink_duration_ms max_blink_duration_ms mean_blink_duration_ms
#> 1                    NA                    NA                     NA
#>   total_blink_time_ms prop_blink_time
#> 1                   0               0
#> 
#> 

# access confounds for a specific epoched time series
# for a specific block and step
confounds$confounds$epoched_timeseries
#> $epoch_prePostProbe
#> $epoch_prePostProbe$block_1
#>                                                                    matched_event
#> PROBE_START_22.pupil_raw                                          PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink                                  PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient                      PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate          PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_22
#> PROBE_STOP_22.pupil_raw                                            PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink                                    PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient                        PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_22
#> PROBE_START_23.pupil_raw                                          PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink                                  PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient                      PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate          PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_23
#> PROBE_STOP_23.pupil_raw                                            PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink                                    PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient                        PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_23
#> PROBE_START_24.pupil_raw                                          PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink                                  PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient                      PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate          PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_24
#> PROBE_STOP_24.pupil_raw                                            PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink                                    PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient                        PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_24
#> PROBE_START_25.pupil_raw                                          PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink                                  PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient                      PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate          PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_25
#> PROBE_STOP_25.pupil_raw                                            PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink                                    PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient                        PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_25
#> PROBE_START_26.pupil_raw                                          PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink                                  PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient                      PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate          PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_26
#> PROBE_STOP_26.pupil_raw                                            PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink                                    PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient                        PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_26
#>                                                                      text_unique
#> PROBE_START_22.pupil_raw                                          PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink                                  PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient                      PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate          PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_22
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_22
#> PROBE_STOP_22.pupil_raw                                            PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink                                    PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient                        PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_22
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_22
#> PROBE_START_23.pupil_raw                                          PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink                                  PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient                      PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate          PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_23
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_23
#> PROBE_STOP_23.pupil_raw                                            PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink                                    PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient                        PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_23
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_23
#> PROBE_START_24.pupil_raw                                          PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink                                  PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient                      PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate          PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_24
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_24
#> PROBE_STOP_24.pupil_raw                                            PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink                                    PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient                        PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_24
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_24
#> PROBE_START_25.pupil_raw                                          PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink                                  PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient                      PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate          PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_25
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_25
#> PROBE_STOP_25.pupil_raw                                            PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink                                    PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient                        PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_25
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_25
#> PROBE_START_26.pupil_raw                                          PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink                                  PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient                      PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate          PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt   PROBE_START_26
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z PROBE_START_26
#> PROBE_STOP_26.pupil_raw                                            PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink                                    PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient                        PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate            PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt     PROBE_STOP_26
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z   PROBE_STOP_26
#>                                                                                                           step
#> PROBE_START_22.pupil_raw                                                                                   raw
#> PROBE_START_22.pupil_raw_deblink                                                                   raw_deblink
#> PROBE_START_22.pupil_raw_deblink_detransient                                           raw_deblink_detransient
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate                   raw_deblink_detransient_interpolate
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt     raw_deblink_detransient_interpolate_lpfilt
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_STOP_22.pupil_raw                                                                                    raw
#> PROBE_STOP_22.pupil_raw_deblink                                                                    raw_deblink
#> PROBE_STOP_22.pupil_raw_deblink_detransient                                            raw_deblink_detransient
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate                    raw_deblink_detransient_interpolate
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt      raw_deblink_detransient_interpolate_lpfilt
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z  raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_START_23.pupil_raw                                                                                   raw
#> PROBE_START_23.pupil_raw_deblink                                                                   raw_deblink
#> PROBE_START_23.pupil_raw_deblink_detransient                                           raw_deblink_detransient
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate                   raw_deblink_detransient_interpolate
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt     raw_deblink_detransient_interpolate_lpfilt
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_STOP_23.pupil_raw                                                                                    raw
#> PROBE_STOP_23.pupil_raw_deblink                                                                    raw_deblink
#> PROBE_STOP_23.pupil_raw_deblink_detransient                                            raw_deblink_detransient
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate                    raw_deblink_detransient_interpolate
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt      raw_deblink_detransient_interpolate_lpfilt
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z  raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_START_24.pupil_raw                                                                                   raw
#> PROBE_START_24.pupil_raw_deblink                                                                   raw_deblink
#> PROBE_START_24.pupil_raw_deblink_detransient                                           raw_deblink_detransient
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate                   raw_deblink_detransient_interpolate
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt     raw_deblink_detransient_interpolate_lpfilt
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_STOP_24.pupil_raw                                                                                    raw
#> PROBE_STOP_24.pupil_raw_deblink                                                                    raw_deblink
#> PROBE_STOP_24.pupil_raw_deblink_detransient                                            raw_deblink_detransient
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate                    raw_deblink_detransient_interpolate
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt      raw_deblink_detransient_interpolate_lpfilt
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z  raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_START_25.pupil_raw                                                                                   raw
#> PROBE_START_25.pupil_raw_deblink                                                                   raw_deblink
#> PROBE_START_25.pupil_raw_deblink_detransient                                           raw_deblink_detransient
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate                   raw_deblink_detransient_interpolate
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt     raw_deblink_detransient_interpolate_lpfilt
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_STOP_25.pupil_raw                                                                                    raw
#> PROBE_STOP_25.pupil_raw_deblink                                                                    raw_deblink
#> PROBE_STOP_25.pupil_raw_deblink_detransient                                            raw_deblink_detransient
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate                    raw_deblink_detransient_interpolate
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt      raw_deblink_detransient_interpolate_lpfilt
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z  raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_START_26.pupil_raw                                                                                   raw
#> PROBE_START_26.pupil_raw_deblink                                                                   raw_deblink
#> PROBE_START_26.pupil_raw_deblink_detransient                                           raw_deblink_detransient
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate                   raw_deblink_detransient_interpolate
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt     raw_deblink_detransient_interpolate_lpfilt
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z raw_deblink_detransient_interpolate_lpfilt_z
#> PROBE_STOP_26.pupil_raw                                                                                    raw
#> PROBE_STOP_26.pupil_raw_deblink                                                                    raw_deblink
#> PROBE_STOP_26.pupil_raw_deblink_detransient                                            raw_deblink_detransient
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate                    raw_deblink_detransient_interpolate
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt      raw_deblink_detransient_interpolate_lpfilt
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z  raw_deblink_detransient_interpolate_lpfilt_z
#>                                                                         range
#> PROBE_START_22.pupil_raw                                           998.000000
#> PROBE_START_22.pupil_raw_deblink                                   998.000000
#> PROBE_START_22.pupil_raw_deblink_detransient                       998.000000
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate           998.000000
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt    971.130196
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z    2.140145
#> PROBE_STOP_22.pupil_raw                                           1051.000000
#> PROBE_STOP_22.pupil_raw_deblink                                   1051.000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient                       1051.000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate           1051.000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt    1010.411304
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z     2.226711
#> PROBE_START_23.pupil_raw                                          1260.000000
#> PROBE_START_23.pupil_raw_deblink                                  1260.000000
#> PROBE_START_23.pupil_raw_deblink_detransient                      1260.000000
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate          1260.000000
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt   1231.702862
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z    2.714386
#> PROBE_STOP_23.pupil_raw                                           1699.000000
#> PROBE_STOP_23.pupil_raw_deblink                                   1699.000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient                       1699.000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate           1699.000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt    1660.799078
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z     3.660014
#> PROBE_START_24.pupil_raw                                          1040.000000
#> PROBE_START_24.pupil_raw_deblink                                  1040.000000
#> PROBE_START_24.pupil_raw_deblink_detransient                      1040.000000
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate          1040.000000
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt    999.811746
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z    2.203352
#> PROBE_STOP_24.pupil_raw                                           1129.000000
#> PROBE_STOP_24.pupil_raw_deblink                                   1129.000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient                       1129.000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate           1129.000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt    1080.197345
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z     2.380503
#> PROBE_START_25.pupil_raw                                          2619.000000
#> PROBE_START_25.pupil_raw_deblink                                   797.000000
#> PROBE_START_25.pupil_raw_deblink_detransient                       797.000000
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate           797.000000
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt    737.992888
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z    1.626364
#> PROBE_STOP_25.pupil_raw                                            680.000000
#> PROBE_STOP_25.pupil_raw_deblink                                    680.000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient                        680.000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate            680.000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt     624.752894
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z     1.376810
#> PROBE_START_26.pupil_raw                                          1088.000000
#> PROBE_START_26.pupil_raw_deblink                                  1088.000000
#> PROBE_START_26.pupil_raw_deblink_detransient                      1088.000000
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate          1088.000000
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt   1036.831886
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z    2.284936
#> PROBE_STOP_26.pupil_raw                                           1136.000000
#> PROBE_STOP_26.pupil_raw_deblink                                   1136.000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient                       1136.000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate           1136.000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt    1093.494898
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z     2.409808
#>                                                                   zscore_max
#> PROBE_START_22.pupil_raw                                           1.8741331
#> PROBE_START_22.pupil_raw_deblink                                   1.8741331
#> PROBE_START_22.pupil_raw_deblink_detransient                       1.8741331
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate           1.8741331
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt    1.8252086
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z  1.8252086
#> PROBE_STOP_22.pupil_raw                                            1.7388454
#> PROBE_STOP_22.pupil_raw_deblink                                    1.7388454
#> PROBE_STOP_22.pupil_raw_deblink_detransient                        1.7388454
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate            1.7388454
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt     1.7002157
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z   1.7002157
#> PROBE_START_23.pupil_raw                                           1.7529229
#> PROBE_START_23.pupil_raw_deblink                                   1.7529229
#> PROBE_START_23.pupil_raw_deblink_detransient                       1.7529229
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate           1.7529229
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt    1.6937001
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z  1.6937001
#> PROBE_STOP_23.pupil_raw                                            1.4647589
#> PROBE_STOP_23.pupil_raw_deblink                                    1.4647589
#> PROBE_STOP_23.pupil_raw_deblink_detransient                        1.4647589
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate            1.4647589
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt     1.4340379
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z   1.4340379
#> PROBE_START_24.pupil_raw                                           1.8640236
#> PROBE_START_24.pupil_raw_deblink                                   1.8640236
#> PROBE_START_24.pupil_raw_deblink_detransient                       1.8640236
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate           1.8640236
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt    1.8174791
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z  1.8174791
#> PROBE_STOP_24.pupil_raw                                            1.7923244
#> PROBE_STOP_24.pupil_raw_deblink                                    1.7923244
#> PROBE_STOP_24.pupil_raw_deblink_detransient                        1.7923244
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate            1.7923244
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt     1.7495699
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z   1.7495699
#> PROBE_START_25.pupil_raw                                           1.3800539
#> PROBE_START_25.pupil_raw_deblink                                   1.6486043
#> PROBE_START_25.pupil_raw_deblink_detransient                       1.6486043
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate           1.4772399
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt    1.3809607
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z  1.3809607
#> PROBE_STOP_25.pupil_raw                                            2.0509766
#> PROBE_STOP_25.pupil_raw_deblink                                    2.0509766
#> PROBE_STOP_25.pupil_raw_deblink_detransient                        2.0509766
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate            2.0509766
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt     1.9363269
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z   1.9363269
#> PROBE_START_26.pupil_raw                                           0.9536077
#> PROBE_START_26.pupil_raw_deblink                                   0.9536077
#> PROBE_START_26.pupil_raw_deblink_detransient                       0.9536077
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate           0.9536077
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt    0.8914853
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z  0.8914853
#> PROBE_STOP_26.pupil_raw                                            1.3920774
#> PROBE_STOP_26.pupil_raw_deblink                                    1.3920774
#> PROBE_STOP_26.pupil_raw_deblink_detransient                        1.3920774
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate            1.3920774
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt     1.3508719
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z   1.3508719
#>                                                                   zscore_min
#> PROBE_START_22.pupil_raw                                           -1.777871
#> PROBE_START_22.pupil_raw_deblink                                   -1.777871
#> PROBE_START_22.pupil_raw_deblink_detransient                       -1.777871
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate           -1.777871
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt    -1.732595
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z  -1.732595
#> PROBE_STOP_22.pupil_raw                                            -1.255038
#> PROBE_STOP_22.pupil_raw_deblink                                    -1.255038
#> PROBE_STOP_22.pupil_raw_deblink_detransient                        -1.255038
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate            -1.255038
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt     -1.179065
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z   -1.179065
#> PROBE_START_23.pupil_raw                                           -2.423805
#> PROBE_START_23.pupil_raw_deblink                                   -2.423805
#> PROBE_START_23.pupil_raw_deblink_detransient                       -2.423805
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate           -2.423805
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt    -2.391404
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z  -2.391404
#> PROBE_STOP_23.pupil_raw                                            -1.440223
#> PROBE_STOP_23.pupil_raw_deblink                                    -1.440223
#> PROBE_STOP_23.pupil_raw_deblink_detransient                        -1.440223
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate            -1.440223
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt     -1.406486
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z   -1.406486
#> PROBE_START_24.pupil_raw                                           -1.388605
#> PROBE_START_24.pupil_raw_deblink                                   -1.388605
#> PROBE_START_24.pupil_raw_deblink_detransient                       -1.388605
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate           -1.388605
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt    -1.311083
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z  -1.311083
#> PROBE_STOP_24.pupil_raw                                            -1.427859
#> PROBE_STOP_24.pupil_raw_deblink                                    -1.427859
#> PROBE_STOP_24.pupil_raw_deblink_detransient                        -1.427859
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate            -1.427859
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt     -1.333209
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z   -1.333209
#> PROBE_START_25.pupil_raw                                           -6.868822
#> PROBE_START_25.pupil_raw_deblink                                   -1.478325
#> PROBE_START_25.pupil_raw_deblink_detransient                       -1.478325
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate           -1.534412
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt    -1.410658
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z  -1.410658
#> PROBE_STOP_25.pupil_raw                                            -1.567514
#> PROBE_STOP_25.pupil_raw_deblink                                    -1.567514
#> PROBE_STOP_25.pupil_raw_deblink_detransient                        -1.567514
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate            -1.567514
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt     -1.394280
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z   -1.394280
#> PROBE_START_26.pupil_raw                                           -2.652579
#> PROBE_START_26.pupil_raw_deblink                                   -2.652579
#> PROBE_START_26.pupil_raw_deblink_detransient                       -2.652579
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate           -2.652579
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt    -2.548880
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z  -2.548880
#> PROBE_STOP_26.pupil_raw                                            -1.119261
#> PROBE_STOP_26.pupil_raw_deblink                                    -1.119261
#> PROBE_STOP_26.pupil_raw_deblink_detransient                        -1.119261
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate            -1.119261
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt     -1.067685
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z   -1.067685
#>                                                                   prop_blink_time
#> PROBE_START_22.pupil_raw                                               0.00000000
#> PROBE_START_22.pupil_raw_deblink                                       0.00000000
#> PROBE_START_22.pupil_raw_deblink_detransient                           0.00000000
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate               0.00000000
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt        0.00000000
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.00000000
#> PROBE_STOP_22.pupil_raw                                                0.00000000
#> PROBE_STOP_22.pupil_raw_deblink                                        0.00000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient                            0.00000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate                0.00000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt         0.00000000
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.00000000
#> PROBE_START_23.pupil_raw                                               0.00000000
#> PROBE_START_23.pupil_raw_deblink                                       0.00000000
#> PROBE_START_23.pupil_raw_deblink_detransient                           0.00000000
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate               0.00000000
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt        0.00000000
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.00000000
#> PROBE_STOP_23.pupil_raw                                                0.00000000
#> PROBE_STOP_23.pupil_raw_deblink                                        0.00000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient                            0.00000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate                0.00000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt         0.00000000
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.00000000
#> PROBE_START_24.pupil_raw                                               0.00000000
#> PROBE_START_24.pupil_raw_deblink                                       0.00000000
#> PROBE_START_24.pupil_raw_deblink_detransient                           0.00000000
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate               0.00000000
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt        0.00000000
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.00000000
#> PROBE_STOP_24.pupil_raw                                                0.00000000
#> PROBE_STOP_24.pupil_raw_deblink                                        0.00000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient                            0.00000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate                0.00000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt         0.00000000
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.00000000
#> PROBE_START_25.pupil_raw                                               0.02801401
#> PROBE_START_25.pupil_raw_deblink                                       0.07803902
#> PROBE_START_25.pupil_raw_deblink_detransient                           0.07803902
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate               0.00000000
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt        0.00000000
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.00000000
#> PROBE_STOP_25.pupil_raw                                                0.00000000
#> PROBE_STOP_25.pupil_raw_deblink                                        0.00000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient                            0.00000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate                0.00000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt         0.00000000
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.00000000
#> PROBE_START_26.pupil_raw                                               0.00000000
#> PROBE_START_26.pupil_raw_deblink                                       0.00000000
#> PROBE_START_26.pupil_raw_deblink_detransient                           0.00000000
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate               0.00000000
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt        0.00000000
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.00000000
#> PROBE_STOP_26.pupil_raw                                                0.00000000
#> PROBE_STOP_26.pupil_raw_deblink                                        0.00000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient                            0.00000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate                0.00000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt         0.00000000
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.00000000
#>                                                                   pre_epoch_pupil_sd
#> PROBE_START_22.pupil_raw                                                 18.87011114
#> PROBE_START_22.pupil_raw_deblink                                         18.87011114
#> PROBE_START_22.pupil_raw_deblink_detransient                             18.87011114
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate                 18.87011114
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt          15.83341397
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z         0.03489315
#> PROBE_STOP_22.pupil_raw                                                  19.58511847
#> PROBE_STOP_22.pupil_raw_deblink                                          19.58511847
#> PROBE_STOP_22.pupil_raw_deblink_detransient                              19.58511847
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate                  19.58511847
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt           16.76829235
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z          0.03695341
#> PROBE_START_23.pupil_raw                                                 39.18459439
#> PROBE_START_23.pupil_raw_deblink                                         39.18459439
#> PROBE_START_23.pupil_raw_deblink_detransient                             39.18459439
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate                 39.18459439
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt          36.55426622
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z         0.08055708
#> PROBE_STOP_23.pupil_raw                                                  46.75058783
#> PROBE_STOP_23.pupil_raw_deblink                                          46.75058783
#> PROBE_STOP_23.pupil_raw_deblink_detransient                              46.75058783
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate                  46.75058783
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt           44.20880062
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z          0.09742589
#> PROBE_START_24.pupil_raw                                                 14.64897395
#> PROBE_START_24.pupil_raw_deblink                                         14.64897395
#> PROBE_START_24.pupil_raw_deblink_detransient                             14.64897395
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate                 14.64897395
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt           8.15804826
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z         0.01797844
#> PROBE_STOP_24.pupil_raw                                                  71.34731950
#> PROBE_STOP_24.pupil_raw_deblink                                          71.34731950
#> PROBE_STOP_24.pupil_raw_deblink_detransient                              71.34731950
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate                  71.34731950
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt           69.20431208
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z          0.15251017
#> PROBE_START_25.pupil_raw                                                 25.10132303
#> PROBE_START_25.pupil_raw_deblink                                         25.10132303
#> PROBE_START_25.pupil_raw_deblink_detransient                             25.10132303
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate                 25.10132303
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt          21.23178857
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z         0.04678991
#> PROBE_STOP_25.pupil_raw                                                  78.38380633
#> PROBE_STOP_25.pupil_raw_deblink                                          78.38380633
#> PROBE_STOP_25.pupil_raw_deblink_detransient                              78.38380633
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate                  78.38380633
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt           80.27863046
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z          0.17691539
#> PROBE_START_26.pupil_raw                                                 14.94516511
#> PROBE_START_26.pupil_raw_deblink                                         14.94516511
#> PROBE_START_26.pupil_raw_deblink_detransient                             14.94516511
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate                 14.94516511
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt          11.00293946
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z         0.02424791
#> PROBE_STOP_26.pupil_raw                                                  25.89951305
#> PROBE_STOP_26.pupil_raw_deblink                                          25.89951305
#> PROBE_STOP_26.pupil_raw_deblink_detransient                              25.89951305
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate                  25.89951305
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt           25.86194518
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z          0.05699370
#>                                                                   epoch_pupil_sd
#> PROBE_START_22.pupil_raw                                             273.2746093
#> PROBE_START_22.pupil_raw_deblink                                     273.2746093
#> PROBE_START_22.pupil_raw_deblink_detransient                         273.2746093
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate             273.2746093
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt      272.9577740
#> PROBE_START_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.6015353
#> PROBE_STOP_22.pupil_raw                                              351.0490369
#> PROBE_STOP_22.pupil_raw_deblink                                      351.0490369
#> PROBE_STOP_22.pupil_raw_deblink_detransient                          351.0490369
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate              351.0490369
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt       350.9249533
#> PROBE_STOP_22.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.7733568
#> PROBE_START_23.pupil_raw                                             301.6715758
#> PROBE_START_23.pupil_raw_deblink                                     301.6715758
#> PROBE_START_23.pupil_raw_deblink_detransient                         301.6715758
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate             301.6715758
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt      301.5107621
#> PROBE_START_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.6644594
#> PROBE_STOP_23.pupil_raw                                              584.8573616
#> PROBE_STOP_23.pupil_raw_deblink                                      584.8573616
#> PROBE_STOP_23.pupil_raw_deblink_detransient                          584.8573616
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate              584.8573616
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt       584.6805383
#> PROBE_STOP_23.pupil_raw_deblink_detransient_interpolate_lpfilt_z       1.2884996
#> PROBE_START_24.pupil_raw                                             319.7413904
#> PROBE_START_24.pupil_raw_deblink                                     319.7413904
#> PROBE_START_24.pupil_raw_deblink_detransient                         319.7413904
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate             319.7413904
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt      319.5755099
#> PROBE_START_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.7042699
#> PROBE_STOP_24.pupil_raw                                              350.6011604
#> PROBE_STOP_24.pupil_raw_deblink                                      350.6011604
#> PROBE_STOP_24.pupil_raw_deblink_detransient                          350.6011604
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate              350.6011604
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt       350.3972600
#> PROBE_STOP_24.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.7721939
#> PROBE_START_25.pupil_raw                                             317.4978075
#> PROBE_START_25.pupil_raw_deblink                                     254.8826566
#> PROBE_START_25.pupil_raw_deblink_detransient                         254.8826566
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate             264.6388087
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt      264.3601953
#> PROBE_START_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.5825883
#> PROBE_STOP_25.pupil_raw                                              187.9236423
#> PROBE_STOP_25.pupil_raw_deblink                                      187.9236423
#> PROBE_STOP_25.pupil_raw_deblink_detransient                          187.9236423
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate              187.9236423
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt       187.5792866
#> PROBE_STOP_25.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.4133810
#> PROBE_START_26.pupil_raw                                             301.7037201
#> PROBE_START_26.pupil_raw_deblink                                     301.7037201
#> PROBE_START_26.pupil_raw_deblink_detransient                         301.7037201
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate             301.7037201
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt      301.3726523
#> PROBE_START_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z      0.6641551
#> PROBE_STOP_26.pupil_raw                                              452.3483977
#> PROBE_STOP_26.pupil_raw_deblink                                      452.3483977
#> PROBE_STOP_26.pupil_raw_deblink_detransient                          452.3483977
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate              452.3483977
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt       452.1269805
#> PROBE_STOP_26.pupil_raw_deblink_detransient_interpolate_lpfilt_z       0.9963825
#> 
#> 
confounds$confounds$epoched_epoch_wide
#> $epoch_prePostProbe
#> $epoch_prePostProbe$block_1
#>                 matched_event    text_unique n_samples n_blinks_in_baseline
#> PROBE_START_22 PROBE_START_22 PROBE_START_22      2000                   NA
#> PROBE_STOP_22   PROBE_STOP_22  PROBE_STOP_22      2000                   NA
#> PROBE_START_23 PROBE_START_23 PROBE_START_23      2000                   NA
#> PROBE_STOP_23   PROBE_STOP_23  PROBE_STOP_23      2000                   NA
#> PROBE_START_24 PROBE_START_24 PROBE_START_24      2000                   NA
#> PROBE_STOP_24   PROBE_STOP_24  PROBE_STOP_24      2000                   NA
#> PROBE_START_25 PROBE_START_25 PROBE_START_25      2000                   NA
#> PROBE_STOP_25   PROBE_STOP_25  PROBE_STOP_25      2000                   NA
#> PROBE_START_26 PROBE_START_26 PROBE_START_26      2000                   NA
#> PROBE_STOP_26   PROBE_STOP_26  PROBE_STOP_26      2000                   NA
#>                time_to_first_blink_ms epoch_duration_ms
#> PROBE_START_22                     NA              1999
#> PROBE_STOP_22                      NA              1999
#> PROBE_START_23                     NA              1999
#> PROBE_STOP_23                      NA              1999
#> PROBE_START_24                     NA              1999
#> PROBE_STOP_24                      NA              1999
#> PROBE_START_25                    295              1999
#> PROBE_STOP_25                      NA              1999
#> PROBE_START_26                     NA              1999
#> PROBE_STOP_26                      NA              1999
#> 
#> 
```
