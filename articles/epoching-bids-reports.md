# Extracting Data Epochs and Exporting Pupil Data

`eyeris` was intentionally designed for intuitive, flexible
preprocessing of pupillometry data, with support for event-based
epoching and BIDS-style organization for reproducible workflows.

In this vignette, we’ll walk through a typical use case:

1.  loading raw data,
2.  preprocessing it,
3.  extracting trial-based epochs,
4.  and exporting everything in a clean, analysis-ready format.

We’ll also demonstrate a unique feature we designed to maximize both
your productivity as well as data quality: `interactive HTML reports`,
which include a record of the steps used to preprocess / epoch any given
dataset – and, for epoched data – an interactive “gallery” view to
quickly skim through trial-level data from each step of the
preprocessing pipeline to make quality control and assurance intuitive
and accessible for any dataset (without needing to write any additional
code)!

## 1 Load and Preprocess Your Data

``` r

# Load eyeris
library(eyeris)
#> 
#> eyeris v3.1.0.9000 - Lumpy Space Princess ꒰•ᴗ•｡꒱۶
#> Welcome! Type ?`eyeris` to get started.

# Load the example memory task file and run default glassbox preproc workflow
demo_data <- eyelink_asc_demo_dataset()
eye <- glassbox(demo_data)
#> ✔ [2026-06-13 06:47:20] [OKAY] Running eyeris::load_asc()
#> ℹ [2026-06-13 06:47:21] [INFO] Processing block: block_1
#> ✔ [2026-06-13 06:47:21] [OKAY] Running eyeris::deblink() for block_1
#> ✔ [2026-06-13 06:47:21] [OKAY] Running eyeris::detransient() for block_1
#> ✔ [2026-06-13 06:47:21] [OKAY] Running eyeris::interpolate() for block_1
#> ✔ [2026-06-13 06:47:21] [OKAY] Running eyeris::lpfilt() for block_1
#> ! [2026-06-13 06:47:21] [WARN] Skipping eyeris::downsample() for block_1
#> ! [2026-06-13 06:47:21] [WARN] Skipping eyeris::bin() for block_1
#> ! [2026-06-13 06:47:21] [WARN] Skipping eyeris::detrend() for block_1
#> ✔ [2026-06-13 06:47:21] [OKAY] Running eyeris::zscore() for block_1
#> ℹ [2026-06-13 06:47:21] [INFO] Block processing summary:
#> ℹ [2026-06-13 06:47:21] [INFO] block_1: OK (steps: 6, latest:
#> pupil_raw_deblink_detransient_interpolate_lpfilt_z)
#> ✔ [2026-06-13 06:47:21] [OKAY] Running eyeris::summarize_confounds()
```

## 2 Extract Data Epochs

[`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md) enables
flexible extraction of trials using:

- start/stop events,
- string-based patterns,
- and even embedded trial metadata.

### Example A: Fixed Time Epochs Around a Matched Event

> Extract a 2-second window centered around each “PROBE” event.

``` r

eye_1a <- eye |>
  epoch(events = "PROBE*", limits = c(-1, 1))
#> ℹ [2026-06-13 06:47:21] [INFO] Epoching pupil data...
#> ℹ [2026-06-13 06:47:21] [INFO] Block 1: found 10 matching events for PROBE
#> ✔ [2026-06-13 06:47:22] [OKAY] Done!
#> ✔ [2026-06-13 06:47:22] [OKAY] Block 1: pupil data from 10 unique event
#> messages extracted
#> ✔ [2026-06-13 06:47:22] [OKAY] Pupil epoching completed in 0.19 seconds
#> ℹ [2026-06-13 06:47:22] [INFO] Recalculating epoched confounds for new
#> epochs...
```

Now, if you take a look at `eye`, you’ll notice there’s a new list
element within this `eyeris` object: `epoch_probe`.

``` r

eye_1a$epoch_probe
#> $block_1
#> # A tibble: 20,000 × 21
#>    block time_orig timebin time_secs time_scaled eye_x eye_y eye      hz type   
#>    <dbl>     <int>   <dbl>     <dbl>       <dbl> <dbl> <dbl> <chr> <dbl> <chr>  
#>  1     1  11335474 0           0.983       0.983  975.  545. R      1000 diamet…
#>  2     1  11335475 0.00100     0.984       0.984  975.  544. R      1000 diamet…
#>  3     1  11335476 0.00200     0.985       0.985  975.  544  R      1000 diamet…
#>  4     1  11335477 0.00300     0.986       0.986  976.  544. R      1000 diamet…
#>  5     1  11335478 0.00400     0.987       0.987  975.  545  R      1000 diamet…
#>  6     1  11335479 0.00500     0.988       0.988  975.  545. R      1000 diamet…
#>  7     1  11335480 0.00600     0.989       0.989  975.  544. R      1000 diamet…
#>  8     1  11335481 0.00700     0.99        0.99   975.  544. R      1000 diamet…
#>  9     1  11335482 0.00800     0.991       0.991  975   545. R      1000 diamet…
#> 10     1  11335483 0.00900     0.992       0.992  975.  546. R      1000 diamet…
#> # ℹ 19,990 more rows
#> # ℹ 11 more variables: pupil_raw <dbl>, pupil_raw_deblink <dbl>,
#> #   pupil_raw_deblink_detransient <dbl>,
#> #   pupil_raw_deblink_detransient_interpolate <dbl>,
#> #   pupil_raw_deblink_detransient_interpolate_lpfilt <dbl>,
#> #   pupil_raw_deblink_detransient_interpolate_lpfilt_z <dbl>,
#> #   text_unique <chr>, template <chr>, matching_pattern <chr>, …
#> 
#> $info
#> $info$block_1
#> $info$block_1$calc_baseline
#> [1] FALSE
#> 
#> $info$block_1$apply_baseline
#> [1] FALSE
#> 
#> $info$block_1$epoch_events
#> [1] "PROBE*"
#> 
#> $info$block_1$epoch_limits
#> [1] -1  1
#> 
#> $info$block_1$n_epochs
#> [1] 10
```

By default, the resulting `eyeris` object will contain the epoched data
frame within a list element called `epoch_xyz` where `xyz` will be a
sanitized version of the original `start` event string you supplied for
the pattern matching procedure.

However, you have the ability to customize this label, by passing a
value to the `label` argument within
[`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md).

⚠️ Warning: if no label is specified and there are no event message
strings provided for sanitization, then you may obtain a strange-looking
epoch list element in your output `eyeris` object (e.g., `epoch_`, or
perhaps even `$epoch_nana`, etc.). The extracted data epochs should
still be accessible here, however, to avoid ambiguous list objects, **we
highly recommend you explicitly** **supply sensible epoch labels here
within your
[`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md) calls
to be safe.**

### Example B: Metadata Parsing with Custom Labels

> Extract the 1-second window after “PROBE_START” and apply a custom
> label to the resulting epoch set.

``` r

eye_1b <- eye |>
  epoch(
    events = "PROBE_START_{trial}",
    limits = c(0, 1),
    label = "probeAfter"
  )
#> ℹ [2026-06-13 06:47:22] [INFO] Epoching pupil data...
#> ℹ [2026-06-13 06:47:22] [INFO] Block 1: found 5 matching events for
#> PROBESTARTtrial
#> ✔ [2026-06-13 06:47:22] [OKAY] Done!
#> ✔ [2026-06-13 06:47:22] [OKAY] Block 1: pupil data from 5 unique event messages
#> extracted
#> ✔ [2026-06-13 06:47:22] [OKAY] Pupil epoching completed in 0.07 seconds
#> ℹ [2026-06-13 06:47:22] [INFO] Recalculating epoched confounds for new
#> epochs...

eye_1b |>
  purrr::pluck("epoch_probeAfter") |>
  head()
#> $block_1
#> # A tibble: 5,000 × 21
#>    block time_orig timebin time_secs time_scaled eye_x eye_y eye      hz type   
#>    <dbl>     <int>   <dbl>     <dbl>       <dbl> <dbl> <dbl> <chr> <dbl> <chr>  
#>  1     1  11336474 0            1.98        1.98  972.  550. R      1000 diamet…
#>  2     1  11336475 0.00100      1.98        1.98  971.  551. R      1000 diamet…
#>  3     1  11336476 0.00200      1.98        1.98  970.  551. R      1000 diamet…
#>  4     1  11336477 0.00300      1.99        1.99  970.  550. R      1000 diamet…
#>  5     1  11336478 0.00400      1.99        1.99  971.  548. R      1000 diamet…
#>  6     1  11336479 0.00501      1.99        1.99  972.  547. R      1000 diamet…
#>  7     1  11336480 0.00601      1.99        1.99  972.  548. R      1000 diamet…
#>  8     1  11336481 0.00701      1.99        1.99  972.  548. R      1000 diamet…
#>  9     1  11336482 0.00801      1.99        1.99  972.  550. R      1000 diamet…
#> 10     1  11336483 0.00901      1.99        1.99  972.  550. R      1000 diamet…
#> # ℹ 4,990 more rows
#> # ℹ 11 more variables: pupil_raw <dbl>, pupil_raw_deblink <dbl>,
#> #   pupil_raw_deblink_detransient <dbl>,
#> #   pupil_raw_deblink_detransient_interpolate <dbl>,
#> #   pupil_raw_deblink_detransient_interpolate_lpfilt <dbl>,
#> #   pupil_raw_deblink_detransient_interpolate_lpfilt_z <dbl>,
#> #   text_unique <chr>, template <chr>, matching_pattern <chr>, …
#> 
#> $info
#> $info$block_1
#> $info$block_1$calc_baseline
#> [1] FALSE
#> 
#> $info$block_1$apply_baseline
#> [1] FALSE
#> 
#> $info$block_1$epoch_events
#> [1] "PROBE_START_{trial}"
#> 
#> $info$block_1$epoch_limits
#> [1] 0 1
#> 
#> $info$block_1$n_epochs
#> [1] 5
```

💡 **Note:** You can customize
[`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md) with
trial-level metadata!

For instance, here, `{trial}` will not only extract data but also add a
`trial` column parsed from the event string, which originally took the
form of `PROBE_START_22` (where `22` was the trial number embedded
within the event message string we had originally programmed to be sent
as event messages at the start of each probe trial on our `PsychoPy` /
`EyeLink` experiment.

    #> # A tibble: 5 × 4
    #>   template            matching_pattern    matched_event  trial
    #>   <chr>               <chr>               <chr>          <chr>
    #> 1 PROBE_START_{trial} ^PROBE_START_(.*?)$ PROBE_START_22 22   
    #> 2 PROBE_START_{trial} ^PROBE_START_(.*?)$ PROBE_START_22 22   
    #> 3 PROBE_START_{trial} ^PROBE_START_(.*?)$ PROBE_START_22 22   
    #> 4 PROBE_START_{trial} ^PROBE_START_(.*?)$ PROBE_START_22 22   
    #> 5 PROBE_START_{trial} ^PROBE_START_(.*?)$ PROBE_START_22 22

### Example C: Epoch with Subtractive Baselining

> Use the 1-second window before `"DELAY_STOP"` as a baseline and apply
> it to the epoch data.

``` r

eye_1c <- eye |>
  epoch(
    events = "PROBE_START_{trial}",
    limits = c(0, 1),
    label = "probeEpochs",
    calc_baseline = TRUE,
    apply_baseline = TRUE,
    baseline_type = "sub",
    baseline_events = "DELAY_STOP_*",
    baseline_period = c(-1, 0)
  )
```

In this example, we’re extracting 1-second epochs following each
`"PROBE_START"` event and applying **subtractive baseline correction**.
The baseline is computed from the **1-second window before** each
corresponding `"DELAY_STOP"` event.

In other words, this means each pupil trace is normalized by subtracting
the average pupil size from the pre-probe delay period (i.e., the
baseline period).

### Example D: Start/End Event Pair Epoching

> Manually define start and end times for two trials:

``` r

start_events <- data.frame(
  time = c(11334491, 11338691),
  msg = c("TRIALID 22", "TRIALID 23")
)

end_events <- data.frame(
  time = c(11337158, 11341292),
  msg = c("RESPONSE_22", "RESPONSE_23")
)

eye_1d <- eye |>
  epoch(
    events = list(start_events, end_events, 1), # 1 = block number
    label = "manualTrials"
  )
```

## 3 Export to a BIDS-like Format

Once epoched, your data is ready to be exported with
[`bidsify()`](https://shawnschwartz.com/eyeris/reference/bidsify.md),
which saves the raw and epoched data in a structured, `BIDS`-inspired
format.

``` r

bidsify(
  eyeris = eye_1c,
  bids_dir = "~/Documents/eyeris",
  participant_id = "001",
  session_num = "01",
  task_name = "assocmem",
  run_num = "01",
  save_raw = TRUE, # Also save raw timeseries
  html_report = TRUE # Generate a preprocessing summary
)
```

Which will create a directory structure like this:

    eyeris
    └── derivatives
        └── sub-001
            └── ses-01
                ├── eye
                │   ├── sub-001_ses-01_task-assocret_run-01_desc-timeseries_pupil.csv
                │   └── sub-001_ses-01_task-assocret_run-01_epoch-prePostProbe_desc-preproc_pupil.csv
                ├── source
                │   └── figures
                │       └── task-assocret_run-01
                │           ├── epoch_prePostProbe
                │           │   ├── run-01_PROBE_START_22_1.png
                │           │   ├── run-01_PROBE_START_22_2.png
                │           │   ├── run-01_PROBE_START_22_3.png
                │           │   ├── run-01_PROBE_START_22_4.png
                │           │   ├── run-01_PROBE_START_22_5.png
                │           │   ├── run-01_PROBE_START_22_6.png
                │           │   ├── ...
                │           │   ├── run-01_PROBE_STOP_22_1.png
                │           │   ├── run-01_PROBE_STOP_22_2.png
                │           │   ├── run-01_PROBE_STOP_22_3.png
                │           │   ├── run-01_PROBE_STOP_22_4.png
                │           │   ├── run-01_PROBE_STOP_22_5.png
                │           │   ├── run-01_PROBE_STOP_22_6.png
                │           │   ├── ...
                │           ├── task-assocret_run-01_fig-1_desc-histogram.jpg
                │           ├── task-assocret_run-01_fig-1_desc-timeseries.jpg
                ├── sub-001_task-assocret_epoch-prePostProbe_run-01.html
                └── sub-001_task-assocret.html

    9 directories, 80 files

## 💡 Data Previews and QC with Interactive Reports

See the [🔎 QC with Interactive Reports
vignette](https://shawnschwartz.com/eyeris/articles/reports.md) for more
details.

## ✨ Summary

This vignette demonstrated how to:

- Load and preprocess raw `.asc` (EyeLink) pupil data files using
  `eyeris`.
- Extract event-based epochs using both pattern matching and manual
  timestamps.
- Flexibly apply baseline correction.
- Save out the results in a clean, reproducible, BIDS-like folder
  structure.

Check out the function documentation for
[`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md) and
[`bidsify()`](https://shawnschwartz.com/eyeris/reference/bidsify.md) to
learn more about other customization options that may be useful for your
specific workflow.

------------------------------------------------------------------------

## 📚 Citing `eyeris`

If you use the `eyeris` package in your research, please cite it!

Run the following in R to get the citation:

``` r

citation("eyeris")
#> To cite package 'eyeris' in publications use:
#> 
#>   Schwartz ST, Yang H, Xue AM, He M (2025). "eyeris: A flexible,
#>   extensible, and reproducible pupillometry preprocessing framework in
#>   R." _bioRxiv_, 1-37. doi:10.1101/2025.06.01.657312
#>   <https://doi.org/10.1101/2025.06.01.657312>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {eyeris: A flexible, extensible, and reproducible pupillometry preprocessing framework in R},
#>     author = {Shawn T Schwartz and Haopei Yang and Alice M Xue and Mingjian He},
#>     journal = {bioRxiv},
#>     year = {2025},
#>     pages = {1--37},
#>     doi = {10.1101/2025.06.01.657312},
#>   }
```
