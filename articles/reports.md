# QC with Interactive Reports

## 1 Setup

After extracting epochs from your data,

``` r

eye <- eye |>
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

(for more details on extracting pupil data epochs, see the [Extracting
Data Epochs and Exporting Pupil Data
vignette](https://shawnschwartz.com/eyeris/articles/epoching-bids-reports.md);
for more details on this specific example code shown above, [click
here](https://shawnschwartz.com/eyeris/articles/epoching-bids-reports.html#example-c-epoch-with-subtractive-baselining)).

## 2 Generating the Interactive HTML Reports

When running `bidsify` on the previously epoched data, be sure to set
`html_report` to `TRUE` (as shown below).

``` r

bidsify(
  eyeris = eye_1c,
  bids_dir = tempdir(), # Replace with preferred path, like "~/Documents/eyeris"
  participant_id = "001",
  session_num = "01",
  task_name = "assocmem",
  run_num = "01",
  save_raw = TRUE, # Also save raw timeseries
  html_report = TRUE, # Generate an interactive preproc summary report document
  report_seed = 0 # Make randomly selected plot epochs reproducible across runs
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

Here, notice specifically these two files:

- sub-001_task-assocret.html
- sub-001_task-assocret_epoch-prePostProbe_run-01.html

## 3 Previewing your Entire Pupil Timeseries

> `sub-001_task-assocret.html` will look something like this:

![](https://github.com/shawntz/eyeris/raw/dev/inst/figures/report_example_annotated-1.png)

## 4 Data QC of Extracted Pupil Epochs with Interactive Reports

> Meanwhile, `sub-001_task-assocret_epoch-prePostProbe_run-01.html` will
> enable you to interact with images of each extracted data epoch (for
> which you will see include separate images for each epoch at each
> sequential stage of the preprocessing pipeline). This feature was
> intentionally designed to make data QC a default behavior without the
> barriers of needing to code up a script with loops to print out images
> of each preprocessing step for each epoch for each participant.
>
> **As you see below, you can use your left/right arrow keys on** **your
> keyboard to quickly scan through the data from each trial, while**
> **simultaneously watching what happened to any given epoch’s signal
> from start** **to finish! We hope this intuitive feature is fun and
> helps you make more** **appropriate preprocessing decisions to
> optimize your signal-to-noise ratio** **with as little overhead as
> possible!**

![](https://github.com/shawntz/eyeris/raw/dev/inst/figures/interactive-reports-demo.gif)

## 5 Gaze Heatmaps

In addition to the standard pupil timeseries plots, `eyeris` now
includes gaze heatmaps that show the distribution of eye coordinates
across the entire screen area. These heatmaps are automatically
generated in the BIDS reports and can also be created manually.

### Standalone Gaze Heatmaps

When you run
[`bidsify()`](https://shawnschwartz.com/eyeris/reference/bidsify.md)
with `html_report = TRUE`, `eyeris` automatically creates standalone
gaze heatmaps for each block of data. These heatmaps show:

- The distribution of eye coordinates (x and y) across the entire screen
  area
- Screen boundaries marked with solid lines
- The screen center marked with a red cross
- Color-coded density showing where the participant looked most
  frequently

### Manual Gaze Heatmap Creation

You can also create gaze heatmaps manually using the
[`plot_gaze_heatmap()`](https://shawnschwartz.com/eyeris/reference/plot_gaze_heatmap.md)
function:

``` r

plot_gaze_heatmap(
  eyeris = eyeris_data$timeseries$block_1,
  screen_width = eyeris_data$info$screen.x,
  screen_height = eyeris_data$info$screen.y,
  n_bins = 50,
  col_palette = "viridis"
)
```

### Epoch-Level Gaze Heatmaps

For epoched data, each epoch automatically generates a gaze heatmap
showing the distribution of eye coordinates during that specific epoch.
These are saved as separate files in the epoch directories and help you
quickly assess:

- Whether the participant was looking at the screen during the epoch
- The spatial distribution of gaze within the epoch
- Any systematic biases in gaze position

The gaze heatmaps use the screen dimensions stored in the `eyeris$info`
object to properly show the full screen area and use color-coded density
to visualize gaze patterns.

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
