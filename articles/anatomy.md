# Anatomy of an eyeris Object

## 📦 Key Components

When preprocessing `.asc` EyeLink files with `eyeris`, returned objects
will be of the class `eyeris`, and will contain key components used
throughout the package’s backend.

The key components are:

- `file`: the original file path of the source `.asc` file
- `timeseries`: a list of data frames (1 df per identified recording
  block per file) which contains the following columns:
  - `block`: block number
  - `time_orig`: raw tracker time (ms)
  - `time_secs`: normalized tracker time, starting at 0 (seconds)
  - `eye_x`: eye position x-coordinate
  - `eye_y`: eye position y-coordinate
  - `eye`: which eye (Left or Right) the recorded data are sourced from
  - `hz`: tracker sampling rate (hz)
  - `type`: whether source data were recorded using the `diameter` or
    `area` method
  - `pupil_raw`: raw recorded pupil source data in arbitrary units
    (a.u.)

You’ll notice that for each preprocessing step run, a new column will be
added after the `pupil_raw` column; these new columns follow a structure
where each subsequent step is appended to the previous columns name
(i.e., `pupil_raw_{previous steps}_{current_step}`). To illustrate:

- `pupil_raw` -\> `pupil_raw_deblink` -\>
  `pupil_raw_deblink_detransient` -\> and so on…

- `events`: a list of data frames containing trial event messages and
  timestamps

- `blinks`: a list of data frames containing start/stop/durations for
  blinks

- `info`: EyeLink EDF header data parsed into a data frame

- `latest`: internal tracker used for assessing which steps have been
  run so far

- `confounds`: a list of data frames containing confounding variables
  for each preprocessing step (see: 📊
  [`summarize_confounds()`](https://shawnschwartz.com/eyeris/reference/summarize_confounds.md))

- `params`: detailed list of steps run and parameters passed to each
  step

- `epoch_{name}`: list of data frames for any given epoched timeseries

💡 **Note:** This vignette describes the structure of an `eyeris` object
as returned by the main preprocessing pipeline.

1.  Understanding these components will help you interpret results,
    debug issues, and extend the pipeline for your own research needs.

2.  Furthermore, binocular data will have a `left` and `right` component
    to the `eyeris` object, which will contain the same components as
    the main `eyeris` object.

3.  There is also a phantom `raw_binocular_object` component to the
    `eyeris` object, which contains the raw binocular data used
    internally to perform the binocular correlation analysis and
    plotting.

Now that we’ve explained what you can expect to see after running the
`eyeris`
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
function, we’ll demonstrate what the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
wrapper is generally comprised of in terms of the steps and defaults
that are implemented.

## 🧱 Building Blocks Under the Hood

While we strongly recommend against manually constructing the pipeline
as will be shown below (given that using the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
will provide maximum opportunities for reproducibility and reduction of
accidental errors), more advanced users may want to see how the
individual steps can be used like building blocks to iteratively test
out parameters, switch steps around / remove steps (***again, we
strongly recommend against doing this unless you know what*** ***you’re
doing***), etc.

### The Default `glassbox()` Steps and Parameters, Deconstructed:

``` r

system.file("extdata", "memory.asc", package = "eyeris") |>
  eyeris::load_asc(block = "auto") |>
  eyeris::deblink(extend = 50) |>
  eyeris::detransient(n = 16) |>
  eyeris::interpolate() |>
  eyeris::lpfilt(wp = 4, ws = 8, rp = 1, rs = 35, plot_freqz = TRUE) |>
  # eyeris::downsample() |>  # optional (please read docs before enabling)
  # eyeris::bin() |>  # optional (please read docs before enabling)
  # eyeris::detrend() |>  # optional (please read docs before enabling)
  eyeris::zscore() |>
  eyeris::summarize_confounds()
```

💡 **For more detailed information on the implementation of functions
within** **the
[`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
and thus how to create your own custom pipeline** **extensions that
conform to the `eyeris` protocol, see the:** [🧩 Building your own
Custom Pipeline Extensions
vignette](https://shawnschwartz.com/eyeris/articles/custom-extensions.md).

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
