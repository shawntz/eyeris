# Preprocessing Multiple Runs Stored in Separate Files

Many studies save **each run (block) of a task into its own `.asc`
file** – for example, because the EyeLink recording was stopped and
restarted between runs, or because each phase of a session was exported
separately. This vignette shows the recommended way to preprocess that
kind of data with `eyeris`: loop over the files, preprocess each one
independently, and let the BIDS `run-<index>` entity keep everything
cleanly organized in your derivatives.

💡 **Two different “multiple runs” situations.** There are two ways
multiple runs can show up, and they are handled differently:

- **This vignette:** multiple runs, each in its own separate `.asc`
  file. You process each file with its own
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  → [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md) →
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  call.
- **The other case:** several recording segments embedded inside a
  *single* `.asc` file. That is handled automatically by
  `load_asc(block = “auto”)` and a single
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  call – see the [Complete
  Pipeline](https://eyeris.shawnschwartz.com/articles/complete-pipeline.md)
  and [Anatomy of an `eyeris`
  Object](https://eyeris.shawnschwartz.com/articles/anatomy.md)
  vignettes. We contrast it briefly at the end.

## The one concept to know: `block` *is* `run`

In `eyeris`, the **input** knob is called `block` (the `block` argument
to
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)),
and the **output** BIDS entity is called `run` (the `run-<index>` token
in your derivative filenames). They map one-to-one:

> `block` *N* on input  →  `run-`*0N* on output

So when you set the block number for a file, you are setting its run
number. “Block” is `eyeris`/experiment-design terminology; “run” is the
[BIDS](https://bids-specification.readthedocs.io/en/stable/appendices/entities.html)
acquisition entity. (There is no `block` entity in BIDS – a
separately-acquired repetition of a task is a `run`.) This is why, in
the workflow below, you set the run number once via
`load_asc(block = ...)` and **do not** need to pass `run_num` to
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md).

## The pattern

Suppose participant `AB01` completed three runs of an associative-memory
task, each saved to its own file:

``` r

library(eyeris)

dl <- path.expand("~/Downloads")
asc_files <- file.path(dl, c(
  "sub-AB01_t1_2024-01-15_10h00.00.000.asc",
  "sub-AB01_t2_2024-01-15_10h30.00.000.asc",
  "sub-AB01_t3_2024-01-15_11h00.00.000.asc"
))
stopifnot(all(file.exists(asc_files)))

output_dir <- path.expand("~/Documents/eyeris")

for (i in seq_along(asc_files)) {
  glassbox(asc_files[i], load_asc = list(block = i), verbose = TRUE) |>
    epoch(
      events = "TST_trial-{trial}_{item}_{associate}",
      limits = c(0, 0.1),
      label  = "trialEpochs"
    ) |>
    bidsify(
      bids_dir       = output_dir,
      participant_id = "AB01",
      session_num    = "01",
      task_name      = "assocmem",
      save_raw       = TRUE,
      html_report    = TRUE,
      report_seed    = 0
    )
}
```

What each piece is doing:

- **`load_asc = list(block = i)`** sets the run number for this file. On
  iteration `i = 1` the file becomes `block_1` → `run-01`, `i = 2` →
  `run-02`, and so on. Passing a single numeric `block` also *forces the
  entire file into one run*, which is exactly what you want when one
  file = one run (more on this in [Sanity
  check](#sanity-check-confirm-one-run-per-file) below).
- **[`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md)**
  extracts your trials. Here `TST_trial-{trial}_{item}_{associate}`
  matches each trial event and parses `trial`, `item`, and `associate`
  into columns of the epoched data frame.
- **[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)**
  writes that run’s derivatives. `participant_id`, `session_num`, and
  `task_name` are the same on every iteration, so all three runs land in
  the same subject/session tree, distinguished only by their
  `run-<index>`.

ℹ️ **Why is there no `run_num` in the
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
call?** Because
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
inherits the run number from the `eyeris` object – i.e., from the
`block` you set in
[`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md).
Setting it again in
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
would be redundant. (`run_num` exists to *relabel* a single-run object –
e.g. force a lone file to be saved as `run-03` – and it is silently
ignored for objects that already contain multiple blocks.)

### The equivalent: label the run in `bidsify()` instead

Setting the run number on **input** with `load_asc(block = i)` is the
form we recommend, but it is not the only one. Because each single-run
file already loads as one block by default, you can leave `load_asc`
untouched and instead label each file on **output** with
`bidsify(run_num = i)` – exactly the “relabel a single-run object” use
of `run_num` from the note above. This loop writes the same `run-01`,
`run-02`, `run-03` derivatives as the one before it:

``` r

library(eyeris)

dl <- path.expand("~/Downloads")
asc_files <- file.path(dl, c(
  "sub-AB01_t1_2024-01-15_10h00.00.000.asc",
  "sub-AB01_t2_2024-01-15_10h30.00.000.asc",
  "sub-AB01_t3_2024-01-15_11h00.00.000.asc"
))
stopifnot(all(file.exists(asc_files)))

output_dir <- path.expand("~/Documents/eyeris")

for (i in seq_along(asc_files)) {
  glassbox(asc_files[i], verbose = TRUE) |>
    epoch(
      events = "TST_trial-{trial}_{item}_{associate}",
      limits = c(0, 0.1),
      label  = "trialEpochs"
    ) |>
    bidsify(
      bids_dir       = output_dir,
      run_num        = i,
      participant_id = "AB01",
      session_num    = "01",
      task_name      = "assocmem",
      save_raw       = TRUE,
      html_report    = TRUE,
      report_seed    = 0
    )
}
```

The only two changes from the first loop are: (1)
[`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
no longer receives `load_asc = list(block = i)`, so each file loads with
its default single block; and (2)
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md) now
takes `run_num = i`, which relabels that block as `run-01`, `run-02`,
etc. Everything else – and every output file – is the same.

⚠️ **The two forms are equivalent only for genuinely single-run files.**
`run_num` relabels a file that resolves to *one* block; if a file
happens to contain multiple embedded recording segments, `run_num` is
ignored (and
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
will emit a warning when `verbose = TRUE`) and the runs are numbered
from the embedded blocks instead. `load_asc(block = i)` also *forces*
the whole file into a single run, so it doubles as a guard against
accidentally-multi-segment files. If you prefer the `run_num` form, it
is worth running the [sanity
check](#sanity-check-confirm-one-run-per-file) below to confirm each
file really is one run.

### What you get

After either loop finishes, your derivatives look like this (per-run
data files shown for `run-01`; `run-02` and `run-03` follow the same
pattern):

    eyeris
    └── derivatives
        └── sub-AB01
            └── ses-01
                ├── eye
                │   ├── sub-AB01_ses-01_task-assocmem_run-01_desc-timeseries.csv
                │   ├── sub-AB01_ses-01_task-assocmem_run-01_desc-blinks.csv
                │   ├── sub-AB01_ses-01_task-assocmem_run-01_desc-events.csv
                │   ├── sub-AB01_ses-01_task-assocmem_run-01_desc-epoch_summary.csv
                │   ├── sub-AB01_ses-01_task-assocmem_run-01_desc-preproc_pupil_epoch-trialepochs.csv
                │   ├── epoch_trialEpochs/   # per-trial confounds CSVs for run-01
                │   │   └── ...
                │   ├── sub-AB01_ses-01_task-assocmem_run-02_desc-timeseries.csv
                │   ├── ...
                │   ├── sub-AB01_ses-01_task-assocmem_run-03_desc-timeseries.csv
                │   └── ...
                ├── source
                │   ├── figures
                │   │   ├── task-assocmem_run-01
                │   │   │   └── ...
                │   │   ├── task-assocmem_run-02
                │   │   │   └── ...
                │   │   └── task-assocmem_run-03
                │   │       └── ...
                │   └── logs
                │       ├── task-assocmem_run-01_metadata.json
                │       ├── task-assocmem_run-02_metadata.json
                │       └── task-assocmem_run-03_metadata.json
                ├── sub-AB01_task-assocmem_epoch-trialEpochs_run-01.html
                ├── sub-AB01_task-assocmem_epoch-trialEpochs_run-02.html
                ├── sub-AB01_task-assocmem_epoch-trialEpochs_run-03.html
                └── sub-AB01_task-assocmem.html

Every data file carries its `run-<index>`, and the top-level
`sub-AB01_task-assocmem.html` report aggregates all three runs. For a
full breakdown of what each derivative file contains, see the
[Extracting Data Epochs and Exporting Pupil
Data](https://eyeris.shawnschwartz.com/articles/epoching-bids-reports.md)
vignette.

💡 **Note on the epoch label in filenames.** Epoch labels are sanitized
(lower-cased, punctuation removed, digits dropped) on their way to disk,
so the `label = “trialEpochs”` you supply appears as `epoch-trialepochs`
in the CSV filename. If you search your output by filename, search
case-insensitively.

## Handling missing or non-sequential runs

A common, important question:

> *“What if I have runs 1–3 but run 2 was never collected? Would run 3
> get saved as run 2?”*

**No – `eyeris` never renumbers your runs.** The run number written to
disk is exactly the number you assign via `block =`. There is no global
counter that re-sequences runs across files.

The one thing to watch is **where you get the number from**. In the loop
above, `block = i` uses the *loop position*, which equals the true run
number **only if your files really are runs 1, 2, 3, … in order with
none missing.** If run 2 was never collected and you only have files for
runs 1 and 3, then [`seq_along()`](https://rdrr.io/r/base/seq.html)
would hand file \#2 (the run-3 file) the index `2` and mislabel it as
`run-02`.

The fix is to drive the run number from the **true run identity**, not
the loop position. Two robust options:

**Option A – an explicit vector of run numbers**, paired with your
files:

``` r

asc_files <- file.path(dl, c(
  "sub-AB01_t1_2024-01-15_10h00.00.000.asc", # run 1
  "sub-AB01_t3_2024-01-15_11h00.00.000.asc"  # run 3 (run 2 not collected)
))
run_nums <- c(1, 3) # the TRUE run numbers, in the same order as `asc_files`

for (i in seq_along(asc_files)) {
  glassbox(asc_files[i], load_asc = list(block = run_nums[i]), verbose = TRUE) |>
    epoch(
      events = "TST_trial-{trial}_{item}_{associate}",
      limits = c(0, 0.1),
      label  = "trialEpochs"
    ) |>
    bidsify(
      bids_dir       = output_dir,
      participant_id = "AB01",
      session_num    = "01",
      task_name      = "assocmem",
      save_raw       = TRUE,
      html_report    = TRUE,
      report_seed    = 0
    )
}
```

**Option B – parse the run number out of the filename.** Here the run is
encoded as `_t1_`, `_t2_`, `_t3_`, so a small regex recovers it (and
naturally handles gaps, since a missing run simply has no file):

``` r

for (f in asc_files) {
  run_n <- as.integer(sub(".*_t(\\d+)_.*", "\\1", basename(f)))

  glassbox(f, load_asc = list(block = run_n), verbose = TRUE) |>
    epoch(
      events = "TST_trial-{trial}_{item}_{associate}",
      limits = c(0, 0.1),
      label  = "trialEpochs"
    ) |>
    bidsify(
      bids_dir       = output_dir,
      participant_id = "AB01",
      session_num    = "01",
      task_name      = "assocmem",
      save_raw       = TRUE,
      html_report    = TRUE,
      report_seed    = 0
    )
}
```

With either approach, run 3 is saved as `run-03` and the gap is
preserved.

ℹ️ **Gaps are valid BIDS.** The BIDS specification does *not* require
run indices to be consecutive – a dataset with `run-01` and `run-03` but
no `run-02` is perfectly valid, and it faithfully records what you
actually collected. So there is no need to “compress” your run numbers
to fill a gap.

## Fixing or re-running a single run

Because every run is preprocessed by its **own**
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
call, you can fix or re-run just one run later without touching the
others – which is the main practical advantage of the separate-file
workflow. For example, if you tweak a preprocessing parameter for run 2:

``` r

glassbox(asc_files[2], load_asc = list(block = 2), verbose = TRUE) |>
  epoch(
    events = "TST_trial-{trial}_{item}_{associate}",
    limits = c(0, 0.1),
    label  = "trialEpochs"
  ) |>
  bidsify(
    bids_dir       = output_dir,
    participant_id = "AB01",
    session_num    = "01",
    task_name      = "assocmem",
    save_raw       = TRUE,
    html_report    = TRUE,
    report_seed    = 0
  )
```

[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
cleans out the existing derivatives for that specific subject +
session + task + **run** before writing, so re-running `run-02` replaces
only `run-02`’s files and leaves `run-01` and `run-03` untouched.

## Sanity check: confirm one run per file

EyeLink `.asc` files sometimes contain **more than one recording
segment** (for instance, when the tracker was stopped and restarted for
a recalibration or drift correct). If `eyeris` auto-detects multiple
segments in a file, it treats that file as multi-block – and in that
case `run_num` is ignored and the runs are numbered from the embedded
block numbers instead of from your `block = i`.

Passing a single numeric `block` (as we do above) **forces the whole
file into one run**, which is what you want for one-file-per-run data.
You can confirm a file resolves to exactly one run by checking the
length of `$timeseries`:

``` r

library(eyeris)
#> 
#> eyeris v3.2.0.9003 - Lumpy Space Princess ꒰•ᴗ•｡꒱۶
#> Welcome! Type ?`eyeris` to get started.

# the bundled demo file stands in for one of your per-run .asc files
demo_file <- eyelink_asc_demo_dataset()

eye <- glassbox(demo_file, load_asc = list(block = 1), verbose = FALSE)

length(eye$timeseries) # 1 -> exactly one run in this file
#> [1] 1
names(eye$timeseries)  # "block_1" -> will be written as run-01
#> [1] "block_1"
```

If `length(eye$timeseries)` comes back greater than `1` for a file you
expected to be a single run, that file is being split into multiple
blocks; forcing `block = <run number>` (a single numeric, as above)
collapses it back into one run.

## The other case: multiple runs inside one file

If instead **all** of your runs were recorded into a single continuous
`.asc` file, you don’t loop – you let `eyeris` split the embedded
segments for you and write them in one
[`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
call:

``` r

glassbox(one_file_with_all_runs, load_asc = list(block = "auto")) |>
  epoch(
    events = "TST_trial-{trial}_{item}_{associate}",
    limits = c(0, 0.1),
    label  = "trialEpochs"
  ) |>
  bidsify(
    bids_dir       = output_dir,
    participant_id = "AB01",
    session_num    = "01",
    task_name      = "assocmem"
  )
```

Here `block = "auto"` (the default) detects each recording segment and
numbers them `run-01`, `run-02`, … from their embedded block
numbers.[^1] See the [Complete
Pipeline](https://eyeris.shawnschwartz.com/articles/complete-pipeline.md)
and [Anatomy of an `eyeris`
Object](https://eyeris.shawnschwartz.com/articles/anatomy.md) vignettes
for more on that path.

## ✨ Summary

- When each run lives in its **own** `.asc` file, **loop** over the
  files and give each one its own
  [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  → [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md) →
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  call.
- In `eyeris`, **`block` is `run`**: set the run number once via
  `load_asc(block = ...)`;
  [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md)
  inherits it, so you don’t pass `run_num`.
- `eyeris` **never renumbers runs** – drive the block number from the
  *true* run identity (an explicit vector or a filename parse), not the
  loop position, and missing runs leave valid, faithful gaps
  (e.g. `run-01`, `run-03`).
- Each run is processed independently, so you can **re-run a single
  run** later without disturbing the others.
- Use `length(eye$timeseries)` to confirm a file resolves to a single
  run; pass a numeric `block` to force it.

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

[^1]: If a user supplies multi-block data that is not automatically
    separated by common start/stop recording indicators, we recommend
    the user to manually cut the data into multiple files (by block).
    `eyeris` supplies a bridge function that enables users to pass
    generic tabular eye-tracking data in case of situations like this
    and/or for trackers that are not natively supported by `eyeris` at
    the time of processing.
