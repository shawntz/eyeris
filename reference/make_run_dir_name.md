# Build the canonical run-directory basename

Returns the basename used for both the per-run figure directory and the
filename prefix of every figure it contains. When `task` is supplied,
the name is namespaced as `task-{task}_run-XX` so that different tasks
sharing the same run number (a valid BIDS pattern, e.g.
`task-study_run-01` and `task-test_run-01`) do not collide. When `task`
is `NULL`, the legacy `run-XX` form is returned for backwards
compatibility.

## Usage

``` r
make_run_dir_name(run_num, task = NULL)
```

## Arguments

- run_num:

  Run number (numeric or character coercible to numeric)

- task:

  Optional BIDS task name. Defaults to `NULL`

## Value

A character string, e.g. `"task-study_run-01"` or `"run-01"`

## Details

This is the single source of truth for run-directory naming: every
writer builds the directory and its contained filenames from it, and
every reader either globs with
[`run_dir_pattern()`](https://shawnschwartz.com/eyeris/reference/run_dir_pattern.md)
(then parses the numeric run via `run-(\\d+)`) or derives filenames from
`basename(run_dir)`.
