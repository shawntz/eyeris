# Filter run-directory basenames to a single task

Given a vector of directory basenames (as returned by
`list.dirs(..., full.names = FALSE)`), keep only the valid run
directories that belong to `task`. When `task` is `NULL` (or empty),
only legacy task-less `run-XX` directories are kept. Comparison of the
task component is done by exact string equality (not regex), so task
names containing regex metacharacters or underscores are handled
correctly.

## Usage

``` r
filter_task_run_dirs(dirs, task = NULL)
```

## Arguments

- dirs:

  Character vector of directory basenames

- task:

  Optional BIDS task name. Defaults to `NULL`

## Value

The subset of `dirs` belonging to `task`
