# Regex matching run-directory basenames

Matches both the task-namespaced form (`task-study_run-01`) and the
legacy form (`run-01`). The numeric run can subsequently be extracted
with `sub(".*run-(\\d+)$", "\\1", x)`.

## Usage

``` r
run_dir_pattern()
```

## Value

A character string containing the regular expression
