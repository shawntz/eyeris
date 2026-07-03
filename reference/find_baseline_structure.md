# Find baseline structure name for a given epoch

Helper function to find the correct baseline structure name that matches
the complex baseline naming scheme used by `eyeris`.

## Usage

``` r
find_baseline_structure(eyeris, epoch_label, verbose = TRUE)
```

## Arguments

- eyeris:

  An object of class `eyeris` derived from
  [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)

- epoch_label:

  The epoch label (without "epoch\_" prefix)

- verbose:

  Logical. Whether to print detailed output (default TRUE)

## Value

The baseline structure name or `NULL` if not found
