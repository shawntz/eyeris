# Print one or more `ggplot` panels to the active device

A single `ggplot` is printed as-is; a list of panels is combined into a
single row with `patchwork` and printed, optionally with a bold,
centered overall title.

## Usage

``` r
rb_print(plots, title = NULL, nrow = 1)
```

## Arguments

- plots:

  A `ggplot` object, or a list of `ggplot` objects

- title:

  Optional overall title (used only for a list of panels)

- nrow:

  Number of rows when combining a list (default 1)

## Value

Invisibly `NULL`; called for the side effect of drawing
