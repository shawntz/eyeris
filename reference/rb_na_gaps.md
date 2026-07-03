# Collapse missing-sample positions into contiguous gap regions

Rather than drawing one vertical rule per `NA` sample (which can be many
thousands of overlapping lines on a full-resolution time series), the
missing segments are collapsed into contiguous runs so they can be
shaded as a handful of rectangles.

## Usage

``` r
rb_na_gaps(x, y)
```

## Arguments

- x:

  Numeric x positions

- y:

  Numeric y values (gaps are where `y` is `NA`)

## Value

A data frame with `xmin`/`xmax` columns (one row per gap run), or `NULL`
when there are no gaps
