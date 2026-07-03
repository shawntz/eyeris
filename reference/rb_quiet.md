# Muffle `reaborn`'s benign duplicate-colour aesthetic warning

`reaborn` (like `seaborn`) accepts a fixed single-series `color`, which
'ggplot2' reports as a duplicated `colour` aesthetic. The warning is
harmless (the colour is applied correctly), so it is muffled to keep the
console and report generation quiet.

## Usage

``` r
rb_quiet(expr)
```

## Arguments

- expr:

  An expression that builds a `reaborn`/`ggplot` object

## Value

The value of `expr`
