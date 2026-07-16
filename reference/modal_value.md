# Find the modal (most frequent) value in a numeric vector

Returns the most common value in a vector. Used to infer the expected
inter-sample interval directly from the data in a way that is robust to
a minority of irregular intervals (e.g., dropped samples).

## Usage

``` r
modal_value(x)
```

## Arguments

- x:

  A numeric vector.

## Value

The most frequently occurring (finite) value in `x`, or `NA_real_` if
`x` contains no finite values.
