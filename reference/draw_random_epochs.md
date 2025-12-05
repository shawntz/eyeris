# Draw random epochs for plotting

Generates random time segments from the time series data for preview
plotting.

## Usage

``` r
draw_random_epochs(x, n, d, hz)
```

## Arguments

- x:

  A data frame containing time series data

- n:

  Number of random epochs to draw

- d:

  Duration of each epoch in seconds

- hz:

  Sampling rate in Hz

## Value

A list of data frames, each containing a random epoch segment
