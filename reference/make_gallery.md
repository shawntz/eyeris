# Create interactive epoch gallery report

Generates an interactive HTML gallery report for epoch data with
lightbox functionality.

## Usage

``` r
make_gallery(eyeris, epochs, out, epoch_name, ...)
```

## Arguments

- eyeris:

  An `eyeris` object containing preprocessing results

- epochs:

  Vector of epoch plot file paths or path to zip file

- out:

  Output directory for the report

- epoch_name:

  Name of the epoch for the report

- ...:

  Additional parameters passed from bidsify

## Value

No return value; creates and renders an HTML gallery report
