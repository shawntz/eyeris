# Build a data-provenance note when filtering ran over long interpolation gaps

If [`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md)
and/or
[`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md)
were run on data that still contained gaps left as `NA` by
`interpolate(max_gap_ms)`, those steps filtered over the gaps
(temporarily filling and then re-masking them), which can slightly bias
the valid samples adjacent to each gap. This records that fact as a
"Data Quality Notes" section in the HTML report so it is part of the
preprocessing provenance.

## Usage

``` r
make_gap_filter_provenance_note(eyeris)
```

## Arguments

- eyeris:

  An `eyeris` object

## Value

A markdown string for the report (empty string if no such filtering over
gaps occurred, or if `max_gap_ms` is not a single finite numeric value)
