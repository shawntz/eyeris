# Process event messages and merge with time series

Matches event messages against templates and extracts metadata,
supporting both exact matches and pattern matching with wildcards.

## Usage

``` r
merge_events_with_timeseries(events, metadata_template, merge = TRUE)
```

## Arguments

- events:

  Events data frame with timestamps and messages

- metadata_template:

  Template pattern to match against

- merge:

  Whether to merge results (default: `TRUE`)

## Value

A data frame with matched events and extracted metadata
