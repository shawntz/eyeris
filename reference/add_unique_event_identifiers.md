# Add unique event identifiers to handle duplicate event messages

This function adds a new column `text_unique` to each events table that
creates unique identifiers for each occurrence of the same event message
by appending a count number. This prevents events like "GOAL" from being
merged across all separate goals.

## Usage

``` r
add_unique_event_identifiers(events_list)
```

## Arguments

- events_list:

  A list of event data frames (one per block)

## Value

Updated events list with `text_unique` column added to each data frame

## Details

This function is called by the exposed wrapper
[`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)
