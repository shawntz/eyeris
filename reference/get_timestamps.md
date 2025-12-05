# Obtain timestamps from events data

Extracts start and end timestamps from events data based on message
patterns.

## Usage

``` r
get_timestamps(
  evs,
  timestamped_events,
  msg_s,
  msg_e,
  limits,
  baseline_mode = FALSE
)
```

## Arguments

- evs:

  Event messages or list of events

- timestamped_events:

  Events data frame with timestamps

- msg_s:

  Start message pattern

- msg_e:

  End message pattern

- limits:

  Time limits for wildcard mode

- baseline_mode:

  Whether in baseline calculation mode

## Value

A list containing start and end timestamps
