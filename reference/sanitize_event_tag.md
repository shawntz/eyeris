# Sanitize event tag string into canonical epoch label

Converts event tag strings into standardized epoch labels by removing
special characters and converting to camel case.

## Usage

``` r
sanitize_event_tag(string, prefix = "epoch_")
```

## Arguments

- string:

  The event tag string to sanitize

- prefix:

  The prefix to add to the sanitized string (default: "epoch\_")

## Value

A sanitized epoch label string
