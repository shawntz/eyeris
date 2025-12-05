# Check epoch message values against available events

Validates that specified event messages exist in the `eyeris` object.

## Usage

``` r
check_epoch_msg_values(eyeris, events)
```

## Arguments

- eyeris:

  The `eyeris` object containing events

- events:

  A data frame containing event messages to validate

## Value

No return value; throws error if invalid messages are found
