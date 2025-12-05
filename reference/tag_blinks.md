# Tag blinks in pupil data

Identifies when pupil data corresponds to eye blinks based on missing
values in the pupil vector.

## Usage

``` r
tag_blinks(pupil_df, pupil_vec)
```

## Arguments

- pupil_df:

  A data frame containing pupil data

- pupil_vec:

  A numeric vector containing pupil diameter values

## Value

A data frame with added column:

- `is_blink`: Logical indicating if pupil data corresponds to a blink
  (NA values)
