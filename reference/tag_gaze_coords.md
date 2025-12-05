# Tag gaze coordinates as on/off screen

Identifies when gaze coordinates fall outside the screen boundaries,
with an optional buffer zone to account for potential overshoot in eye
tracking.

## Usage

``` r
tag_gaze_coords(pupil_df, screen_width, screen_height, overshoot_buffer = 0.05)
```

## Arguments

- pupil_df:

  A data frame containing gaze coordinates

- screen_width:

  The screen width in pixels

- screen_height:

  The screen height in pixels

- overshoot_buffer:

  Additional buffer zone beyond screen edges (default: `0.05`).
  Expressed as proportion of screen size. For example, `0.05` means 5%
  beyond screen edges will still be considered "on screen"

## Value

A data frame with added column:

- `is_offscreen`: Logical indicating if gaze is outside screen
  boundaries
