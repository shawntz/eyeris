# Normalize gaze coordinates to screen-relative units

Transforms raw gaze coordinates (in pixels) to normalized coordinates
where:

- (0,0) represents the center of the screen

- Coordinates are scaled to \[-1,1\] range

- Also calculates the normalized distance from screen center

## Usage

``` r
normalize_gaze_coords(pupil_df, screen_width, screen_height)
```

## Arguments

- pupil_df:

  A data frame containing raw gaze coordinates (`eye_x`, `eye_y`)

- screen_width:

  The screen width in pixels

- screen_height:

  The screen height in pixels

## Value

A data frame with added columns:

- `eye_x_norm`: Normalized x coordinate \[-1,1\]

- `eye_y_norm`: Normalized y coordinate \[-1,1\]

- `gaze_dist_from_center`: Normalized distance from screen center
