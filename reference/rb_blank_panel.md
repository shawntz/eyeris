# Build an informative empty panel

Returns a blank `ggplot` panel with a centered message, used in place of
a real plot when there are too few valid samples to draw one.

## Usage

``` r
rb_blank_panel(message, title = NULL, msg_color = "red", xlab = "", ylab = "")
```

## Arguments

- message:

  The message to display in the middle of the panel

- title:

  Optional panel title

- msg_color:

  Colour of the message text (default `"red"`)

- xlab, ylab:

  Axis labels (default empty)

## Value

A `ggplot` object
