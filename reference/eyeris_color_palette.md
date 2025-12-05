# Default color palette for eyeris plotting functions

A custom color palette designed for visualizing pupil data preprocessing
steps. This palette is based on the RColorBrewer Set1 palette and
provides distinct, visually appealing colors for different preprocessing
stages.

## Usage

``` r
eyeris_color_palette()
```

## Value

A character vector of 7 hex color codes representing the default eyeris
color palette

## Details

The palette includes 7 colors optimized for:

- High contrast and visibility

- Colorblind-friendly design

- Consistent visual hierarchy across preprocessing steps

- Professional appearance in reports and publications

Colors are designed to work well with both light and dark backgrounds
and maintain readability when overlaid in time series plots.

## Examples

``` r
# get the default color palette
colors <- eyeris_color_palette()
print(colors)
#> [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#F781BF" "#A65628"

# use in a plot
plot(1:7, 1:7, col = colors, pch = 19, cex = 3)

```
