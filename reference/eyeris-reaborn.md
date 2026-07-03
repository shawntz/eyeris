# Internal `reaborn`-based plotting helpers

`eyeris` renders its diagnostic figures with `reaborn` (an R port of the
'Python' 'seaborn' library that builds on 'ggplot2'). Because every
`reaborn` plot is a `ggplot` object, these helpers construct the
individual panels and hand back `ggplot` objects that the exported
plotting functions [`print()`](https://rdrr.io/r/base/print.html) to the
active graphics device. Printing keeps the existing device-capture flow
used by the interactive HTML reports intact
([`png()`](https://rdrr.io/r/grDevices/png.html)/[`jpeg()`](https://rdrr.io/r/grDevices/png.html)
-\> draw -\> [`dev.off()`](https://rdrr.io/r/grDevices/dev.html)), where
the final page drawn on a static-filename device is the one written to
disk.
