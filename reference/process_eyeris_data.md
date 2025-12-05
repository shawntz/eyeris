# Process eyeris data and create eyeris object

Process eyeris data and create eyeris object

## Usage

``` r
process_eyeris_data(x, block, eye, hz, pupil_type, file, binoc, binoc_mode)
```

## Arguments

- x:

  The eyelinker object

- block:

  Block specification

- eye:

  Eye specification ("L", "R", "LR", "left", "right")

- hz:

  Sample rate

- pupil_type:

  Pupil data type

- file:

  Original file path

- binoc:

  Boolean binocular data detected

- binoc_mode:

  Binocular mode ("average", "left", "right", "both")

## Value

An `eyeris` object
