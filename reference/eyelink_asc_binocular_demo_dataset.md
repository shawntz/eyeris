# Access example EyeLink .asc binocular mock dataset file provided by the eyeris package.

Returns the file path to the demo binocular `.asc` EyeLink pupil data
file included in the `eyeris` package.

## Usage

``` r
eyelink_asc_binocular_demo_dataset()
```

## Value

A character string giving the full file path to the demo `.asc` EyeLink
pupil data file

## Details

This dataset is a mock dataset trimmed from a larger data file. The
original data file was obtained from:
https://github.com/scott-huberty/eyelinkio/blob/main/src/eyelinkio/tests/data/test_raw_binocular.edf

## Examples

``` r
path_to_binocular_demo_dataset <- eyelink_asc_binocular_demo_dataset()
print(path_to_binocular_demo_dataset)
#> [1] "/home/runner/work/_temp/Library/eyeris/extdata/binocular.asc"
```
