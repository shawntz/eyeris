# Access example EyeLink .asc demo dataset file provided by the eyeris package.

Returns the file path to the demo `.asc` EyeLink pupil data file
included in the `eyeris` package.

## Usage

``` r
eyelink_asc_demo_dataset()
```

## Value

A character string giving the full file path to the demo `.asc` EyeLink
pupil data file

## Examples

``` r
path_to_demo_dataset <- eyelink_asc_demo_dataset()
print(path_to_demo_dataset)
#> [1] "/home/runner/work/_temp/Library/eyeris/extdata/memory.asc"
```
