# Create a progress bar for tracking operations

Creates a progress bar using the progress package with customizable
formatting.

## Usage

``` r
progress_bar(
  total,
  msg = "Processing",
  width = 80,
  show_percent = TRUE,
  show_eta = TRUE,
  clear = FALSE
)
```

## Arguments

- total:

  The total number of items to process

- msg:

  The message to display before the progress bar

- width:

  The width of the progress bar in characters

- show_percent:

  Whether to show percentage completion

- show_eta:

  Whether to show estimated time remaining

- clear:

  Whether to clear the progress bar when done

## Value

A progress bar object from the progress package
