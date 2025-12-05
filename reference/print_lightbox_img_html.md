# Print lightbox image HTML for zip-based gallery

Generates HTML code for lightbox image gallery functionality that loads
images from zip files using zip.js.

## Usage

``` r
print_lightbox_img_html(zip_path, image_filenames = NULL, verbose = TRUE)
```

## Arguments

- zip_path:

  Path to the zip file containing images (can be absolute or relative)

- image_filenames:

  Vector of image filenames within the zip

- verbose:

  Logical. Whether to print verbose output (default TRUE).

## Value

A character string containing HTML code for the lightbox gallery
