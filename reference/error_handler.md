# Handle errors with custom error classes

A utility function to handle errors with specific error classes and
provide appropriate error messages using the cli package.

## Usage

``` r
error_handler(e, e_class)
```

## Arguments

- e:

  The error object to handle

- e_class:

  The expected error class to check against

## Value

No return value; either displays an error message via cli or stops
execution with the original error
