# Check if a parameter should be omitted from call stack display

Determines if a parameter should be omitted when formatting call stacks
to avoid memory issues with large epoch-related data structures.

## Usage

``` r
should_omit_parameter(name, val)
```

## Arguments

- name:

  The parameter name

- val:

  The parameter value

## Value

TRUE if the parameter should be omitted, FALSE otherwise
