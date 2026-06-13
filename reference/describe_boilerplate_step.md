# Describe a single pipeline step as a methods-section sentence

Describe a single pipeline step as a methods-section sentence

## Usage

``` r
describe_boilerplate_step(step, p, info)
```

## Arguments

- step:

  The step name (suffix), e.g. `"deblink"`

- p:

  The recorded parameter list for that step

- info:

  The `eyeris$info` list

## Value

A length-one character string, or `NULL` if the step has no meaningful
description
