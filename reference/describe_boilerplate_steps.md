# Describe each recorded pipeline step as a methods-section sentence

Walks an `eyeris` object's recorded `params` in canonical pipeline order
and returns one prose sentence per step, with the actual parameter
values substituted in. Unknown/custom steps are appended at the end.

## Usage

``` r
describe_boilerplate_steps(params, info)
```

## Arguments

- params:

  The `eyeris$params` list (named by step suffix)

- info:

  The `eyeris$info` list (used for context, e.g., sample rate)

## Value

A character vector of sentences (possibly length 0)
