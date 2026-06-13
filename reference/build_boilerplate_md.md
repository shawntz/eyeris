# Build the Markdown methods boilerplate for an eyeris object

Internal worker that assembles the full Markdown boilerplate body (note,
methods prose, reproducibility/sidecar reference, citation, and
license). Used both by the exported
[`boilerplate()`](https://shawnschwartz.com/eyeris/reference/boilerplate.md)
function and by the HTML report generator.

## Usage

``` r
build_boilerplate_md(
  eyeris,
  version = NULL,
  n_runs = NULL,
  include_citation = TRUE,
  include_license = TRUE
)
```

## Arguments

- eyeris:

  An `eyeris` object

- version:

  Optional `eyeris` version string; defaults to the installed version

- n_runs:

  Optional integer number of recording blocks/runs; inferred from the
  object when `NULL`

- include_citation:

  Logical; whether to append a citation

- include_license:

  Logical; whether to append the CC BY 4.0 license note

## Value

A length-one character string of Markdown
