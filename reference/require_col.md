# Assert that a required column exists in an input data frame

Assert that a required column exists in an input data frame

## Usage

``` r
require_col(df, col, df_name, role)
```

## Arguments

- df:

  The data frame to check.

- col:

  The required column name.

- df_name:

  The name of the data frame (for the error message).

- role:

  A human-readable description of the column's role.

## Value

No return value; throws an error if the column is missing.
