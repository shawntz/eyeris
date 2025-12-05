# List available tables in eyeris database

Lists all tables in the `eyeris` project database with optional
filtering.

## Usage

``` r
eyeris_db_list_tables(con, data_type = NULL, subject = NULL)
```

## Arguments

- con:

  Database connection

- data_type:

  Optional filter by data type

- subject:

  Optional filter by subject ID

## Value

Character vector of table names
