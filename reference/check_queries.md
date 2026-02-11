# check_queries

An internal function designed to assess if all the necessary queries to
compute the desired variable are loaded in the project.

## Usage

``` r
check_queries(var, GCAM_version = "v7.1")
```

## Arguments

- var:

  variable name

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

boolean indicating if all the required queries to compute the desired
variable are available.
