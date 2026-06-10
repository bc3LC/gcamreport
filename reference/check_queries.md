# check_queries

An internal function designed to assess if all the necessary queries to
compute the desired variable are loaded in the project.

## Usage

``` r
check_queries(var, GCAM_version = "v8.2")
```

## Arguments

- var:

  Variable name

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

boolean indicating if all the required queries to compute the desired
variable are available.
