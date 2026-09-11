# load_query

Recursively loads the necessary queries for the specified variables.

## Usage

``` r
load_query(var, base_data, final_queries)
```

## Arguments

- var:

  The name of the variable for which queries are to be loaded. This
  should be provided as a character string.

- base_data:

  A dataframe containing the internal variables required for the
  queries. This dataframe should include necessary context and metadata
  for query loading.

- final_queries:

  A vector of query names or identifiers that need to be loaded. This
  parameter specifies which queries are to be retrieved for the
  variable.

## Value

The function ensures that the specified queries are loaded and
available. It does not return a value directly but updates the internal
state to include the necessary queries.

## Details

This internal function is used to recursively load queries required for
a given variable and its dependencies. It ensures that all necessary
queries are available for processing the specified variables.
