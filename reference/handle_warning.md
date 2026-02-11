# handle_warning

An internal function designed to manage mismatches between mapping files
and queries. It prompts the user for input, offering the option to
manually check the issue or to continue with the current process.

## Usage

``` r
handle_warning(mapping_name1, mapping_name2 = NULL, query_name = NULL)
```

## Arguments

- mapping_name1:

  The name of the mapping file 1.

- mapping_name2:

  The name of the mapping file 2.

- query_name:

  The name of the query.

## Value

A warning is issued, and the user's decision (manual check or
continuation) is captured.
