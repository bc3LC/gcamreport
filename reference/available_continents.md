# available_continents

Retrieves and optionally prints a list of all available
continents/regions' groups for the IAMC reporting dataset.

## Usage

``` r
available_continents(print = TRUE, GCAM_version = "v7.1")
```

## Arguments

- print:

  Logical. If TRUE (default), prints the list of available regions'
  groups. to the console. If FALSE, suppresses the printing and only
  returns the list.

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

A vector of character strings representing the names of all available
regions' groups. If \`print\` is TRUE, the function also prints this
list to the console.

## Details

This function provides a list of regions' groups that are available for
use in IAMC reporting. By default, it prints this list, but it can also
be used to obtain the list programmatically.
