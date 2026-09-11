# available_variables

Retrieves and optionally prints a list of all available variables. for
the IAMC reporting dataset.

## Usage

``` r
available_variables(print = TRUE, GCAM_version = "v8.2")
```

## Arguments

- print:

  Logical. If TRUE (default), prints the list of available variables to
  the console. If FALSE, suppresses the printing and only returns the
  list.

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

A vector of character strings representing the names of all available
variables. If \`print\` is TRUE, the function also prints this list to
the console.

## Details

This function provides a list of variables that are available for use in
IAMC reporting. By default, it prints this list, but it can also be used
to obtain the list programmatically.
