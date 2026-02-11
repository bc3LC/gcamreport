# load_variable

Recursively loads the specified variable and its dependent variables.

## Usage

``` r
load_variable(var, GCAM_version = "v7.1", GWP_version = "AR5")
```

## Arguments

- var:

  The name of the variable to be loaded. This should be specified as a
  character string.

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

- GWP_version:

  Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or
  'AR4'.

## Value

Loads the specified variable and its dependencies into the environment.
This function does not return a value but ensures that the variable and
its dependencies are available for further processing.

## Details

This internal function is used to load a given variable from the GCAM
project along with any dependent variables that are required for its
proper context and calculations.
