# get_co2_emiss

Retrieves the non-bio CO2 emissions query by sector, subsector, and
technology. Emissions are scaled to the no bio sector within the
get_co2_emiss function.

## Usage

``` r
get_co2_emiss(GCAM_version = "v7.1")
```

## Arguments

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

\`co2_emiss\` global variable.
