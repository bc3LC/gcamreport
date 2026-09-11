# get_co2_emiss

Retrieves the non-bio CO2 emissions query by sector, subsector, and
technology. Emissions are scaled to the no bio sector within the
get_co2_emiss function.

## Usage

``` r
get_co2_emiss(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`co2_emiss\` global variable.
