# get_transmission_invest

Calculate Investment in Electricity Transmission and Distribution.
Scales 2020 numbers based on the average of other model results from
Mcollion et al. 2018. Converts 2015 values to 2010 dollars.

## Usage

``` r
get_transmission_invest(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`transmission_invest_clean\` global variable
