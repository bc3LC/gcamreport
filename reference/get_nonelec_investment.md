# get_nonelec_investment

Calculate investment for non-electricity energy supply sectors
(hydrogen, refining/liquids, gas processing) using GCAM's native
"Capital investment demands by tech" query. Values are converted from
1975\$/timestep to billion 2010\$/yr.

## Usage

``` r
get_nonelec_investment(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`nonelec_investment_clean\` global variable
