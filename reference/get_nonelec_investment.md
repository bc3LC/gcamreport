# get_nonelec_investment

Calculate investment for non-electricity energy supply sectors
(hydrogen, refining/liquids, gas processing) using GCAM's native
"Capital investment demands by tech" query. Values are converted from
1975\$/timestep to billion 2010\$/yr.

## Usage

``` r
get_nonelec_investment(GCAM_version = "v7.1")
```

## Arguments

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

\`nonelec_investment_clean\` global variable
