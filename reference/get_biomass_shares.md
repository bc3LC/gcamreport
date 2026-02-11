# get_biomass_shares

Get biomass production shares: total = residue + msw + purpose_grown;
The share of msw + residue over the total will be substracted from the
biomass ag demand

## Usage

``` r
get_biomass_shares(GCAM_version = "v7.1")
```

## Arguments

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

\`biomass_shares\` global variable.
