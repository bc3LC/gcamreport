# get_biomass_shares

Get biomass production shares: total = residue + msw + purpose_grown;
The share of msw + residue over the total will be substracted from the
biomass ag demand

## Usage

``` r
get_biomass_shares(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`biomass_shares\` global variable.
