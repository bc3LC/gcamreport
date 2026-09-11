# Get CO2 Price Share

Retrieves the CO2 price share of each region or sector compared to the
total CO2 price.

## Usage

``` r
get_co2_price_share_bysec(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

Global variable \`co2_price_share_bysec\` containing CO2 price shares by
sector.
