# conv_ghg_co2e

Converts GHG emissions to CO2e.

## Usage

``` r
conv_ghg_co2e(data, GCAM_version = "v8.2", GWP_version = "AR5")
```

## Arguments

- data:

  Dataset containing GHG emissions.

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

- GWP_version:

  Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or
  'AR4'.
