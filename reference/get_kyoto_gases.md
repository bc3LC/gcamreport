# get_kyoto_gases

Get sectorial GHG emissions.

## Usage

``` r
get_kyoto_gases(GCAM_version = "v8.2", GWP_version = "AR5")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

- GWP_version:

  Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or
  'AR4'.

## Value

\`kyoto_gases_clean\` global variable.
