# get_fgas

Computes F-Gases emissions.

## Usage

``` r
get_fgas(GCAM_version = "v8.2", GWP_version = "AR5")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

- GWP_version:

  Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or
  'AR4'.

## Value

\`f_gases_total\`, \`f_gases_hfc\`, \`f_gases_pfc\` global variables.
