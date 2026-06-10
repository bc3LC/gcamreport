# get_value_added

Compute value added by the the aggregated agr + ind + services sectors.
Each sector receives 1/3 of the total value added

## Usage

``` r
get_value_added(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`value_added_clean\` global variables.
