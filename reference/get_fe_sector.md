# Compute and aggregate final energy by sector.

This function consolidates final energy data by handling overlaps
between sector-level and subsector-level queries. For instance, both
international and domestic air transport are categorized under aviation,
but are sourced from different queries: international from the
sector-level and domestic from the subsector-level. This aggregation
step prevents duplicate entries with inconsistent data for the same
reporting categories.

## Usage

``` r
get_fe_sector(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`fe_sector_clean\` global variable.
