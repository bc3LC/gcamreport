# filter_loading_regions

Filters a GCAM project dataframe by the desired regions.

## Usage

``` r
filter_loading_regions(
  data,
  desired_regions = "All",
  variable,
  GCAM_version = "v8.2"
)
```

## Arguments

- data:

  Dataframe to filter.

- desired_regions:

  Vector of regions to include. Defaults to 'All'. To view available
  regions, run \`available_regions()\`. The dataset will only include
  the specified regions.

- variable:

  Variable information for the dataset.

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

Filtered dataframe.
