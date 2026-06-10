# Filter data by desired regions.

This function filters a dataset based on the specified regions listed in
the "regions" column.

## Usage

``` r
filter_data_regions(data, GCAM_version = "v8.2")
```

## Arguments

- data:

  The dataset to be filtered.

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

A subset of the original data containing only the specified regions.
