# Filter data by desired regions.

This function filters a dataset based on the specified regions listed in
the "regions" column.

## Usage

``` r
filter_data_regions(data, GCAM_version = "v7.1")
```

## Arguments

- data:

  The dataset to be filtered.

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

A subset of the original data containing only the specified regions.
