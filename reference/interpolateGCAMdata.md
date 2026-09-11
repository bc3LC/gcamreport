# interpolateGCAMdata

Interpolate GCAM data between the years 2015 2021.

## Usage

``` r
interpolateGCAMdata(
  data,
  yearcol = "year",
  valuecol = "value",
  year_to_appear = base_year
)
```

## Arguments

- data:

  The data set to interpolate on.

- yearcol:

  The year column name. By default, \`year\`.

- valuecol:

  The value column name. By default, \`value\`.

- year_to_appear:

  The year that must be present in the interpolated data. By default,
  \`base_year\`.

## Value

data with the year year_to_appear interpolated linearly.
