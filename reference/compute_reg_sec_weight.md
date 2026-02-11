# compute_reg_sec_weight

An internal function designed to compute the regional weights of a set
of variables. The World region is considered as the annual unit.

## Usage

``` r
compute_reg_sec_weight(dt)
```

## Arguments

- dt:

  dataset with the following columns: scenario, region, var (reporting
  variable), year, value

## Value

dataset with \`reg_sec_weight\` column.
