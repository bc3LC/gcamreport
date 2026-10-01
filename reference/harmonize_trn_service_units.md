# harmonize_trn_service_units

An internal function that expresses transport service output in million
pass-km and million ton-km. GCAM v9.1 reports transport service in
billion km while earlier versions report it in million km, so billion
rows are rescaled and relabeled.

## Usage

``` r
harmonize_trn_service_units(dataset)
```

## Arguments

- dataset:

  Transport service query result containing \`value\` and \`Units\`
  columns.

## Value

Dataset with transport service values in million pass-km / million
ton-km.
