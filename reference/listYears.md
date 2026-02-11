# listYears

Return the years of the queries available for a scenario in a project
data set. This function requires the data set to have been previously
loaded, so it cannot take a file name.

## Usage

``` r
listYears(projData, scenarios = NULL, queries = NULL, anyscen = TRUE)
```

## Arguments

- projData:

  The data set to report on.

- scenarios:

  The name(s) of the scenario(s) to report on. If NULL, report on all of
  them.

- queries:

  The name(s) of the queries(s) to report on. If NULL, report on all of
  them.

- anyscen:

  If TRUE, then list queries that are in any scenario. If FALSE, list
  queries that are in all scenarios.

## Value

list of years reported in the project/scenario/queries.
