# evaluate_outcome

Auxiliary function to assign an outcome considering the scenario_compass
data range

## Usage

``` r
evaluate_outcome(data)
```

## Arguments

- data:

  dataset to be assessed. Needs to have the refernce scenario_compass
  data and the report value in different columns.

## Value

dataset with a new column, \`outcome\`, indicating if the value is in
between the \`medium\`, \`high\`, \`ok\` range, or either was
\`not_assigned\` (if NA) or \`failed\` otherwise
