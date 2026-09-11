# check_user_choices_plot

Check user's choices to do the plot: at least one scenario, variable,
year, and region must be chosen. In case of 'grouped' plot, all
variables must be from the same category.

## Usage

``` r
check_user_choices_plot(vars, scen, years, reg, grouped)
```

## Arguments

- vars:

  user's selected variables

- scen:

  user's selected scenarios

- years:

  user's selected years

- reg:

  user's selected regions

- grouped:

  if TRUE, aim to display grouped plot; ungrouped plot otherwise
