# check_inf

An internal function designed to assess if there exist Inf value in a
queary. If Inf found, warn with a message.

## Usage

``` r
check_inf(dataset, value_var_name = "value", dataset_name = NULL)
```

## Arguments

- dataset:

  Dataset to be inspected.

- value_var_name:

  Column name containing the values. By default = 'value'.

- dataset_name:

  Dataset/Query name to display the warning message.

## Value

Warning message if necessary.
