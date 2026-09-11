# left_join_strict

A restrictive version of `left_join` that ensures that all keys in the
left dataset have corresponding matches in the right dataset. If any
rows in the left dataset do not have matching keys in the right dataset,
the function will throw an error.

## Usage

``` r
left_join_strict(
  left_df,
  right_df,
  by = NULL,
  by_message = by,
  mapping = "",
  ignore = if (exists("ignore.global", envir = .myGlobals)) .myGlobals$ignore.global else
    NULL,
  ...
)
```

## Arguments

- left_df:

  A data frame. The left dataset in the join.

- right_df:

  A data frame. The right dataset in the join.

- by:

  A character vector of variables to join by. If \`NULL\`, the function
  will use all common variables.

- by_message:

  A character vector of variables to join by to output the ERROR
  message, if necessay. If \`NULL\`, the function will use the variables
  defined in the \`by\` parameter.

- mapping:

  Optional. Mapping name to be displayed in case of ERROR.

- ignore:

  Optional. Policy names introduced by the user to ignore during
  gcamreport processing of physical quantities, since otherwise they
  will be flagged as names missing from mapping files and cause an
  error. Note: Currently having one of the specified name patterns in
  any column of the query results, such as sector, subsector, input,
  etc. will cause the error to be disregarded. Same behavior than adding
  the policy names to the corresponding mappings indicating
  \`NoReported\`.

- ...:

  Additional arguments passed to \`dplyr::left_join()\`.

## Value

A data frame resulting from the left join. If any rows in \`left_df\` do
not have matching keys in \`right_df\`, an error is thrown.
