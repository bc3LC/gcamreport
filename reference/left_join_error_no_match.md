# left_join_error_no_match

A restrictive version of `left_join`. Function from `gcamdata`.

## Usage

``` r
left_join_error_no_match(d, ..., ignore_columns = NULL)
```

## Arguments

- d:

  Data frame (typically from pipeline)

- ...:

  Rest of call to `left_join`

- ignore_columns:

  Optional column name(s) to ignore, character vector

## Value

Joined data.

## Details

Restrictive version of \`dplyr::left_join()\` meant for replacing
\`match\` calls.
