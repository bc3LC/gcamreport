# check_match

Check for excluded and included variables: find the unique values of
x\$colmn_x that are not covered in y\$colmn_y, unless opt is set to "i",
in which case it returns the ones that are included

## Usage

``` r
check_match(x, y, colmn_x, colmn_y = NULL, opt = "e")
```

## Arguments

- x:

  base dataset containing colmn_x.

- y:

  base dataset containing colmn_y.

- colmn_x:

  column name present in dataset x.

- colmn_y:

  column name present in dataset y. If set to NULL, colmn_x will be used
  for both datasets (x and y).

- opt:

  if "e", find values present in x\$colmn_x NOT PRESENT in y\$colmn_y,
  if "i", find values PRESENT in x\$colmn_x that are also present in
  y\$colmn_y.
