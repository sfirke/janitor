# Elevate a row to be the column names of a data.frame.

Elevate a row to be the column names of a data.frame.

## Usage

``` r
row_to_names(
  dat,
  row_number,
  ...,
  remove_row = TRUE,
  remove_rows_above = TRUE,
  sep = "_"
)
```

## Arguments

- dat:

  The input data.frame

- row_number:

  The row(s) of `dat` containing the variable names or the string
  `"find_header"` to use `find_header(dat=dat, ...)` to find the
  row_number. Allows for multiple rows input as a numeric vector. NA's
  are ignored, and if a column contains only `NA` value it will be named
  `"NA"`.

- ...:

  Sent to
  [`find_header()`](https://sfirke.github.io/janitor/reference/find_header.md),
  if `row_number = "find_header"`. Otherwise, ignored.

- remove_row:

  Should the row `row_number` be removed from the resulting data.frame?

- remove_rows_above:

  If `row_number != 1`, should the rows above `row_number` - that is,
  between `1:(row_number-1)` - be removed from the resulting data.frame?

- sep:

  A character string to separate the values in the case of multiple rows
  input to `row_number`.

## Value

A data.frame with new names (and some rows removed, if specified)

## See also

Other Set names:
[`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md),
[`find_header()`](https://sfirke.github.io/janitor/reference/find_header.md),
[`mu_to_u`](https://sfirke.github.io/janitor/reference/mu_to_u.md)

## Examples

``` r
x <- data.frame(
  X_1 = c(NA, "Title", 1:3),
  X_2 = c(NA, "Title2", 4:6)
)
x %>%
  row_to_names(row_number = 2)
#>   Title Title2
#> 3     1      4
#> 4     2      5
#> 5     3      6

x %>%
  row_to_names(row_number = "find_header")
#>   Title Title2
#> 3     1      4
#> 4     2      5
#> 5     3      6
```
