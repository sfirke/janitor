# Find the header row in a data.frame

Find the header row in a data.frame

## Usage

``` r
find_header(dat, ...)
```

## Arguments

- dat:

  The input data.frame

- ...:

  See details

## Value

The row number for the header row

## Details

If `...` is missing, then the first row with no missing values is used.

When searching for a specified value or value within a column, the first
row with a match will be returned, regardless of the completeness of the
rest of that row. If `...` has a single character argument, then the
first column is searched for that value. If `...` has a named numeric
argument, then the column whose position number matches the value of
that argument is searched for the name (see the last example below). If
more than one row is found matching a value that is searched for, the
number of the first matching row will be returned (with a warning).

## See also

Other Set names:
[`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md),
[`mu_to_u`](https://sfirke.github.io/janitor/reference/mu_to_u.md),
[`row_to_names()`](https://sfirke.github.io/janitor/reference/row_to_names.md)

## Examples

``` r
# the first row
find_header(data.frame(A = "B"))
#> [1] 1
# the second row
find_header(data.frame(A = c(NA, "B")))
#> [1] 2
# the second row since the first has an empty value
find_header(data.frame(A = c(NA, "B"), B = c("C", "D")))
#> [1] 2
# The third row because the second column was searched for the text "E"
find_header(data.frame(A = c(NA, "B", "C", "D"), B = c("C", "D", "E", "F")), "E" = 2)
#> [1] 3
```
