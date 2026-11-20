# Generate a frequency table (1-, 2-, or 3-way).

A fully-featured alternative to
[`table()`](https://rdrr.io/r/base/table.html). Results are data.frames
and can be formatted and enhanced with janitor's family of `adorn_`
functions.

Specify a `data.frame` and the one, two, or three unquoted column names
you want to tabulate. Three variables generates a list of 2-way tabyls,
split by the third variable.

Alternatively, you can tabulate a single variable that isn't in a
`data.frame` by calling `tabyl()` on a vector, e.g.,
`tabyl(mtcars$gear)`.

## Usage

``` r
tabyl(dat, ...)

# Default S3 method
tabyl(dat, show_na = TRUE, show_missing_levels = TRUE, ...)

# S3 method for class 'data.frame'
tabyl(dat, var1, var2, var3, show_na = TRUE, show_missing_levels = TRUE, ...)
```

## Arguments

- dat:

  A `data.frame` containing the variables you wish to count. Or, a
  vector you want to tabulate.

- ...:

  Additional arguments passed to methods.

- show_na:

  Should counts of `NA` values be displayed? In a one-way tabyl, the
  presence of `NA` values triggers an additional column showing valid
  percentages (calculated excluding `NA` values).

- show_missing_levels:

  Should counts of missing levels of factors be displayed? These will be
  rows and/or columns of zeroes. Useful for keeping consistent output
  dimensions even when certain factor levels may not be present in the
  data.

- var1:

  The column name of the first variable.

- var2:

  (optional) the column name of the second variable (its values become
  the column names in a 2-way tabulation).

- var3:

  (optional) the column name of the third variable (a 3-way tabulation
  is split into a list on its values).

## Value

A `data.frame` with frequencies and percentages of the tabulated
variable(s). A 3-way tabulation returns a list of data frames.

## Examples

``` r

tabyl(mtcars, cyl)
#>  cyl  n percent
#>    4 11 0.34375
#>    6  7 0.21875
#>    8 14 0.43750
tabyl(mtcars, cyl, gear)
#>  cyl  3 4 5
#>    4  1 8 2
#>    6  2 4 1
#>    8 12 0 2
tabyl(mtcars, cyl, gear, am)
#> $`0`
#>  cyl  3 4 5
#>    4  1 2 0
#>    6  2 2 0
#>    8 12 0 0
#> 
#> $`1`
#>  cyl 3 4 5
#>    4 0 6 2
#>    6 0 2 1
#>    8 0 0 2
#> 

# or using the %>% pipe
mtcars %>%
  tabyl(cyl, gear)
#>  cyl  3 4 5
#>    4  1 8 2
#>    6  2 4 1
#>    8 12 0 2

# illustrating show_na functionality:
my_cars <- rbind(mtcars, rep(NA, 11))
my_cars %>% tabyl(cyl)
#>  cyl  n    percent valid_percent
#>    4 11 0.33333333       0.34375
#>    6  7 0.21212121       0.21875
#>    8 14 0.42424242       0.43750
#>   NA  1 0.03030303            NA
my_cars %>% tabyl(cyl, show_na = FALSE)
#>  cyl  n percent
#>    4 11 0.34375
#>    6  7 0.21875
#>    8 14 0.43750

# Calling on a single vector not in a data.frame:
val <- c("hi", "med", "med", "lo")
tabyl(val)
#>  val n percent
#>   hi 1    0.25
#>   lo 1    0.25
#>  med 2    0.50
```
