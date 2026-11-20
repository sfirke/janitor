# Generate a frequency table of a factor grouped into top-n, bottom-n, and all other levels.

Get a frequency table of a factor variable, grouped into categories by
level.

## Usage

``` r
top_levels(input_vec, n = 2, show_na = FALSE)
```

## Arguments

- input_vec:

  The factor variable to tabulate.

- n:

  Number of levels to include in top and bottom groups

- show_na:

  Should cases where the variable is `NA` be shown?

## Value

A `data.frame` (actually a `tbl_df`) with the frequencies of the
grouped, tabulated variable. Includes counts and percentages, and valid
percentages (calculated omitting `NA` values, if present in the vector
and `show_na = TRUE`.)

## Examples

``` r
top_levels(as.factor(mtcars$hp), 2)
#>                  as.factor(mtcars$hp)  n percent
#>                                52, 62  2  0.0625
#>  <<< Middle Group (18 categories) >>> 28  0.8750
#>                              264, 335  2  0.0625
```
