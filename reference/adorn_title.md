# Add column name to the top of a two-way tabyl.

This function adds the column variable name to the top of a `tabyl` for
a complete display of information. This makes the tabyl prettier, but
renders the `data.frame` less useful for further manipulation.

## Usage

``` r
adorn_title(dat, placement = "top", row_name, col_name)
```

## Arguments

- dat:

  A `data.frame` of class `tabyl` or other `data.frame` with a
  tabyl-like layout. If given a list of data.frames, this function will
  apply itself to each `data.frame` in the list (designed for 3-way
  `tabyl` lists).

- placement:

  The title placement, one of `"top"`, or `"combined"`. See **Details**
  for more information.

- row_name:

  (optional) default behavior is to pull the row name from the
  attributes of the input `tabyl` object. If you wish to override that
  text, or if your input is not a `tabyl`, supply a string here.

- col_name:

  (optional) default behavior is to pull the column_name from the
  attributes of the input `tabyl` object. If you wish to override that
  text, or if your input is not a `tabyl`, supply a string here.

## Value

The input `tabyl`, augmented with the column title. Non-tabyl inputs
that are of class `tbl_df` are downgraded to basic data.frames so that
the title row prints correctly.

## Details

The `placement` argument indicates whether the column name should be
added to the `top` of the tabyl in an otherwise-empty row `"top"` or
appended to the already-present row name variable (`"combined"`). The
formatting in the `"top"` option has the look of base R's
[`table()`](https://rdrr.io/r/base/table.html); it also wipes out the
other column names, making it hard to further use the `data.frame`
besides formatting it for reporting. The `"combined"` option is more
conservative in this regard.

## Examples

``` r

mtcars %>%
  tabyl(am, cyl) %>%
  adorn_title(placement = "top")
#>     cyl     
#>  am   4 6  8
#>   0   3 4 12
#>   1   8 3  2

# Adding a title to a non-tabyl
library(tidyr)
library(dplyr)
mtcars %>%
  group_by(gear, am) %>%
  summarise(avg_mpg = mean(mpg), .groups = "drop") %>%
  pivot_wider(names_from = am, values_from = avg_mpg) %>%
  adorn_rounding() %>%
  adorn_title("top", row_name = "Gears", col_name = "Cylinders")
#>         Cylinders     
#> 1 Gears         0    1
#> 2     3      16.1 <NA>
#> 3     4        21 26.3
#> 4     5      <NA> 21.4
```
