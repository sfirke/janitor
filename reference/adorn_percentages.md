# Convert a data.frame of counts to percentages.

This function defaults to excluding the first column of the input
data.frame, assuming that it contains a descriptive variable, but this
can be overridden by specifying the columns to adorn in the `...`
argument.

## Usage

``` r
adorn_percentages(dat, denominator = "row", na.rm = TRUE, ...)
```

## Arguments

- dat:

  A `tabyl` or other data.frame with a tabyl-like layout. If given a
  list of data.frames, this function will apply itself to each
  `data.frame` in the list (designed for 3-way `tabyl` lists).

- denominator:

  The direction to use for calculating percentages. One of "row", "col",
  or "all".

- na.rm:

  should missing values (including `NaN`) be omitted from the
  calculations?

- ...:

  columns to adorn. This takes a
  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  specification. By default, all numeric columns (besides the initial
  column, if numeric) are adorned, but this allows you to manually
  specify which columns should be adorned, for use on a `data.frame`
  that does not result from a call to
  [`tabyl()`](https://sfirke.github.io/janitor/reference/tabyl.md).

## Value

A `data.frame` of percentages, expressed as numeric values between 0 and
1.

## Examples

``` r

mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col")
#>  am         4         6         8
#>   0 0.2727273 0.5714286 0.8571429
#>   1 0.7272727 0.4285714 0.1428571

# calculates correctly even with totals column and/or row:
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_totals("row") %>%
  adorn_percentages()
#>     am         4         6         8
#>      0 0.1578947 0.2105263 0.6315789
#>      1 0.6153846 0.2307692 0.1538462
#>  Total 0.3437500 0.2187500 0.4375000

# Control the columns to be adorned with the ... variable selection argument
# If using only the ... argument, you can use empty commas as shorthand
# to supply the default values to the preceding arguments:

cases <- data.frame(
  region = c("East", "West"),
  year = 2015,
  recovered = c(125, 87),
  died = c(13, 12)
)

cases %>%
  adorn_percentages(, , recovered:died)
#>  region year recovered      died
#>    East 2015 0.9057971 0.0942029
#>    West 2015 0.8787879 0.1212121
```
