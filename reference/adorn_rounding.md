# Round the numeric columns in a data.frame.

Can run on any `data.frame` with at least one numeric column. This
function defaults to excluding the first column of the input data.frame,
assuming that it contains a descriptive variable, but this can be
overridden by specifying the columns to round in the `...` argument.

If you're formatting percentages, e.g., the result of
[`adorn_percentages()`](https://sfirke.github.io/janitor/reference/adorn_percentages.md),
use
[`adorn_pct_formatting()`](https://sfirke.github.io/janitor/reference/adorn_pct_formatting.md)
instead. This is a more flexible variant for ad-hoc usage. Compared to
[`adorn_pct_formatting()`](https://sfirke.github.io/janitor/reference/adorn_pct_formatting.md),
it does not multiply by 100 or pad the numbers with spaces for alignment
in the results `data.frame`. This function retains the class of numeric
input columns.

## Usage

``` r
adorn_rounding(dat, digits = 1, rounding = "half to even", ...)
```

## Arguments

- dat:

  A `tabyl` or other `data.frame` with similar layout. If given a list
  of data.frames, this function will apply itself to each `data.frame`
  in the list (designed for 3-way `tabyl` lists).

- digits:

  How many digits should be displayed after the decimal point?

- rounding:

  Method to use for rounding - either "half to even" (the base R default
  method), or "half up", where 14.5 rounds up to 15.

- ...:

  Columns to adorn. This takes a tidyselect specification. By default,
  all numeric columns (besides the initial column, if numeric) are
  adorned, but this allows you to manually specify which columns should
  be adorned, for use on a data.frame that does not result from a call
  to `tabyl`.

## Value

The `data.frame` with rounded numeric columns.

## Examples

``` r

mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages() %>%
  adorn_rounding(digits = 2, rounding = "half up")
#>  am    4    6    8
#>   0 0.16 0.21 0.63
#>   1 0.62 0.23 0.15

# tolerates non-numeric columns:
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("all") %>%
  mutate(dummy = "a") %>%
  adorn_rounding()
#>  am   4   6   8 dummy
#>   0 0.1 0.1 0.4     a
#>   1 0.2 0.1 0.1     a

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
  adorn_percentages(, , ends_with("ed")) %>%
  adorn_rounding(, , all_of(c("recovered", "died")))
#>  region year recovered died
#>    East 2015       0.9  0.1
#>    West 2015       0.9  0.1
```
