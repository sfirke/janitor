# Add underlying Ns to a tabyl displaying percentages.

This function adds back the underlying Ns to a `tabyl` whose percentages
were calculated using
[`adorn_percentages()`](https://sfirke.github.io/janitor/reference/adorn_percentages.md),
to display the Ns and percentages together. You can also call it on a
non-tabyl data.frame to which you wish to append Ns.

## Usage

``` r
adorn_ns(
  dat,
  position = "rear",
  ns = attr(dat, "core"),
  format_func = function(x) {
     format(x, big.mark = ",")
 },
  ...
)
```

## Arguments

- dat:

  A data.frame of class `tabyl` that has had `adorn_percentages` and/or
  `adorn_pct_formatting` called on it. If given a list of data.frames,
  this function will apply itself to each data.frame in the list
  (designed for 3-way `tabyl` lists).

- position:

  Should the N go in the front, or in the rear, of the percentage?

- ns:

  The Ns to append. The default is the "core" attribute of the input
  tabyl `dat`, where the original Ns of a two-way `tabyl` are stored.
  However, if your Ns are stored somewhere else, or you need to
  customize them beyond what can be done with `format_func`, you can
  supply them here.

- format_func:

  A formatting function to run on the Ns. Consider defining with
  [`base::format()`](https://rdrr.io/r/base/format.html).

- ...:

  Columns to adorn. This takes a tidyselect specification. By default,
  all columns are adorned except for the first column and columns not of
  class `numeric`, but this allows you to manually specify which columns
  should be adorned, for use on a data.frame that does not result from a
  call to `tabyl`.

## Value

A `data.frame` with Ns appended

## Examples

``` r
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
#>  am         4         6          8
#>   0 3 (27.3%) 4 (57.1%) 12 (85.7%)
#>   1 8 (72.7%) 3 (42.9%)  2 (14.3%)

# Format the Ns with a custom format_func:
set.seed(1)
bigger_dat <- data.frame(
  sex = rep(c("m", "f"), 3000),
  age = round(runif(3000, 1, 102), 0)
)
bigger_dat$age_group <- cut(bigger_dat$age, quantile(bigger_dat$age, c(0, 1 / 3, 2 / 3, 1)))

bigger_dat %>%
  tabyl(age_group, sex, show_missing_levels = FALSE) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(format_func = function(x) format(x, big.mark = ".", decimal.mark = ","))
#>  age_group              f              m          Total
#>     (1,34]  33.9% (1.018)  32.3%   (970)  33.1% (1.988)
#>    (34,68]  33.0%   (990)  33.7% (1.012)  33.4% (2.002)
#>   (68,102]  32.7%   (980)  33.3% (1.000)  33.0% (1.980)
#>       <NA>   0.4%    (12)   0.6%    (18)   0.5%    (30)
#>      Total 100.0% (3.000) 100.0% (3.000) 100.0% (6.000)
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
 adorn_percentages("col",,recovered:died) %>%
 adorn_pct_formatting(,,,,,recovered:died) %>%
 adorn_ns(,,,recovered:died)
#>  region year   recovered       died
#>    East 2015 59.0% (125) 52.0% (13)
#>    West 2015 41.0%  (87) 48.0% (12)
```
