# Format a `data.frame` of decimals as percentages.

Numeric columns get multiplied by 100 and formatted as percentages
according to user specifications. This function defaults to excluding
the first column of the input data.frame, assuming that it contains a
descriptive variable, but this can be overridden by specifying the
columns to adorn in the `...` argument. Non-numeric columns are always
excluded.

The decimal separator character is the result of `getOption("OutDec")`,
which is based on the user's locale. If the default behavior is
undesirable, change this value ahead of calling the function, either by
changing locale or with `options(OutDec = ",")`. This aligns the decimal
separator character with that used in
[`base::print()`](https://rdrr.io/r/base/print.html).

## Usage

``` r
adorn_pct_formatting(
  dat,
  digits = 1,
  rounding = "half to even",
  affix_sign = TRUE,
  ...
)
```

## Arguments

- dat:

  a data.frame with decimal values, typically the result of a call to
  `adorn_percentages` on a `tabyl`. If given a list of data.frames, this
  function will apply itself to each data.frame in the list (designed
  for 3-way `tabyl` lists).

- digits:

  how many digits should be displayed after the decimal point?

- rounding:

  method to use for rounding - either "half to even", the base R default
  method, or "half up", where 14.5 rounds up to 15.

- affix_sign:

  should the % sign be affixed to the end?

- ...:

  columns to adorn. This takes a tidyselect specification. By default,
  all numeric columns (besides the initial column, if numeric) are
  adorned, but this allows you to manually specify which columns should
  be adorned, for use on a data.frame that does not result from a call
  to `tabyl`.

## Value

a data.frame with formatted percentages

## Examples

``` r
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()
#>  am     4     6     8
#>   0 27.3% 57.1% 85.7%
#>   1 72.7% 42.9% 14.3%

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
  adorn_percentages("col", , recovered:died) %>%
  adorn_pct_formatting(, , , recovered:died)
#>  region year recovered  died
#>    East 2015     59.0% 52.0%
#>    West 2015     41.0% 48.0%
```
