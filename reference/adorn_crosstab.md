# Add presentation formatting to a crosstabulation table.

This function is deprecated, use
[`tabyl()`](https://sfirke.github.io/janitor/reference/tabyl.md) with
the `adorn_` family of functions instead.

## Usage

``` r
adorn_crosstab(
  dat,
  denom = "row",
  show_n = TRUE,
  digits = 1,
  show_totals = FALSE,
  rounding = "half to even"
)
```

## Arguments

- dat:

  a data.frame with row names in the first column and numeric values in
  all other columns. Usually the piped-in result of a call to `crosstab`
  that included the argument `percent = "none"`.

- denom:

  the denominator to use for calculating percentages. One of "row",
  "col", or "all".

- show_n:

  should counts be displayed alongside the percentages?

- digits:

  how many digits should be displayed after the decimal point?

- show_totals:

  display a totals summary? Will be a row, column, or both depending on
  the value of `denom`.

- rounding:

  method to use for truncating percentages - either "half to even", the
  base R default method, or "half up", where 14.5 rounds up to 15.

## Value

Returns a data.frame.
