# Append a totals row to a data.frame.

This function is deprecated, use
[`adorn_totals()`](https://sfirke.github.io/janitor/reference/adorn_totals.md)
instead.

## Usage

``` r
add_totals_row(dat, fill = "-", na.rm = TRUE)
```

## Arguments

- dat:

  an input data.frame with at least one numeric column.

- fill:

  if there are more than one non-numeric columns, what string should
  fill the bottom row of those columns?

- na.rm:

  should missing values (including NaN) be omitted from the
  calculations?

## Value

Returns a data.frame with a totals row, consisting of "Total" in the
first column and column sums in the others.
