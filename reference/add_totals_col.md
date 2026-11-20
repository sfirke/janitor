# Append a totals column to a data.frame.

This function is deprecated, use
[`adorn_totals(where = "col")`](https://sfirke.github.io/janitor/reference/adorn_totals.md)
instead.

## Usage

``` r
add_totals_col(dat, na.rm = TRUE)
```

## Arguments

- dat:

  an input data.frame with at least one numeric column.

- na.rm:

  should missing values (including NaN) be omitted from the
  calculations?

## Value

Returns a data.frame with a totals column containing row-wise sums.
