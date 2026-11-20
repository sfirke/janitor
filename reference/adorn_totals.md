# Append a totals row and/or column to a data.frame

This function defaults to excluding the first column of the input
data.frame, assuming that it contains a descriptive variable, but this
can be overridden by specifying the columns to be totaled in the `...`
argument. Non-numeric columns are converted to character class and have
a user-specified fill character inserted in the totals row.

## Usage

``` r
adorn_totals(dat, where = "row", fill = "-", na.rm = TRUE, name = "Total", ...)
```

## Arguments

- dat:

  An input `data.frame` with at least one numeric column. If given a
  list of data.frames, this function will apply itself to each
  `data.frame` in the list (designed for 3-way `tabyl` lists).

- where:

  One of "row", "col", or `c("row", "col")`

- fill:

  If there are non-numeric columns, what should fill the bottom row of
  those columns? If a string, relevant columns will be coerced to
  character. If `NA` then column types are preserved.

- na.rm:

  Should missing values (including `NaN`) be omitted from the
  calculations?

- name:

  Name of the totals row and/or column. If both are created, and `name`
  is a single string, that name is applied to both. If both are created
  and `name` is a vector of length 2, the first element of the vector
  will be used as the row name (in column 1), and the second element
  will be used as the totals column name. Defaults to "Total".

- ...:

  Columns to total. This takes a tidyselect specification. By default,
  all numeric columns (besides the initial column, if numeric) are
  included in the totals, but this allows you to manually specify which
  columns should be included, for use on a data.frame that does not
  result from a call to `tabyl`.

## Value

A `data.frame` augmented with a totals row, column, or both. The
`data.frame` is now also of class `tabyl` and stores information about
the attached totals and underlying data in the tabyl attributes.

## Examples

``` r
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_totals()
#>     am  4 6  8
#>      0  3 4 12
#>      1  8 3  2
#>  Total 11 7 14
```
