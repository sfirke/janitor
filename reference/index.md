# Package index

## Cleaning data

### Cleaning variable names

- [`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md)
  : Cleans names of an object (usually a data.frame).
- [`make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.md)
  : Cleans a vector of text, typically containing the names of an
  object.

## Exploring data

tabyls are an enhanced version of tables. See
[`vignette("tabyls")`](https://sfirke.github.io/janitor/articles/tabyls.md)
for more details.

- [`tabyl()`](https://sfirke.github.io/janitor/reference/tabyl.md) :
  Generate a frequency table (1-, 2-, or 3-way).

- [`adorn_ns()`](https://sfirke.github.io/janitor/reference/adorn_ns.md)
  : Add underlying Ns to a tabyl displaying percentages.

- [`adorn_pct_formatting()`](https://sfirke.github.io/janitor/reference/adorn_pct_formatting.md)
  :

  Format a `data.frame` of decimals as percentages.

- [`adorn_percentages()`](https://sfirke.github.io/janitor/reference/adorn_percentages.md)
  : Convert a data.frame of counts to percentages.

- [`adorn_rounding()`](https://sfirke.github.io/janitor/reference/adorn_rounding.md)
  : Round the numeric columns in a data.frame.

- [`adorn_title()`](https://sfirke.github.io/janitor/reference/adorn_title.md)
  : Add column name to the top of a two-way tabyl.

- [`adorn_totals()`](https://sfirke.github.io/janitor/reference/adorn_totals.md)
  : Append a totals row and/or column to a data.frame

- [`as_tabyl()`](https://sfirke.github.io/janitor/reference/as_tabyl.md)
  :

  Add `tabyl` attributes to a data.frame

- [`untabyl()`](https://sfirke.github.io/janitor/reference/untabyl.md) :

  Remove `tabyl` attributes from a data.frame.

### Change order

- [`row_to_names()`](https://sfirke.github.io/janitor/reference/row_to_names.md)
  : Elevate a row to be the column names of a data.frame.
- [`find_header()`](https://sfirke.github.io/janitor/reference/find_header.md)
  : Find the header row in a data.frame

## Comparison

Compare data frames columns

- [`compare_df_cols()`](https://sfirke.github.io/janitor/reference/compare_df_cols.md)
  : Compare data frames columns before merging
- [`compare_df_cols_same()`](https://sfirke.github.io/janitor/reference/compare_df_cols_same.md)
  : Do the the data.frames have the same columns & types?

## Removing unnecessary columns / rows

- [`remove_constant()`](https://sfirke.github.io/janitor/reference/remove_constant.md)
  : Remove constant columns from a data.frame or matrix.

- [`remove_empty()`](https://sfirke.github.io/janitor/reference/remove_empty.md)
  : Remove empty rows and/or columns from a data.frame or matrix.

- [`get_dupes()`](https://sfirke.github.io/janitor/reference/get_dupes.md)
  :

  Get rows of a `data.frame` with identical values for the specified
  variables.

- [`get_one_to_one()`](https://sfirke.github.io/janitor/reference/get_one_to_one.md)
  : Find the list of columns that have a 1:1 mapping to each other

- [`top_levels()`](https://sfirke.github.io/janitor/reference/top_levels.md)
  : Generate a frequency table of a factor grouped into top-n, bottom-n,
  and all other levels.

- [`single_value()`](https://sfirke.github.io/janitor/reference/single_value.md)
  : Ensure that a vector has only a single value throughout.

## Rounding / dates helpers

Help to mimic some behaviour from Excel or SAS. These should be used on
vector.

- [`round_half_up()`](https://sfirke.github.io/janitor/reference/round_half_up.md)
  : Round a numeric vector; halves will be rounded up, ala Microsoft
  Excel.
- [`signif_half_up()`](https://sfirke.github.io/janitor/reference/signif_half_up.md)
  : Round a numeric vector to the specified number of significant
  digits; halves will be rounded up.
- [`round_to_fraction()`](https://sfirke.github.io/janitor/reference/round_to_fraction.md)
  : Round to the nearest fraction of a specified denominator.
- [`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.md)
  : Convert dates encoded as serial numbers to Date class.
- [`sas_numeric_to_date()`](https://sfirke.github.io/janitor/reference/sas_numeric_to_date.md)
  : Convert a SAS date, time or date/time to an R object
- [`excel_time_to_numeric()`](https://sfirke.github.io/janitor/reference/excel_time_to_numeric.md)
  : Convert a time that may be inconsistently or inconveniently
  formatted from Microsoft Excel to a numeric number of seconds between
  0 and 86400.
- [`convert_to_date()`](https://sfirke.github.io/janitor/reference/convert_to_date.md)
  [`convert_to_datetime()`](https://sfirke.github.io/janitor/reference/convert_to_date.md)
  : Parse dates from many formats

## Misc / helpers

These functions can help perform less frequent operations.

- [`describe_class()`](https://sfirke.github.io/janitor/reference/describe_class.md)
  : Describe the class(es) of an object

- [`paste_skip_na()`](https://sfirke.github.io/janitor/reference/paste_skip_na.md)
  :

  Like [`paste()`](https://rdrr.io/r/base/paste.html), but missing
  values are omitted

- [`chisq.test()`](https://sfirke.github.io/janitor/reference/chisq.test.md)
  :

  Apply [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)
  to a two-way tabyl

- [`fisher.test()`](https://sfirke.github.io/janitor/reference/fisher.test.md)
  :

  Apply
  [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) to
  a two-way tabyl

- [`mu_to_u`](https://sfirke.github.io/janitor/reference/mu_to_u.md) :
  Constant to help map from mu to u
