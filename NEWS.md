NEWS
===========

# janitor 0.4.0.9000 (development version, in progress)

## Release summary
Will be version 1.0, containing a new `tabyl` API and with breaking changes to the output of `clean_names()`.

Targeting CRAN submission around February 2018.

## Breaking changes

### A fully-overhauled `tabyl`

Redid the approach to tidy counts / contingency tables, combining `tabyl` and `crosstab` into an all-encompassing function `tabyl` that can tabulate one, two, or three variables.  The resulting `tabyl` data.frames can be manipulated and formatted using a family of `adorn_` functions.  See the [`tabyl` vignette](https://github.com/sfirke/janitor/blob/master/vignettes/tabyls.md) for more.

The legacy functions `crosstab` and `adorn_crosstab` have been deprecated, but remain in the package for now.  Existing code that relies on `tabyl` will break if the `sort` argument is used, as that argument no longer exists in `tabyl` (use `dplyr::arrange()` instead).

### Breaking improvements to `clean_names`

`clean_names()` now detects and preserves camelCase inputs and allows multiple options for case outputs of the cleaned data.frame.  It also converts accented letters and turns `#` into `"number"`.  This is a breaking change, e.g., `variableName` in the data is now converted to `variable_name` (or `variableName`, `VariableName`, etc. depending on your preference).  This may cause old code to break, where it would have been converted to `variablename`.  To minimize this inconvenience, there's a quick fix for compatibility: you can find-and-replace to insert the argument `case = "old_janitor"` to preserve the old behavior of `clean_names()` as of version 0.3.1 (and thus not have to redo your scripts beyond that.)

## Major Features

- `clean_names()` transliterates accented letters, e.g., `çãüœ` becomes `cauoe` [(#120)](https://github.com/sfirke/janitor/issues/120).  Thanks to **@fernandovmacedo**.
  - *To obtain this character transliteration functionality on a Windows computer, you will need version >= 1.1.6 of the stringi package.  As of January 2018, this is available on GitHub, but not yet on CRAN*.


- `clean_names()` offers multiple options for variable name styling.  In addition to `snake_case` you can select `smallCamelCase`, `BigCamelCase`, `ALL_CAPS` and others. [(#131)](https://github.com/sfirke/janitor/issues/131).  
  - Thanks to **@tazinho**, who wrote the [snakecase](https://github.com/Tazinho/snakecase/) package that janitor depends on to do this, as well as the patch to incorporate it into `clean_names()`.


- Launched the janitor documentation website: [http://sfirke.github.io/janitor](http://sfirke.github.io/janitor).  Thanks to the [pkgdown](https://github.com/r-lib/pkgdown) package!

- Deprecated the functions `remove_empty_rows()` and `remove_empty_cols()`, which are replaced by the single function `remove_empty()`. [(#100)](https://github.com/sfirke/janitor/issues/100)
  - `remove_empty()` does not have a default value for the `which` argument, forcing more explicit and readable code. e.g. `remove_empty("rows")`.


- The new `adorn_title()` function shows the name of the 2nd `tabyl` variable - this un-tidies the data.frame but makes the result clearer to readers [(#77)](https://github.com/sfirke/janitor/issues/77)

## Minor Features

- `tabyl` objects now print with row numbers suppressed
- `clean_names()` now retains the character `#` as `"number"` in the resulting names
- The utility function `round_half_up()` is now exported for public use.  It's an exact implementation of [http://stackoverflow.com/questions/12688717/round-up-from-5-in-r/12688836#12688836](http://stackoverflow.com/questions/12688717/round-up-from-5-in-r/12688836#12688836).

## Bug fixes
- `adorn_totals("row")` handles quirky variable names in 1st column [(#118)](https://github.com/sfirke/janitor/issues/118)
- `get_dupes()` returns the correct result when a variable in the input data.frame is already called `"n"` [(#162)](https://github.com/sfirke/janitor/issues/162)

***********************

# janitor 0.3.1  (Release date: 2018-01-04)

## Release summary

This is a bug-fix release with no new functionality or changes.  It fixes a bug where `adorn_crosstab()` failed if the `tibble` package was version > 1.4.

Major changes to janitor are currently in development on GitHub and will be released soon.  This is not that next big release.

***********************

# janitor 0.3.0  (Release date: 2017-05-06)

## Release summary

The primary purpose of this release is to maintain accuracy given breaking changes to the dplyr package, upon which janitor is built, in dplyr version >0.6.0.  This update also contains a number of minor improvements.

**Critical: if you update the package `dplyr` to version >0.6.0, you *must* update janitor to version 0.3.0 to ensure accurate results from janitor's `tabyl()` function.**  This is due to a change in the behavior of dplyr's `_join` functions (*discussed in [#111)](https://github.com/sfirke/janitor/issues/111)*.

janitor 0.3.0 is compatible with this new version of dplyr as well as old versions of dplyr back to 0.5.0.  That is, updating janitor to 0.3.0 does not necessitate an update to dplyr >0.6.0.


## Breaking changes
- The functions `add_totals_row` and `add_totals_col` were combined into a single function, `adorn_totals()`. [(#57)](https://github.com/sfirke/janitor/issues/57).  The `add_totals_` functions are now deprecated and should not be used.
- The first argument of `adorn_crosstab()` is now "dat" instead of "crosstab" (indicating that the function can be called on any data.frame, not just a result of `crosstab()`)

## Major Features

- Exported the `%>%` pipe from magrittr [(#107)](https://github.com/sfirke/janitor/issues/107).

**Deprecated the following functions:**
- `use_first_valid_of()` - use `dplyr::coalesce()` instead
- `convert_to_NA()` - use `dplyr::na_if()` instead
- `add_totals_row()` and `add_totals_col()` - replaced by the single function `adorn_totals()`

## Minor Features
- `adorn_totals()` and `ns_to_percents()` can now be called on data.frames that have non-numeric columns beyond the first one (those columns will be ignored) [(#57)](https://github.com/sfirke/janitor/issues/57)
- `adorn_totals("col")` retains factor class in 1st column if 1st column in the input data.frame was a factor

## Bug fixes
- `clean_names()` now handles leading spaces [(#85)](https://github.com/sfirke/janitor/issues/85)
- `adorn_crosstab()` and `ns_to_percents()` work on a 2-column data.frame [(#89)](https://github.com/sfirke/janitor/issues/89)
- `adorn_totals()` now works on a grouped tibble [(#97)](https://github.com/sfirke/janitor/issues/97)
- Long variable names with spaces no longer break `tabyl()` and `crosstab()` [(#87)](https://github.com/sfirke/janitor/issues/87)
- An `NA_` column in the result of a `crosstab()` will appear at the last column position [(#109)](https://github.com/sfirke/janitor/issues/109)

***********************

# janitor 0.2.1 (Release date: 2016-10-30)

## Bug fixes
- `tabyl()` and `crosstab()` now appear in the package manual [(#65)](https://github.com/sfirke/janitor/issues/65)
- Fixed minor bug per CRAN request - `tabyl()` and `crosstab()` failed to retain ill-formatted variable names only when using R 3.2.5 for Windows [(#76)](https://github.com/sfirke/janitor/issues/76)
- `add_totals_row()` works on two-column data.frame [(#69)](https://github.com/sfirke/janitor/issues/69)
- `use_first_valid_of()` returns POSIXct-class result when given POSIXct inputs

***********************

# janitor 0.2.0 (Release date: 2016-10-03)

## Features

### Major
**Submitted to CRAN!**

### Minor
- The count in `tabyl()` for factor levels that aren't present is now `0` instead of `NA` [(#48)](https://github.com/sfirke/janitor/issues/48)

## Bug fixes
- Can call tabyl() on the result of a tabyl(), e.g., `mtcars %>% tabyl(mpg) %>% tabyl(n)`  [(#54)](https://github.com/sfirke/janitor/issues/54)
- `get_dupes()` now works on variables with spaces in column names [(#62)](https://github.com/sfirke/janitor/issues/62)

## Package management

- Reached 100% unit test code coverage

***********************

# janitor 0.1.2

## Features

### Major
- Added a function `adorn_crosstab()` that formats the results of a `crosstab()` for pretty printing.  Shows % and N in the same cell, with the % symbol, user-specified rounding (method and number of digits), and the option to include a totals row and/or column. E.g., `mtcars %>% crosstab(cyl, gear) %>% adorn_crosstab()`.
- `crosstab()` can be called in a `%>%` pipeline, e.g., `mtcars %>% crosstab(cyl, gear)`.  Thanks to [@chrishaid](https://github.com/chrishaid) [(#34)](https://github.com/sfirke/janitor/pull/34)
- `tabyl()` can also be called in a `%>%` pipeline, e.g., `mtcars %>% tabyl(cyl)` [(#35)](https://github.com/sfirke/janitor/issues/35)
- Added `use_first_valid_of()` function [(#32)](https://github.com/sfirke/janitor/issues/32)
- Added minor functions for manipulating numeric data.frames for presentation: `ns_to_percents()`, `add_totals_row()`, `add_totals_col()`,

### Minor

- `crosstab()` returns 0 instead of NA when there are no instances of a variable combination.
- A call like `tabyl(df$vecname)` retains the more-descriptive `$` symbol in the column name of the result - if you want a legal R name in the result, call it as `df %>% tabyl(vecname)`
- Single and double quotation marks are handled by `clean_names()`


## Package management

- Added codecov to measure test coverage
- Added unit test coverage
- Added Travis-CI for continuous integration

***********************

# janitor 0.1 (Release date: 2016-04-17)

- Initial draft of skeleton package on GitHub
