# janitor 2.2.0 (2023-02-02)

## Breaking changes

These are all minor breaking changes resulting from enhancements and are not expected to affect the vast majority of users.

* A new `...` argument was added to `row_to_names()`, preceding the `remove_row` argument, as part of the new `find_header()` functionality.  If code previously used `remove_row` as an unnamed argument, it will now error.  If code previously used the unsupported behavior of passing anything other than `TRUE` or `FALSE` to `remove_row`, unexpected results may occur.

* Microsoft Excel incorrectly has a leap day on 29 February 1900 (see https://docs.microsoft.com/en-us/office/troubleshoot/excel/wrongly-assumes-1900-is-leap-year).  `excel_numeric_to_date()` did not account for this error, and now it does.  Dates returned from `excel_numeric_to_date()` that precede 1 March 1900 will now be one day later compared to previous versions (i.e. what was 1 Feb 1900 is now 2 Feb 1900), and dates that Excel presents as 29 Feb 1900 will become `as.POSIXct(NA)`.  (#423, thanks **@billdenney** for fixing)

* A minor breaking change is that the time zone is now always set for `excel_numeric_to_date()` and `convert_date()`.  The default timezone is `Sys.timezone()`, previously it was an empty string (`""`). (#422, thanks **@billdenney** for fixing)

* `get_dupes()` results are now sorted first by descending order of `dupe_count`, then alphabetically by sorting variables. (#493)

* There are several minor breaking changes resulting from enhancements to `adorn_ns()`:
  * The addition of the new argument `format_func` means that previous calls relying on `,,,` as shorthand to get to the `...` column selection argument may now require an extra comma.
  * `adorn_ns()` now defaults to displaying numbers of >3 digits with `big.mark = ","`, as part of the default value of the new `format_func` argument.  E.g., `1234`  is now `1,234`.
  * `adorn_ns()` no longer prints leading whitespace when `position = "front"` - this is not a visible change in the printed result and it would be rare that this affects any code.

* When the first column of the data.frame input to `adorn_totals()` is a factor and a totals row is added to the bottom, that column now remains a factor, with "Total" or other user-specified totals name added to its factor levels (#494).

## New features

* `row_to_names()` now has a new helper function, `find_header()` to help find the row that contains the names.  It can be used by passing `row_number="find_header"`.  See the documentation of `row_to_names()` and `find_header()` for more examples. (fix #429)

* `remove_empty()` has a new argument, `cutoff` which allows rows or columns to be removed if at least the `cutoff` fraction of the data are missing.  (fix #446, thanks to **@jzadra** for suggesting the feature and **@billdenney** for fixing)

* A new function `sas_numeric_to_date()` has been added to convert SAS dates, times, and datetimes to R objects (fix #475, thanks to **@billdenney** for suggesting and implementing)

* A new function `single_value()` has been added to ensure that only a single value or missing values are present in a vector (fix #428)

* A new function `get_one_to_one()` has been added to find columns that map 1:1 to each other, even if the values within the columns differ (fix #291, **@billdenney**)

* `adorn_Ns()` contains a new `format_func` argument so that the user can format the Ns to their liking, e.g., changing the `big.mark` character. (#444)

* `clean_names()` can now be called on database connection in a dbplyr code pipeline (#467)

## Minor features

* `make_clean_names()` (and therefore `clean_names()`) issues a warning if the mu or micro symbol is in the names and it is not or may not be handled by a `replace` argument value.  (#448, thanks **@IndrajeetPatil** for reporting and **@billdenney** for fixing)  The rationale is that standard transliteration would convert `"[mu]g"` to `"mg"` when it would be more typically be converted to `"ug"` for use as a unit.  A new, unexported constant (janitor:::mu_to_u) was added to help with mu to "u" replacements.

* `excel_numeric_to_date()` now warns when times are converted to `NA` due to hours that do not exist because of daylight savings time (fix #420, thanks **@Geomorph2** for reporting and **@billdenney** for fixing).  It also warns when inputs are not positive, since Excel only supports values down to 1 (#423).

* If a `tabyl()` or similar data.frame is sorted (e.g., with `dplyr::arrange()`), then has `adorn_totals()` and/or `adorn_percentages()` called on it, followed by `adorn_ns()`, the Ns will be sorted correctly to match the tabyl they're being adorned on. (fix #407)

* `clean_names()` now supports all object types that have either names or dimnames (#481, @DanChaltiel).

* `adorn_pct_formatting()` uses the locale-dependent value of `decimal.mark` as a decimal separator, e.g., in locales where `getOption("OutDec")` is `,` it will print percentages in the format `"12,34%"`.  This character can also be set manually with `options(OutDec = ",")`.(#451).

* `adorn_totals(where ="row")` now preserves factor class and levels of the first column of the input data.frame (#494). 

* `make_clean_names()` now allows for duplicate names to be returned by specifying `TRUE` to the new `allow_dupes` argument (#495, @JasonAizkalns).

* Some warning messages now have classes so that they can be specifically suppressed with `suppressWarnings(..., class="the_class_to_suppress")`.  To find the class of a warning you typically must look at the code where the error is occurring.  (#452, thanks to **@mgacc0** for suggesting and **@billdenney** for fixing)

## Bug fixes

* `adorn_percentages()` was refactored for compatibility with `dplyr` package versions >= 1.1.0 (#490)

* When a numeric variable is supplied as the 2nd variable (column) or 3rd variable (list) of a `tabyl`, the resulting columns or list are now sorted in numeric order, not alphabetic. (#438, thanks **@daaronr** for reporting and **@mattroumaya** for fixing)

* `tabyl()` now succeeds when the second variable is named `"n"` (#445).

* `adorn_ns()` can act on a single-column data.frame input with custom Ns supplied if the variable to adorn is specified with `...` (#456).

* `adorn_totals()` on a one_way tabyl preserves the `tabyl_type` attribute so that a subsequent call to `adorn_pct_formatting()` works correctly on one-way tabyls (#523).

# janitor 2.1.0 (2021-01-05)

## New features

* The `adorn_totals()` function now accepts the special argument `fill = NA`, which will insert a class-appropriate `NA` value into each column that isn't being totaled.  This preserves the class of each column; previously they were all convered to character. (thanks **@hamstr147** for implementing in #404 and **@ymer** for reporting in #298).

* `adorn_totals()` now takes the value of `"both"` for the `where` argument.  That is, `adorn_totals("both")` is a shorter version of `adorn_totals(c("col", "row"))`.  (#362, thanks to **@svgsstats** for implementing and **@sfd99** for suggesting).

* `adorn_totals()` now optionally accepts separate name values for a totals row and a totals column.  The default remains that a single name, `"Total"`, is applied to both.  But now if a vector of two strings is passed to the `name` parameter, the first one will be used as the row heading (in column 1) and the second will be used as the column heading. (Thanks **@francisbarton** for suggesting in #359 and implementing in #413.)



## Bug fixes

* Fixed rounding issue in round_half_up() function (#396, thanks to **@JJSteph**)

* Warnings for incomplete argument names are fixed (fix #367, thanks to **@pabecerra** for reporting and **@billdenney** for fixing)

* 3-way tabyls with factors have columns and rows sorted in the correct order, by factor level (#379).

* Transliteration from extended ASCII (character codes >127) to printable ASCII (character codes <=127) is now better supported (#389, thanks to **@dcorynia** for reporting and **@billdenney** for fixing)

* `clean_names` called on a grouped tibble now also changes the names of the grouping variable(s), in addition to the column names (#260, thanks **@CerebralMastication** for reporting and the tidyverse team for fixing).

* Omitting a numeric column of a tibble when using the `...` select in `adorn_totals()` now succeeds (#388)

* A call to make a 3-way `tabyl()` now succeeds when the first variable is of class `ordered` (#386)

* If a totals row and/or column is present on a tabyl as a result of `adorn_totals()`, the functions `chisq.test()` and `fisher.test()` drop the totals and print a warning before proceding with the calculations (#385).

# janitor 2.0.1 (2020-04-12)

## Bug fixes and Breaking changes

Transliteration of characters within `make_clean_names()` now operates across operating systems, independent of differences in `stringi` installations (Fix #365, thanks to **@eamoncaddigan** for reporting and **@billdenney** for fixing).

This bug patch represents a breaking change with the way that `make_clean_names()` worked in janitor versions 1.2.1.9000 and 2.0.0 as the transliterations are now more generalized and follow a more best-practice approach to transliterating to ASCII.

# janitor 2.0.0 (2020-04-07)

## Breaking changes

* `clean_names()` and `make_clean_names()` are now more locale-independent and translation to ASCII is simpler (in many cases, Unicode is removed, e.g., the Greek character "delta" becomes a "d"). You may also now control how substitutions occur and add your own substitutions (like "%" becoming "percent").  As a result of these changes, the clean names generated by these functions may break with what was produced in prior versions of janitor. (Fix #331, thanks to @billdenney)

As part of the improvements to `make_clean_names()` and `clean_names()`, the `...` argument was added, allowing the user to pass additional information to the underlying transformation function from the `snakecase` package, `to_any_case()`.  This allows for greater user control of `clean_names()` / `make_clean_names()` and for new functionality like specifying `case = "title"` for transforming variable names back to title case for making plots.

* The `adorn_*` family of functions now allows control of columns to be adorned using the `...` argument.  This often-requested feature results in a small breakage as the now-redundant argument `skip_first_col` in `adorn_percentages()` was removed.

* Obsolete functions were deprecated: `crosstab`, `adorn_crosstab`, `use_first_valid_of`, `convert_to_NA`, `remove_empty_cols`, `remove_empty_rows`, `add_totals_col`, `add_totals_row`.

## Major features


* The new functions `convert_to_date()` and `convert_to_datetime()` generalize the work done by `excel_numeric_to_date()` allowing conversion to date or datetimes from many forms of input from numeric, to characters that look like numbers, to characters that look like dates or datetimes, to Dates, to date-times (POSIXt) (#310, thanks to **@billdenney** for implementing).  For instance, this succeeds: `convert_to_date(c("2020-02-29", "40000.1"))`.

* The new function `signif_half_up()` rounds a numeric vector to the specified number of significant digits with halves rounded up (#314, thanks to **@khueyama** for suggesting and implementing).

* `make_clean_names()` now allows the user to specify parts of names to be replaced (Fix #316, thanks to @woodwards for reporting and @woodwards and @billdenney for implementing)

* `make_clean_names()` will ensure that column names are never duplicated (Fix #251, thanks to @jzadra for reporting and @billdenney for implementing)

* `clean_names()` and `make_clean_names()` have a more generic interface where all arguments from `make_clean_names()` are accessible from `clean_names()` (Fix #339, thanks to @ari-nz and @billdenney).

* The variables considered by the function `get_dupes()` can be specified using the select helper functions from `tidyselect`.  This includes `-column_name` to omit a variable as well as the matching functions `starts_with()`, `ends_with()`, `contains()`, and `matches()`.  See `?tidyselect::select_helpers` for more (#326, thanks to **@jzadra** for suggesting and implementing).

## Minor features

* A `quiet` argument was added to `remove_empty()` and `remove_constant()`  providing more information when `quiet = 'FALSE'` (#70, thanks to **@jbkunst** for suggesting and **@billdenney** for implementing).

* `row_to_names()` works on matrix input (#320, thanks to **@billdenney** for suggesting and implementing

* `clean_names()` can now be called on *tbl_graph* objects from the `tidygraph` package. (#252, thanks to @gvdr for bringing up the issue and thanks to @Tazinho for proposing solution).

## Bug fixes

* `adorn_ns()` doesn't append anything to character columns when called on a data.frame resulting from a call to `adorn_percentages()`.  (#195).

* The `name` argument to `adorn_totals()` is correctly applied to 3-way tabyls (#306) (thanks to **@jzadra** for reporting).

* `adorn_rounding()` now works when called on a 3-way tabyl.

* `remove_constant()` works correctly with tibbles (in addition to already working on data.frames and matrices) (thanks to **@billdenney** for implementing).

* `get_dupes()` works when called on a grouped tibble (#329) (thanks to **@jzadra** for fixing).

* When the second variable in a tabyl (the column variable) contains the empty string `""`, it is converted to `"emptystring_` before being spread to the tabyl's column names.  Previously it became the default variable name `V1`. (#203).

* Behind-the-scenes code changes to maintain compatibility with breaking changes to dplyr 1.0.0, tibble 3.0.0, and R 4.0.0.

# janitor 1.2.1 (2020-01-22)

Adjusted a single test to account for a different error message produced by the `tidyselect` package.  No changes to package functionality.

# janitor 1.2.0 (2019-04-20)

## Major features

* The new function `make_clean_names()` takes a character vector and returns the cleaned text, with the same functionality as the existing `clean_names()`, which runs on a data.frame, manipulating its names. (#197, thanks **@tazinho** and everyone who contributed to the discussion).

This function can be supplied as a value for the `.name_repair` argument of `as_tibble()` in the `tibble` package.  For example: `as_tibble(iris, .name_repair = make_clean_names)`.

* The new function `compare_df_cols()` compares the names and classes of columns in a set of supplied data.frames or tibbles, reporting on the specific columns that are or are not similar.  This is for the common use case where a set of data files should all have the same specifications but, in practice, may not. A companion function `compare_df_cols_same()` gives a `TRUE/FALSE` result indicating if the columns are the same (and therefore bindable, though FALSE is not definitive that binding will fail).

  * Its helper function `describe_class()` is exported for developers who wish to extend it so that the `compare_df_` functions treat their custom classes appropriately.

This feature (#50) took almost 3 years from conception to implementation.  Major thanks to **@billdenney** for making it happen!

* A new function `round_to_fraction()` allows rounding to a fraction with specified denominator, e.g., to the nearest 1/7 (#235, thanks to **@billdenney** for suggesting & implementing).

* The functions `janitor::chisq.test()` and `janitor::fisher.test()` to enable running these statistical tests from the base `stats` package on two-way `tabyl` objects.  While the package loading message says the base functions are masked, the base tests still run on `table` objects (#255, thanks **@juba** for implementing).

* `remove_empty()` now has a companion function `remove_constant()` which removes columns containing only a single unique value, optionally ignoring `NA` (#222, thanks to **@billdenney** for suggesting & implementing).


## Minor features

* `excel_numeric_to_date()` now returns a POSIXct object and includes a time zone. (#225, thanks to **@billdenney** for the feature.)

* `clean_names()` can now be called on a *simple features* object from the `sf` package.  (#247, thanks to **@JosiahParry** for suggesting & implementing.)

* `adorn_totals()` gains an argument `"name"` that allows the user to specify a value other than "Total" to appear as the name of the added row and/or column (#263).  Thanks to **@StephieLaPugh** for suggesting and **@daniel-barnett** for implementing.

* `remove_empty()` and `remove_constant()` now work with matrices (returning a matrix).  (#215)  Thanks to **@jsta** for reporting and **@billdenney** for patching.

* If the third variable in a three-way tabyl is a factor, the resulting list is sorted in order of its levels (#250).  Empty factor levels in the 3rd variable are still omitted regardless of the value of `show_missing_levels`.

## Bug fixes

* `excel_numeric_to_date()` no longer gives an overflow error for integer input (for dates since 1968).  (#241)  Thanks to **@hideaki** for reporting and **@billdenney** for patching.

* `clean_names()` and `make_clean_names()` now support 'none' as a case option, passed through to `snakecase::to_any_case()`. (#269) Thanks to **@andrewbarros** for reporting and patching.


# janitor 1.1.1 (2018-07-30)

## Release summary

Patches a bug introduced in version 1.1.0 where `excel_numeric_to_date()` would fail if given an input vector containing an `NA` value.

## Bug fixes

* `excel_numeric_to_date()` again handles `NA` correctly, in version 1.1.0 the function would error if any values of the input vector were `NA`. (#220). Thanks **@emilelatour** for reporting and **@billdenney** for patching.


# janitor 1.1.0 (2018-07-17)

## Release summary
This release was requested by CRAN to address some minor package dependency issues.  It also contains several updates and additions described below.

## Major features

The new function `row_to_names()` handles the case where a dirty data file is read in with its names stored as a row of the data.frame, rather than in the names.  This function sets the names of the data.frame to this row and optionally cleans up the rows above and including where the names were stored.  Thanks to **@billdenney** for writing this feature.

## Minor features

`excel_numeric_to_date()` can now convert fractions of a day to time, e.g., `excel_numeric_to_date(43001.01, include_time = TRUE)` returns the POSIXlt value `"2017-09-23 00:14:24"`.  Thanks to **@billdenney**.

## Breaking changes

As part of `excel_numeric_to_date()` now handling times, if a Date-only result is requested (the default behavior of `include_time = FALSE`), any fractional part of the date is now removed.  The printed date itself is identical, but the internal representation of this object now contains only the integer part of the date.  For example, while under both the old and new versions of this function the call `excel_numeric_to_date_old(42001.1)` would return the Date object `"2014-12-28"`, calling `as.numeric` on this Date result would previously return `16432.1`, while now it returns `16432`.

This an improved behavior, as now `excel_numeric_to_date(42001.1, include_time = FALSE) == as.Date("2014-12-28")` returns TRUE, while previously it would appear to be equivalent from the printed value but this comparison would return FALSE.

# janitor 1.0.0  (2018-03-17)

## Release summary
A stable version 1.0.0, with a new `tabyl` API and with breaking changes to the output of `clean_names()`.

This builds on the original functionality of janitor, with similar-but-improved tools and significantly-changed implementation.

## Breaking changes

### A fully-overhauled `tabyl`

`tabyl()` is now a single function that can count combinations of one, two, or three variables, ala base R's `table()`.  The resulting `tabyl` data.frames can be manipulated and formatted using a family of `adorn_` functions.  See the [tabyls vignette](https://sfirke.github.io/janitor/articles/tabyls.html) for more.

The now-redundant legacy functions `crosstab()` and `adorn_crosstab()` have been deprecated, but remain in the package for now.  Existing code that relies on the version of `tabyl` present in janitor versions <= 0.3.1 will break if the `sort` argument was used, as that argument no longer exists in `tabyl` (use `dplyr::arrange()` instead).

### Improvements to `clean_names`

`clean_names()` now detects and preserves camelCase inputs, allows multiple options for case outputs of the cleaned names, and preserves whether there's space between letters and numbers.  It also transliterates accented letters and turns `#` into `"number"`.

These changes may cause old code to break. E.g., a raw column name `variableName` would now be converted to `variable_name` (or `variableName`, `VariableName`, etc. depending on your preference), where previously it would have been converted to `variablename`.

To minimize this inconvenience, there's a quick fix for compatibility: you can find-and-replace to insert the argument `case = "old_janitor"`, preserving the old behavior of `clean_names()` as of janitor version 0.3.1 (and thus not have to redo your scripts beyond that.)

No further changes are planned to `clean_names()` and its results should be stable from version 1.0.0 onward.

## Major features

- `clean_names()` transliterates accented letters, e.g., `çãüœ` becomes `cauoe` [(#120)](https://github.com/sfirke/janitor/issues/120).  Thanks to **@fernandovmacedo**.

- `clean_names()` offers multiple options for variable name styling.  In addition to `snake_case` output you can select `smallCamelCase`, `BigCamelCase`, `ALL_CAPS` and others. [(#131)](https://github.com/sfirke/janitor/issues/131).
  - Thanks to **@tazinho**, who wrote the [snakecase](https://github.com/Tazinho/snakecase/) package that janitor depends on to do this, as well as the patch to incorporate it into `clean_names()`.  And thanks to **@maelle** for proposing this feature.


- Launched the janitor documentation website: [https://sfirke.github.io/janitor](https://sfirke.github.io/janitor/).  Thanks to the [pkgdown](https://github.com/r-lib/pkgdown/) package.

- Deprecated the functions `remove_empty_rows()` and `remove_empty_cols()`, which are replaced by the single function `remove_empty()`. [(#100)](https://github.com/sfirke/janitor/issues/100)
  - To encourage transparency, `remove_empty()` prints a message if no value is supplied for the `which` argument; to suppress this, supply a value to `which`, even if it's the default `c("rows", "cols")`.


- The new `adorn_title()` function adds the name of the 2nd `tabyl` variable (i.e., the name of the column variable).  This un-tidies the data.frame but makes the result clearer to readers [(#77)](https://github.com/sfirke/janitor/issues/77)

## Minor features

- The utility function `round_half_up()` is now exported for public use.  It's an exact implementation of [https://stackoverflow.com/questions/12688717/round-up-from-5-in-r/12688836#12688836/](https://stackoverflow.com/questions/12688717/round-up-from-5-in-r/12688836#12688836/), written by **@mrdwab**.
- `tabyl` objects now print with row numbers suppressed
- `clean_names()` now retains the character `#` as `"number"` in the resulting names

## Bug fixes
- `adorn_totals("row")` handles quirky variable names in 1st column [(#118)](https://github.com/sfirke/janitor/issues/118)
- `get_dupes()` returns the correct result when a variable in the input data.frame is already called `"n"` [(#162)](https://github.com/sfirke/janitor/issues/162)

***********************

# janitor 0.3.1  (2018-01-04)

## Release summary

This is a bug-fix release with no new functionality or changes.  It fixes a bug where `adorn_crosstab()` failed if the `tibble` package was version > 1.4.

Major changes to janitor are currently in development on GitHub and will be released soon.  This is not that next big release.

***********************

# janitor 0.3.0  (2017-05-06)

## Release summary

The primary purpose of this release is to maintain accuracy given breaking changes to the dplyr package, upon which janitor is built, in dplyr version >0.6.0.  This update also contains a number of minor improvements.

**Critical: if you update the package `dplyr` to version >0.6.0, you *must* update janitor to version 0.3.0 to ensure accurate results from janitor's `tabyl()` function.**  This is due to a change in the behavior of dplyr's `_join` functions (*discussed in [#111)](https://github.com/sfirke/janitor/issues/111)*.

janitor 0.3.0 is compatible with this new version of dplyr as well as old versions of dplyr back to 0.5.0.  That is, updating janitor to 0.3.0 does not necessitate an update to dplyr >0.6.0.


## Breaking changes
- The functions `add_totals_row` and `add_totals_col` were combined into a single function, `adorn_totals()`. [(#57)](https://github.com/sfirke/janitor/issues/57).  The `add_totals_` functions are now deprecated and should not be used.
- The first argument of `adorn_crosstab()` is now "dat" instead of "crosstab" (indicating that the function can be called on any data.frame, not just a result of `crosstab()`)

## Major features

- Exported the `%>%` pipe from magrittr [(#107)](https://github.com/sfirke/janitor/issues/107).

**Deprecated the following functions:**
- `use_first_valid_of()` - use `dplyr::coalesce()` instead
- `convert_to_NA()` - use `dplyr::na_if()` instead
- `add_totals_row()` and `add_totals_col()` - replaced by the single function `adorn_totals()`

## Minor features
- `adorn_totals()` and `ns_to_percents()` can now be called on data.frames that have non-numeric columns beyond the first one (those columns will be ignored) [(#57)](https://github.com/sfirke/janitor/issues/57)
- `adorn_totals("col")` retains factor class in 1st column if 1st column in the input data.frame was a factor

## Bug fixes
- `clean_names()` now handles leading spaces [(#85)](https://github.com/sfirke/janitor/issues/85)
- `adorn_crosstab()` and `ns_to_percents()` work on a 2-column data.frame [(#89)](https://github.com/sfirke/janitor/issues/89)
- `adorn_totals()` now works on a grouped tibble [(#97)](https://github.com/sfirke/janitor/issues/97)
- Long variable names with spaces no longer break `tabyl()` and `crosstab()` [(#87)](https://github.com/sfirke/janitor/issues/87)
- An `NA_` column in the result of a `crosstab()` will appear at the last column position [(#109)](https://github.com/sfirke/janitor/issues/109)

***********************

# janitor 0.2.1 (2016-10-30)

## Bug fixes
- `tabyl()` and `crosstab()` now appear in the package manual [(#65)](https://github.com/sfirke/janitor/issues/65)
- Fixed minor bug per CRAN request - `tabyl()` and `crosstab()` failed to retain ill-formatted variable names only when using R 3.2.5 for Windows [(#76)](https://github.com/sfirke/janitor/issues/76)
- `add_totals_row()` works on two-column data.frame [(#69)](https://github.com/sfirke/janitor/issues/69)
- `use_first_valid_of()` returns POSIXct-class result when given POSIXct inputs

***********************

# janitor 0.2.0 (2016-10-03)

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

# janitor 0.1 (2016-04-17)

- Initial draft of skeleton package on GitHub
