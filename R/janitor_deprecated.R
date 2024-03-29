#' Deprecated Functions in Package janitor
#'
#' These functions have already become defunct or may be defunct as soon as the next release.
#'
#' * [adorn_crosstab()] -> `adorn_`
#' * [crosstab()] -> [tabyl()]
#' * [use_first_valid_of()] -> [dplyr::coalesce()]
#' * [convert_to_NA()] -> [dplyr::na_if()]
#' * [add_totals_col()] -> [`adorn_totals(where = "col")`][adorn_totals()]
#' * [add_totals_row()] -> [adorn_totals()]
#' * [remove_empty_rows()] -> [`remove_empty("rows")`][remove_empty()]
#' * [remove_empty_cols()] -> [`remove_empty("cols")`][remove_empty()]
#'
#' @name janitor_deprecated
#' @keywords internal
# EXCLUDE COVERAGE START
NULL




#' @title Generate a crosstabulation of two vectors.
#' @param ... arguments
#' @keywords internal
#' @description
#' This function is deprecated, use [`tabyl(dat, var1, var2)`][tabyl()] instead.
#' @export

crosstab <- function(...) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::crosstab()",
    with = "tabyl()",
    details = "See the guide to tabyl(): https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html"
  )
}

#' @title Add presentation formatting to a crosstabulation table.
#' @description
#' This function is deprecated, use [tabyl()] with the `adorn_` family of functions instead.
#' @param dat a data.frame with row names in the first column and numeric values in all other columns.  Usually the piped-in result of a call to  `crosstab` that included the argument `percent = "none"`.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param show_n should counts be displayed alongside the percentages?
#' @param digits how many digits should be displayed after the decimal point?
#' @param show_totals display a totals summary? Will be a row, column, or both depending on the value of `denom`.
#' @param rounding method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @return Returns a data.frame.
#' @keywords internal
#' @export

adorn_crosstab <- function(dat, denom = "row", show_n = TRUE, digits = 1, show_totals = FALSE, rounding = "half to even") {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::adorn_crosstab()",
    with = "tabyl()",
    details = "See the adorn_* functions for formatting a tabyl: https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html"
  )
}

#' @title Append a totals row to a data.frame.
#'
#' @description
#' This function is deprecated, use [adorn_totals()] instead.
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param fill if there are more than one non-numeric columns, what string should fill the bottom row of those columns?
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals row, consisting of "Total" in the first column and column sums in the others.
#' @keywords internal
#' @export
add_totals_row <- function(dat, fill = "-", na.rm = TRUE) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::add_totals_row()",
    with = "adorn_totals()",
    details = "See the adorn_* functions for formatting a tabyl or data.frame: https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html"
  )
}

#' @title Append a totals column to a data.frame.
#'
#' @description
#' This function is deprecated, use [`adorn_totals(where = "col")`][adorn_totals()] instead.
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @keywords internal
#' @return Returns a data.frame with a totals column containing row-wise sums.
#' @export

add_totals_col <- function(dat, na.rm = TRUE) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::add_totals_cols()",
    with = "adorn_totals()",
    details = "See the adorn_* functions for formatting a tabyl or data.frame: https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html"
  )
}


#' @title Returns first non-`NA` value from a set of vectors.
#'
#' @description
#' Warning: Deprecated, do not use in new code. Use [dplyr::coalesce()] instead.
#'
#' At each position of the input vectors, iterates through in order and returns the first non-NA value.
#' This is a robust replacement of the common `ifelse(!is.na(x), x, ifelse(!is.na(y), y, z))`.
#' It's more readable and handles problems like [ifelse()]'s inability to work with dates in this way.
#'
#' @param ... the input vectors.  Order matters: these are searched and prioritized in the order they are supplied.
#' @param if_all_NA what value should be used when all of the vectors return `NA` for a certain index?  Default is `NA`.
#' @return Returns a single vector with the selected values.
#' @seealso janitor_deprecated
#' @export
#' @keywords internal
use_first_valid_of <- function(..., if_all_NA = NA) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::use_first_valid_of()",
    with = "dplyr::coalesce()"
  )
}

#' @title Convert string values to true `NA` values.
#'
#' @description
#' Warning: Deprecated, do not use in new code. Use [dplyr::na_if()] instead.
#'
#' Converts instances of user-specified strings into `NA`.  Can operate on either a single vector or an entire data.frame.
#'
#' @param dat vector or data.frame to operate on.
#' @param strings character vector of strings to convert.
#' @return Returns a cleaned object.  Can be a vector, data.frame, or `tibble::tbl_df` depending on the provided input.
#' @seealso janitor_deprecated
#' @export
#' @keywords internal
#'
convert_to_NA <- function(dat, strings) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::convert_to_NA()",
    with = "dplyr::na_if()"
  )
}


### Deprecated separate remove row/col functions

#' @title Removes empty rows from a data.frame.
#'
#' @description
#' This function is deprecated, use [`remove_empty("rows")`][remove_empty()] instead.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty rows.
#' @examples
#' # not run:
#' # dat %>% remove_empty_rows
#' @export
#' @keywords internal

remove_empty_rows <- function(dat) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::remove_empty_rows()",
    with = "janitor::remove_empty()"
  )
}

#' @title Removes empty columns from a data.frame.
#'
#' @description
#' This function is deprecated, use [`remove_empty("cols")`][remove_empty()] instead.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty columns.
#' @export
#' @keywords internal

remove_empty_cols <- function(dat) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "janitor::remove_empty_cols()",
    with = "janitor::remove_empty()"
  )
}

# EXCLUDE COVERAGE END
