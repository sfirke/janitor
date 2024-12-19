#' Deprecated Functions in Package janitor
#'
#' These functions have already become defunct or may be defunct as soon as the next release.
#'
#' * [use_first_valid_of()] -> [dplyr::coalesce()]
#' * [convert_to_NA()] -> [dplyr::na_if()]
#' * [remove_empty_rows()] -> [`remove_empty("rows")`][remove_empty()]
#' * [remove_empty_cols()] -> [`remove_empty("cols")`][remove_empty()]
#'
#' @name janitor_deprecated
#' @keywords internal
# EXCLUDE COVERAGE START
NULL

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
