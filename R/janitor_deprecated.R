#' Deprecated Functions in Package janitor
#'
#' These functions have already become defunct or may be defunct as soon as the next release.
#'
#' \itemize{
#'   \item \code{\link{adorn_crosstab}}
#'   \item \code{\link{crosstab}}
#'   \item \code{\link{use_first_valid_of}}
#'   \item \code{\link{convert_to_NA}}
#' }
#'
#' @name janitor_deprecated
# EXCLUDE COVERAGE START
NULL




#' @title Generate a crosstabulation of two vectors.
#'
#' @description
#' This function is deprecated, use \code{tabyl(dat, var1, var2)} instead.
#' @export

crosstab <- function(...) {
  lifecycle::deprecate_stop(when = "2.0.0",
                            what = "janitor::crosstab()",
                            with = "tabyl()",
                            details = "See the guide to tabyl(): https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html")
}

#' @title Add presentation formatting to a crosstabulation table.
#'
#' @description
#' This function is deprecated, use the \code{adorn_} family of functions instead.
#' @param dat a data.frame with row names in the first column and numeric values in all other columns.  Usually the piped-in result of a call to  \code{crosstab} that included the argument \code{percent = "none"}.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param show_n should counts be displayed alongside the percentages?
#' @param digits how many digits should be displayed after the decimal point?
#' @param show_totals display a totals summary? Will be a row, column, or both depending on the value of \code{denom}.
#' @param rounding method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @return Returns a data.frame.
#' @export

adorn_crosstab <- function(dat, denom = "row", show_n = TRUE, digits = 1, show_totals = FALSE, rounding = "half to even") {
  lifecycle::deprecate_stop(when = "2.0.0",
                            what = "janitor::adorn_crosstab()",
                            with = "tabyl()",
                            details = "See the adorn_* functions for formatting a tabyl: https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html")
}
# EXCLUDE COVERAGE END