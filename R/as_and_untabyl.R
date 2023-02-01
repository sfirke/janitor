#' @title Add \code{tabyl} attributes to a data.frame.
#'
#' @description
#' A \code{tabyl} is a data.frame containing counts of a variable or co-occurrences of two variables (a.k.a., a contingency table or crosstab).  This specialized kind of data.frame has attributes that enable \code{adorn_} functions to be called for precise formatting and presentation of results.  E.g., display results as a mix of percentages, Ns, add totals rows or columns, rounding options, in the style of Microsoft Excel PivotTable.
#'
#' A \code{tabyl} can be the result of a call to \code{janitor::tabyl()}, in which case these attributes are added automatically.  This function adds \code{tabyl} class attributes to a data.frame that isn't the result of a call to \code{tabyl} but meets the requirements of a two-way tabyl:
#' 1) First column contains values of variable 1
#' 2) Column names 2:n are the values of variable 2
#' 3) Numeric values in columns 2:n are counts of the co-occurrences of the two variables.*
#'
#' * = this is the ideal form of a tabyl, but janitor's \code{adorn_} functions tolerate and ignore non-numeric columns in positions 2:n.
#'
#' For instance, the result of \code{dplyr::count()} followed by \code{tidyr::spread()} can be treated as a \code{tabyl}.
#'
#' The result of calling \code{tabyl()} on a single variable is a special class of one-way tabyl; this function only pertains to the two-way tabyl.
#'
#' @param dat a data.frame with variable values in the first column and numeric values in all other columns.
#' @param axes is this a two_way tabyl or a one_way tabyl?  If this function is being called by a user, this should probably be "2".  One-way tabyls are created by \code{tabyl} but are a special case.
#' @param row_var_name (optional) the name of the variable in the row dimension; used by \code{adorn_title()}.
#' @param col_var_name (optional) the name of the variable in the column dimension; used by \code{adorn_title()}.
#' @return Returns the same data.frame, but with the additional class of "tabyl" and the attribute "core".
#' @export
#' @examples
#' as_tabyl(mtcars)
#'

as_tabyl <- function(dat, axes = 2, row_var_name = NULL, col_var_name = NULL) {
  if (!axes %in% 1:2) {
    stop("axes must be either 1 or 2")
  }
  
  # check whether input meets requirements
  if (!is.data.frame(dat)) {
    stop("input must be a data.frame")
  }
  if (sum(unlist(lapply(dat, is.numeric))[-1]) == 0) {
    stop("at least one one of columns 2:n must be of class numeric")
  }
  
  # assign core attribute and classes
  if("tabyl" %in% class(dat)){
    # if already a tabyl, may have totals row.  Safest play is to simply reorder the core rows to match the dat rows
    attr(dat, "core") <- attr(dat, "core")[order(match(attr(dat, "core")[, 1],
                                                       dat[, 1])), ]
    row.names(attr(dat, "core")) <- 1:nrow(attr(dat, "core")) # if they're sorted in the prior step above, this resets
  } else {
    attr(dat, "core") <- as.data.frame(dat) # core goes first so dat does not yet have attributes attached to it
  }
  
  attr(dat, "tabyl_type") <- ifelse(
    !is.null(attr(dat, "tabyl_type")),
    attr(dat, "tabyl_type"), # if a one_way tabyl has as_tabyl called on it, it should stay a one_way #523
    dplyr::case_when(
      axes == 1 ~ "one_way",
      axes == 2 ~ "two_way"
    ))
  class(dat) <- c("tabyl", setdiff(class(dat), "tabyl"))
  
  if (!missing(row_var_name) | !missing(col_var_name)) {
    if (axes != 2) {
      stop("variable names are only meaningful for two-way tabyls")
    }
    attr(dat, "var_names") <- list(row = row_var_name, col = col_var_name)
  }
  
  dat
}

#' @title Remove \code{tabyl} attributes from a data.frame.
#'
#' @description
#' Strips away all \code{tabyl}-related attributes from a data.frame.
#'
#' @param dat a data.frame of class \code{tabyl}.
#' @return Returns the same data.frame, but without the \code{tabyl} class and attributes.
#' @export
#' @examples
#'
#' mtcars %>%
#'   tabyl(am) %>%
#'   untabyl() %>%
#'   attributes() # tabyl-specific attributes are gone

untabyl <- function(dat) {
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    purrr::map(dat, untabyl)
  } else {
    if (!"tabyl" %in% class(dat)) {
      warning("untabyl() called on a non-tabyl")
    }
    class(dat) <- class(dat)[!class(dat) %in% "tabyl"]
    attr(dat, "core") <- NULL
    # These attributes may not exist, but simpler to declare them NULL regardless than to check to see if they exist:
    attr(dat, "totals") <- NULL
    attr(dat, "tabyl_type") <- NULL # may not exist, but simpler to declare it NULL regardless than to check to see if it exists
    attr(dat, "var_names") <- NULL # may not exist, but simpler to declare it NULL regardless than to check to see if it exists
    dat
  }
}