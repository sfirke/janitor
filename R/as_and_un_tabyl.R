#' @title Add \code{tabyl} attributes to a data.frame.
#'
#' @description
#' A \code{tabyl} is a data.frame containing counts of a variable or co-occurences of two variables (a.k.a., a contingency table or crosstab).  This specialized kind of data.frame has attributes that enable \code{adorn_} functions to be called for precise formatting and presentation of results.  E.g., display results as a mix of percentages, Ns, add totals rows or columns, rounding options, in the style of Microsoft Excel PivotTable.
#' 
#' A \code{tabyl} can be the result of a call to \code{janitor::tabyl()}, in which case the attributes are added automatically.  This function adds \code{tabyl} class attributes to a data.frame that isn't the result of a call to \code{tabyl} but meets the requirements of a tabyl:
#' 1) First column contains values of variable 1
#' 2) Column names 2:n are the values of variable 2
#' 3) Numeric values in columns 2:n are counts of the co-occurences of the two variables.*
#' 
#' * = this is the ideal form of a tabyl, but janitor's \code{adorn_} functions tolerate and ignore non-numeric columns in positions 2:n. 
#' 
#' For instance, the result of \code{dplyr::count()} followed by \code{tidyr::spread()} can be treated as a \code{tabyl}.
#' 
#' The result of calling \code{tabyl()} on a single variable is a special class of one-way tabyl; this function only pertains to the two-way tabyl.
#'
#' @param dat a data.frame with variable values in the first column and numeric values in all other columns.
#' @return Returns the same data.frame, but with the additional class of "tabyl" and the attribute "core".
#' @export
#' @examples
#' as_tabyl(mtcars)
#' 

as_tabyl <- function(dat){
  if("tabyl" %in% class(dat)){ return(dat) }
  
  # check whether input meets requirements
  if(!is.data.frame(dat)){stop("input must be a data.frame")}
  if(sum(unlist(lapply(dat, is.numeric))[-1]) == 0){stop("at least one one of columns 2:n must be of class numeric")}
  
  # assign core attribute and classes
  attr(dat, "core") <- as.data.frame(dat)
  attr(dat, "tabyl_type") <- "two_way"
  class(dat) <- c(class(dat), "tabyl")
  dat
}

#' @title Remove \code{tabyl} attributes to a data.frame.
#'
#' @description
#' Strips away all \code{tabyl}-related attributes from a data.frame.
#'
#' @param dat a data.frame of class \code{tabyl}.
#' @return Returns the same data.frame, but without the \code{tabyl} class and attributes.
#' @export
#' @examples
#' mtcars %>%
#'   as_tabyl() %>%
#'   un_tabyl()

un_tabyl <- function(dat){
  if(! "tabyl" %in% class(dat)){warning("un_tabyl() called on a non-tabyl")}
  class(dat) <- class(dat)[! class(dat) %in% "tabyl"]
  attr(dat, "core") <- NULL
  attr(dat, "totals") <- NULL # may not exist, but simpler to declare it NULL regardless than to check to see if it exists
  attr(dat, "tabyl_type") <- NULL # may not exist, but simpler to declare it NULL regardless than to check to see if it exists
  dat
}