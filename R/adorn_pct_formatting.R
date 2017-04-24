#' @title Format a data.frame of decimals as percentages.
#'
#' @description
#' Numeric columns get multiplied by 100 and formatted as percentages according to user specifications.  This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable.  Other non-numeric columns are also excluded.
#'
#' @param dat a data.frame with decimal values, typically the result of a call to \code{adorn_percentages} on a \code{tabyl}.
#' @param digits
#' @param method 
#' @param symbol
#' 
#' @return 
#' @export
#' @examples
#' 
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_percentages("col") %>%
#'   adorn_pct_formatting()

adorn_pct_formatting <- function(dat, digits = 1, method = "half to even", symbol = TRUE){
 
   
}