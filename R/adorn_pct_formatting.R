#' @title Format a data.frame of decimals as percentages.
#'
#' @description
#' Numeric columns get multiplied by 100 and formatted as percentages according to user specifications.  This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable.  Other non-numeric columns are also excluded.
#'
#' @param dat a data.frame with decimal values, typically the result of a call to \code{adorn_percentages} on a \code{tabyl}.
#' @param digits how many digits should be displayed after the decimal point?
#' @param rounding method to use for rounding - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @param affix_sign should the \% sign be affixed to the end?
#' 
#' @return a data.frame with formatted percentages
#' @export
#' @examples
#' 
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_percentages("col") %>%
#'   adorn_pct_formatting()

adorn_pct_formatting <- function(dat, digits = 1, rounding = "half to even", affix_sign = TRUE){
  #TODO: validate inputs
  
  numeric_cols <- which(unlist(lapply(dat, is.numeric)))
  numeric_cols <- setdiff(numeric_cols, 1) # assume 1st column should not be included so remove it from numeric_cols
  
  dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) x * 100)
  if(rounding == "half to even"){ dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) round(x, digits)) }
  else if(rounding == "half up"){ dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) round_half_up(x, digits)) }
  dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) format(x, nsmall = digits, trim = TRUE)) # so that 0% prints as 0.0% or 0.00% etc.
  if(affix_sign){ dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) { paste0(x, "%")}) }
  dat
   
}