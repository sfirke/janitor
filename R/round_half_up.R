#' @title Round a numeric vector; ties will be rounded up, ala Microsoft Excel. 
#'
#' @description
#' In base R \code{round()}, halves are rounded to even, e.g., 12.5 and 11.5 are both rounded to 12.  This function rounds 12.5 to 13 (assuming \code{digits = 0}).  Negative halves are rounded away from zero, e.g., -0.5 is rounded to -1.
#' 
#' This may skew subsequent statistical analysis of the data, but may be desirable in certain contexts.  This function is implemented exactly from \url{http://stackoverflow.com/a/12688836}; see that question and comments for discussion of this issue.
#'
#' @param x a numeric vector to round.
#' @param digits how many digits should be displayed after the decimal point?
#' @export
#' @examples
#' round_half_up(12.5, 0)
#' round_half_up(1.125, 2)
#' round_half_up(1.125, 1)
#' round_half_up(-0.5, 0) # negatives get rounded away from zero
#' 
round_half_up <- function(x, digits){
  posneg = sign(x)
  z = abs(x) * 10 ^ digits
  z = z + 0.5
  z = trunc(z)
  z = z / 10 ^ digits
  z * posneg
}

