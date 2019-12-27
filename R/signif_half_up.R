#' @title Round a numeric vector to the specified number of significant digits; halves will be rounded up.
#'
#' @description
#' In base R \code{signif()}, halves are rounded to even, e.g.,
#' \code{signif(11.5, 2)} and \code{signif(12.5, 2)} are both rounded to 12.
#' This function rounds 12.5 to 13 (assuming \code{digits = 2}). Negative halves
#' are rounded away from zero, e.g., \code{signif(-2.5, 1)} is rounded to -3.
#'
#' This may skew subsequent statistical analysis of the data, but may be
#' desirable in certain contexts. This function is implemented from 
#' \url{https://stackoverflow.com/a/1581007}; see that question and
#' comments for discussion of this issue.
#'
#' @param x a numeric vector to round.
#' @param digits integer indicating the number of significant digits to be used.
#' @export
#' @examples
#' signif_half_up(12.5, 2)
#' signif_half_up(1.125, 3)
#' signif_half_up(-2.5, 1) # negatives get rounded away from zero
#'
signif_half_up <- function(x, digits = 6) {
  
  xs <- which(x != 0)
  
  y <- rep(0, length(x))
  z <- rep(0, length(x))
  
  y[xs] <- 10 ^ (digits - ceiling(log10(abs(x[xs]))))
  
  z[xs] <- round_half_up(x[xs] * y[xs]) / y[xs]
  
  return(z)
  
}