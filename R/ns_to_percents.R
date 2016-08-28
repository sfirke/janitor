#' @title Convert a numeric data.frame to row-, column-, or totals-wise percentages.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable.
#'
#' @param dat a data.frame with row names in the first column and numeric values in all other columns.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame of percentages, expressed as numeric values between 0 and 1.
#' @export
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   ns_to_percents(denom = "all")
  
ns_to_percents <- function(dat, denom = "row", na.rm = TRUE){

    if(! denom %in% c("row", "col", "all")){stop("'denom' must be one of 'row', 'col', or 'all'")}
  
  complete_n <- sum(dat[, -1], na.rm = TRUE)
  n_col <- ncol(dat)
  
  if(denom == "row"){
    row_sum <- rowSums(dat[, 2:n_col], na.rm = na.rm)
    dat[, 2:n_col] <- dat[, 2:n_col] / row_sum 
  } else if(denom == "col"){
    col_sum <- colSums(dat[, 2:n_col], na.rm = na.rm)
    dat[, 2:n_col] <- sweep(dat[, 2:n_col], 2, col_sum,`/`) # from http://stackoverflow.com/questions/9447801/dividing-columns-by-colsums-in-r
  } else if(denom == "all"){
    dat[, 2:n_col] <- dat[, 2:n_col] / complete_n 
  }
 
  dat 
}

