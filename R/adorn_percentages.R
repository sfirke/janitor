#' @title Convert a data.frame of counts to percentages.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable.  If the input data.frame is not a \code{tabyl}, it will convert to one in order to preserve the underlying values in the \code{core} attribute.
#'
#' @param dat a \code{tabyl} or other data.frame with a tabyl-like layout.
#' @param denominator the direction to use for calculating percentages.  One of "row", "col", or "all".
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' 
#' @return Returns a data.frame of percentages, expressed as numeric values between 0 and 1.
#' @export
#' @examples
#' 
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages("col")
#'   
#' # calculates correctly even with totals column and/or row:
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_totals("row") %>%
#'   adorn_percentages()
  
adorn_percentages <- function(dat, denominator = "row", na.rm = TRUE){
  # catch bad inputs
  if(! denominator %in% c("row", "col", "all")){stop("'denominator' must be one of 'row', 'col', or 'all'")}
  numeric_cols <- which(unlist(lapply(dat, is.numeric)))
  numeric_cols <- setdiff(numeric_cols, 1) # assume 1st column should not be included so remove it from numeric_cols. Moved up to this line so that if only 1st col is numeric, the function errors
  cols_to_tally <- numeric_cols

  if(! "tabyl" %in% class(dat)){ dat <- as_tabyl(dat)}
  if("col" %in% attr(dat, "totals")){ cols_to_tally <- numeric_cols[-length(numeric_cols)] } # if there's a totals col, don't use it to calculate the %s

  if(denominator == "row"){
    # if row-wise percentages and a totals column, need to exempt totals col and make it all 1s
    if("col" %in% attr(dat, "totals")){
      dat[[ncol(dat)]] <- rep(1, nrow(dat))
    }
    row_sum <- rowSums(dat[cols_to_tally], na.rm = na.rm)
    dat[, cols_to_tally] <- dat[cols_to_tally] / row_sum 
  } else if(denominator == "col"){
    # if col-wise percentages and a row column, need to exempt totals row and make it all 1s
    if("row" %in% attr(dat, "totals")){
      col_sum <- colSums(dat[-nrow(dat), ][numeric_cols], na.rm = na.rm)
    } else {
      col_sum <- colSums(dat[numeric_cols], na.rm = na.rm)
    }
    dat[numeric_cols] <- sweep(dat[numeric_cols], 2, col_sum,`/`) # from http://stackoverflow.com/questions/9447801/dividing-columns-by-colsums-in-r
    
  } else if(denominator == "all"){
    # if all-wise percentages, need to exempt any totals col or row
    if("row" %in% attr(dat, "totals")){
      complete_n <- sum(dat[-nrow(dat), cols_to_tally], na.rm = TRUE)
    } else {
      complete_n <- sum(dat[, cols_to_tally], na.rm = TRUE)
    }
    dat[numeric_cols] <- dat[numeric_cols] / complete_n 
  }
 
  dat 
}

