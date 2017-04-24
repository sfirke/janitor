#' @title Round the numeric columns in a data.frame.
#'
#' @description
#' This function defaults to excluding the first column of the input data.frame, assuming that it contains a descriptive variable.  Designed to run on a \code{tabyl} data.frame, but can run on any data.frame with at least one numeric column.
#'
#' @param dat a \code{tabyl} or other data.frame with a tabyl-like layout.
#' @param digits how many digits should be displayed after the decimal point?
#' @param method method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @param skip_first_col should the first column be left un-rounded, assuming it contains values of a descriptive variable as in a \code{tabyl}?  Defaults to \code{TRUE}.
#' 
#' @return Returns the data.frame with rounded numeric columns.
#' @export
#' @examples
#' 
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_percentages() %>%
#'   adorn_rounding(digits = 2, method = "half up)
#'   
#' # tolerates non-numeric columns:
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_percentages("all") %>%
#'   mutate(dummy = "a") %>%
#'   adorn_rounding()


adorn_rounding <- function(dat, digits = 1, method = "half to even", skip_first_col = TRUE){
  if(! method %in% c("half to even", "half up")){stop("'method' must be one of 'half to even' or 'half up'")}
  numeric_cols <- which(unlist(lapply(dat, is.numeric)))
  if(skip_first_col){ numeric_cols <- setdiff(numeric_cols, 1) }
  
  if(method == "half to even"){
    dat[, numeric_cols] <- lapply(dat[, numeric_cols], function(x) round_half_up(x, digits = digits))
  } else {
    dat[, numeric_cols] <- lapply(dat[, numeric_cols], function(x) round(x, digits = digits))
  }
  dat
}