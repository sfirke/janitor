#' @title Append a totals row to a data.frame.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming it's a descriptive variable not to be summed.  It also excludes other non-numeric columns. 
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param fill if there are more than one non-numeric columns, what string should fill the bottom row of those columns?
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals row, consisting of "Total" in the first column and column sums in the others.
#' @export
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   add_totals_row


add_totals_row <- function(dat, fill = "-", na.rm = TRUE){
  clean_dat <- clean_names(dat) # bad names will make select_if choke
  
  if(ncol(dplyr::select_if(clean_dat, is.numeric)) == 0){stop("data.frame must contain at least one column of class numeric")}
  
  # creates the totals row to be appended
  col_vec <- function(a_col, na_rm = na.rm){
    if(is.numeric(a_col)){ # can't do this with if_else because it doesn't like the sum() of a character vector, even if that clause is not reached
      sum(a_col, na.rm = na_rm)
    } else {fill}
  }
  
  col_totals <- lapply(dat, col_vec) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(names(dat))
    
  col_totals[nrow(col_totals), 1] <- "Total" # replace final row, first column with "Total"
  dplyr::bind_rows(clean_dat %>%
                     stats::setNames(names(dat)) %>%
                     dplyr::mutate_at(1, as.character), col_totals)
  
}

#' @title Append a totals column to a data.frame.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming it's a descriptive variable not to be summed.  It also excludes other non-numeric columns. 
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals column containing row-wise sums.
#' @export
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   add_totals_col

add_totals_col <- function(dat, na.rm = TRUE){
  
  clean_dat <- clean_names(dat) # bad names will make select_if choke
  if(ncol(dplyr::select_if(clean_dat, is.numeric)) == 0){stop("data.frame must contain at least one column of class numeric")}
  row_totals <- clean_dat %>%
    dplyr::select_if(is.numeric) %>%
    dplyr::transmute(Total = rowSums(., na.rm = na.rm))
  
  dplyr::bind_cols(dat, row_totals) %>%
    stats::setNames(c(names(dat), "Total")) # put back original names
}

