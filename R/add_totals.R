#' @title Append a totals row and/or column to a data.frame.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming it's a descriptive variable not to be summed.  It also excludes other non-numeric columns.
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param which one of "row", "col", or \code{c("row", "col")} 
#' @param fill if there are multiple non-numeric columns, what string should fill the bottom row of those columns?
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame augmented with a totals row, column, or both.
#' @export
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   add_totals()


add_totals <- function(dat, which = c("row", "col"), fill = "-", na.rm = TRUE){
  if("grouped_df" %in% class(dat)){ dat <- dplyr::ungroup(dat) } # grouped_df causes problems, #97
  
  dat[[1]] <- as.character(dat[[1]]) # for type matching when binding the word "Total" on a factor.  Moved up to this line so that if only 1st col is numeric, the function errors
  if(sum(unlist(lapply(dat, is.numeric))) == 0){stop("at least one one of columns 2:n must be of class numeric")}
  
  if("row" %in% which){
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
    dat <- dplyr::bind_rows(dat %>%
                              stats::setNames(names(dat)) %>%
                              dplyr::mutate_at(1, as.character), col_totals)
  }
  
  if("col" %in% which){
    # Add totals col
    clean_dat <- clean_names(dat) # bad names will make select_if choke
    row_totals <- clean_dat %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::transmute(Total = rowSums(., na.rm = na.rm))
    
    dat$Total <- row_totals$Total
  }
  
  dat
}

