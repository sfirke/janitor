#' @title Append a totals row or column to a data.frame.
#'
#' @description
#' Both functions exclude the first column of the input data.frame, assuming that it contains a descriptive variable not to be summed.
#'
#' @param dat an input data.frame with numeric 
#' @return Returns a data.frame.
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   add_totals_row %>%
#'   add_totals_col


add_totals_row <- function(dat){
  dat[[1]] <- as.character(dat[[1]]) # for binding to the "Total" character value of add-on row
  col_totals <- data.frame(x1 = "Total", t(colSums(dat[,-1])), stringsAsFactors = FALSE) %>%
    stats::setNames(names(dat))
  dplyr::bind_rows(dat, col_totals)
}

add_totals_col <- function(dat){
  row_totals <- data.frame(Total = rowSums(dat[,-1]))
  dplyr::bind_cols(dat, row_totals)
}

