#' @title Append a totals row to a data.frame.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable not to be summed.
#'
#' @param dat an input data.frame with numeric values in all columns beyond the first.
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals row, consisting of "Total" in the first column and column sums in the others.
#' @export
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_totals_row


adorn_total_row <- function(dat, na.rm = TRUE){
  check_all_cols_after_first_are_numeric(dat)
  dat[[1]] <- as.character(dat[[1]]) # for binding to the "Total" character value of add-on row
  col_totals <- data.frame(x1 = "Total", t(colSums(dat[-1], na.rm = na.rm)), stringsAsFactors = FALSE) %>%
    stats::setNames(names(dat))
  dplyr::bind_rows(dat, col_totals)
}

#' Deprecated; use adorn_total_row
#' @export
add_totals_row <- function(...){
  message("add_totals_row will be deprecated; use adorn_total_row")
  adorn_total_row(...)
}

#' @title Append a totals column to a data.frame.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable not to be summed.
#'
#' @param dat an input data.frame with numeric values in all columns beyond the first.
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals column, consisting of "Total" in the first row and row sums in the others.
#' @export
#' @examples
#' library(dplyr) # for the %>% pipe
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_total_row

adorn_total_col <- function(dat, na.rm = TRUE){
  check_all_cols_after_first_are_numeric(dat)
  row_totals <- data.frame(Total = rowSums(dat[-1], na.rm = na.rm))
  dplyr::bind_cols(dat, row_totals)
}

#' Deprecated; use adorn_total_col
#' @export
add_totals_col <- function(...){
  message("add_totals_col will be deprecated; use adorn_total_col")
  adorn_total_col(...)
}

