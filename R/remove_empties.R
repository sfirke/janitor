#' @title Removes empty rows from a data.frame.
#'
#' @description
#' Removes all rows from a data.frame that are composed entirely of \code{NA} values.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty rows.
#' @export
#' @examples
#' # not run:
#' # dat %>% remove_empty_rows

remove_empty_rows <- function(dat){
  dat[rowSums(is.na(dat)) != ncol(dat), ]
}

#' @title Removes empty columns from a data.frame.
#'
#' @description
#' Removes all columns from a data.frame that are composed entirely of \code{NA} values.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty columns.
#' @export
#' @examples
#' # not run:
#' # dat %>% remove_empty_cols
#'

remove_empty_cols <- function(dat){
  dat[colSums(!is.na(dat)) > 0]
}