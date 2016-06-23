#' @title Get rows of a \code{data.frame} with identical values for the specified variables.
#'
#' @description
#' For hunting duplicate records during data cleaning.  Specify the data.frame and the variable combination to search for duplicates and get back the duplicated rows.
#'
#' @param dat the input data.frame.
#' @param ... unquoted variable names to search for duplicates.
#' @return Returns a data.frame (actually a \code{tbl_df}) with the full records where the specified variables have duplicated values, as well as a variable \code{dupe_count} showing the number of rows sharing that combination of duplicated values.
#' @export
#' @examples
#' get_dupes(mtcars, mpg, hp)
#' # or called with magrittr pipe %>% :
#' library(dplyr)
#' mtcars %>% get_dupes(wt)
#'

get_dupes <- function(dat, ...) {
  names <- as.list(substitute(list(...)))[-1L]
  
  if(length(names)==0){
    names <- names(dat)
    message("No variable names specified - using all columns.\n")
  }
 # use lapply to check that each value of names is present in names(dat); if not, throw a rejection error
  ###########################
  
  dupe_count <- NULL # to appease NOTE for CRAN; does nothing.
  
  # calculate counts to join back to main df
  # would be nicer to use dplyr::n() but it's not exported
  counts <- dat %>%
    dplyr::count_(vars = names)

  # join new count vector to main data.frame
  dupes <- suppressMessages(dplyr::inner_join(counts, dat))
  
  dupes <- dupes %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = names) %>%
    dplyr::rename(dupe_count = n)
  
  var_names <- sapply(names, deparse)
  
  if(nrow(dupes) == 0){message(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))}
  dupes
}