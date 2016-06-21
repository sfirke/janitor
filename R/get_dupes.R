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
  
  dupe_count <- NULL # to appease NOTE for CRAN; does nothing.
  
  # fake declare the everything() function, which is not yet exported by dplyr but will be in the next release
  everything <- function() {stop("everything() is screwing up in the get_dupes function")}
  # when select is called, it will look in its own namespace and find everything() there

  # calculate counts to join back to main df
  # would be nicer to use dplyr::n() but it's not exported
  counts <- dat %>%
    dplyr::count(...)
  
  # join new count vector to main data.frame
  dupes <- suppressMessages(dplyr::inner_join(counts, dat))
  
  dupes <- dupes %>%
    dplyr::filter(n > 1)  %>%
    dplyr::select(..., dupe_count = n, everything()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(...)
  
  var_names <- sapply(as.list(substitute(list(...)))[-1L], deparse)

  if(nrow(dupes) == 0){warning(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))}
  dupes
}