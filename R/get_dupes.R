#' @importFrom magrittr "%>%"
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
  dupes <- dat %>%
    dplyr::group_by(...) %>%
    dplyr::filter(n() > 1)  %>%
    dplyr::mutate(dupe_count = n()) %>%
    dplyr::select(..., dupe_count) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(...)

  var_names <- sapply(as.list(substitute(list(...)))[-1L], deparse)

  if(nrow(dupes) == 0){return(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))}
  dupes
}