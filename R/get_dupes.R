#' @title Get rows of a \code{data.frame} with identical values for the specified variables.
#'
#' @description
#' For hunting duplicate records during data cleaning.  Specify the data.frame and the variable combination to search for duplicates and get back the duplicated rows.
#'
#' @param dat The input data.frame.
#' @param ... Unquoted variable names to search for duplicates. This takes a tidyselect specification.
#' @return Returns a data.frame (actually a \code{tbl_df}) with the full records where the specified variables have duplicated values, as well as a variable \code{dupe_count} showing the number of rows sharing that combination of duplicated values.
#' @export
#' @examples
#' get_dupes(mtcars, mpg, hp)
#' 
#' # or called with the magrittr pipe %>% :
#' mtcars %>% get_dupes(wt)
#' 
#' # You can use tidyselect helpers to specify variables:
#' mtcars %>% get_dupes(weight = wt, starts_with("cy"))
#'

get_dupes <- function(dat, ...) {
  
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = dat)
  
  names(dat)[pos] <- names(pos) #allows for renaming within get_dupes() consistent with select()
  
  if (rlang::dots_n(...) == 0) { # if no tidyselect variables are specified, check the whole data.frame
    var_names <- names(dat)
    nms <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    var_names <- names(pos)
    nms <- rlang::syms(var_names)
  }
  
  dupe_count <- NULL # to appease NOTE for CRAN; does nothing.
  
  # calculate counts to join back to main df
  counts <- dat %>%
    dplyr::count(!!! nms, name = "dupe_count")
  
  # join new count vector to main data.frame
  dupes <- suppressMessages(dplyr::inner_join(counts, dat))
  
  dupes <- dupes %>%
    dplyr::filter(dupe_count > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!! nms)
  
  # shorten error message for large data.frames
  if (length(var_names) > 10) {
    var_names <- c(var_names[1:9], paste("... and", length(var_names) - 9, "other variables"))
  }
  if (nrow(dupes) == 0) {
    message(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))
  }
  dupes
}


