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
  df_name <- deparse(substitute(dat))
  
  if(length(names)==0){
    names <- names(dat)
    message("No variable names specified - using all columns.\n")
  }
  
 # check that each variable name provided is present in names(dat); if not, throw error
  var_names <- names
  if(is.list(var_names)){ var_names <- lapply(names, deparse) } # 'names' is not a list if defaulting to whole df, need this for consistency
  check_vars_in_df(dat, df_name, unlist(var_names))
  
  dupe_count <- NULL # to appease NOTE for CRAN; does nothing.
  
  # calculate counts to join back to main df
  counts <- dat %>%
    dplyr::count_(vars = names)

  # join new count vector to main data.frame
  dupes <- suppressMessages(dplyr::inner_join(counts, dat))
  
  dupes <- dupes %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange_(.dots = names) %>%
    dplyr::rename(dupe_count = n)
  
  if(nrow(dupes) == 0){message(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))}
  dupes
}

# takes a data.frame and vector of variable names, confirms that all var names match data.frame names 
check_vars_in_df <- function(dat, dat_name, names_vec){
  if(is.list(names_vec)){ names_vec <- lapply(names_vec, deparse)}
  in_df <- unlist(lapply(names_vec, function(x) x %in% names(dat)))
  if(sum(in_df) != length(in_df)){
    stop(paste0(
      paste0("These variables do not match column names in ", dat_name, ": "),
      paste(names_vec[!in_df], collapse =", ")
    )
    )
  }
}
