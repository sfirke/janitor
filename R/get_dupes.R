#' @title Get rows of a \code{data.frame} with identical values for the specified variables.
#'
#' @description
#' For hunting duplicate records during data cleaning.  Specify the data.frame and the variable combination to search for duplicates and get back the duplicated rows.
#'
#' @param dat The input data.frame.
#' @param ... Unquoted variable names to search for duplicates. This takes a tidyselect specification.
#' @return Returns a data.frame with the full records where the specified variables have duplicated values, as well as a variable \code{dupe_count} showing the number of rows sharing that combination of duplicated values. If the input data.frame was of class \code{tbl_df}, the output is as well. 
#' @export
#' @examples
#' get_dupes(mtcars, mpg, hp)
#' 
#' # or called with the magrittr pipe %>% :
#' mtcars %>% get_dupes(wt)
#' 
#' # You can use tidyselect helpers to specify variables:
#' mtcars %>% get_dupes(-c(wt, qsec))
#' mtcars %>% get_dupes(starts_with("cy"))
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr dots_n syms
get_dupes <- function(dat, ...) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = dat)

  #Check if dat is grouped and if so, save structure and ungroup temporarily
  is_grouped <- dplyr::is_grouped_df(dat)
  
  if(is_grouped) {
    dat_groups <- dplyr::group_vars(dat)
    dat <- dat %>% dplyr::ungroup()
    if(getOption("get_dupes.grouped_warning",TRUE) & interactive()) {
      message(paste0("Data is grouped by [", paste(dat_groups, collapse = "|"), "]. Note that get_dupes() is not group aware and does not limit duplicate detection to within-groups, but rather checks over the entire data frame. However grouping structure is preserved.\nThis message is shown once per session and may be disabled by setting options(\"get_dupes.grouped_warning\" = FALSE).")) #nocov
      options("get_dupes.grouped_warning" = FALSE) #nocov
    }
  }
  
  if (rlang::dots_n(...) == 0) { # if no tidyselect variables are specified, check the whole data.frame
    var_names <- names(dat)
    nms <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    var_names <- names(pos)
    nms <- rlang::syms(var_names)
  }
  
  dupe_count <- NULL # to appease NOTE for CRAN; does nothing.
  
  
  dupes <- dat %>%
    dplyr::add_count(!!! nms, name = "dupe_count") %>%
    dplyr::filter(dupe_count > 1) %>%
    dplyr::select(!!! nms, dupe_count, dplyr::everything()) %>%
    dplyr::arrange(dplyr::desc(dupe_count), !!! nms)
  
  # shorten error message for large data.frames
  if (length(var_names) > 10) {
    var_names <- c(var_names[1:9], paste("... and", length(var_names) - 9, "other variables"))
  }
  if (nrow(dupes) == 0) {
    message(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))
  }

  #Reapply groups if dat was grouped
  if(is_grouped) dupes <- dupes %>% dplyr::group_by(!!!rlang::syms(dat_groups))
  
  return(dupes)
}


