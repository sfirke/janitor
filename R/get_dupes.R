#' @title Filter a df to all duplicated observations on the supplied columns
#'
#' @description
#' Filter a dataframe to all instances of on duplicates on
#' the supplied columns, such that you can \code{View()} the result
#' or take any other steps necessary to resolve the duplicates. 
#' 
#' This differs from \code{duplicated()} in that \code{duplicated()} 
#' returns one row per duplicate, while \code{get_dupes()} returns all
#' duplicates, and also all columns.
#'
#' @param df the input data frame
#' @param ...  unquoted variable names to search for duplicates
#' @return A data frame filter to only duplicate observations
#' @export
#' @examples
#' dupe_df <- data.frame(a = c(1,3,3,3,5), b = c("a", "c","c", "e", "c"))
#' get_dupes(dupe_df)
#' get_dupes(dupe_df, a)

get_dupes <- function(df, ...){
  vars <- lazyeval::lazy_dots(...)
  if (length(vars)>0){
    target <- dplyr::select_(df, .dots=vars)
  }
  else{
    target <- df
  }

  dup_index <- duplicated(target) | duplicated(target, fromLast = TRUE)
  
  df[dup_index, ]
}
