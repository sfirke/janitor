#' @title Count the number of duplicates in the supplied columns
#'
#' @description
#' Summarise the number of duplicates on the supplied columns.
#'
#' @param df the input data frame
#' @param ...  unquoted variable names to search for duplicates
#' @return the data frame is returned silently for further piping
#' @export
#' @import dplyr
#' @examples
#' # Count duplicate ids
#' df <- data.frame(id = c(1,2,3,3,3), stuff = c("a", "b", "c", "d", "d"))
#' count_dupes(df)
#' count_dupes(df, id)
#'
#' # Join the number of duplicates with the duplicate observations from get_dupes
#' df <- data.frame(id = c(1,2,3,3,3), stuff = c("a", "b", "c", "d", "d"))
#' t <- df %>%
#'   get_dupes(., id) %>%
#'   left_join(count_dupes(., id))

count_dupes <- function(df, ..., sort = FALSE){
  vars <- lazyeval::lazy_dots(...)
  
  if (length(vars)==0){
    vars <- names(df)
  }

  df %>% count_(vars, sort = sort) %>%
    filter(n>1) %>%
    rename(.duplicates=n)
}
