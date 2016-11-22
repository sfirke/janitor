#' @import dplyr
# TODO: write documentation, write tests
dups_count <- function(df, ..., sort = FALSE){
  if (length(vars)>0){
    vars <- lazyeval::lazy_dots(...)
  }
  else{
    vars <- names(df)
  }
  
  df %>% count_(vars, sort = sort) %>%
    filter(n>1) %>%
    rename(.duplicates=n)
}