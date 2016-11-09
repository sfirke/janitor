#' @export
get_dupes <- function(df, ...){
  vars <- lazyeval::lazy_dots(...)
  target <- df %>% select_(.dots = vars)
  dup_index <- duplicated(target) | duplicated(target, fromLast = TRUE)
  
  df[dup_index, ]
}
