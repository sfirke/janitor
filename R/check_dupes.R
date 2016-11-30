#' @title Check if there are duplicates in the supplied columns
#'
#' @description
#' Check whether the supplied columns have duplicates and, 
#' if there are duplicates, handle at the supplied level.
#'
#' @param df the input data frame
#' @param ...  unquoted variable names to search for duplicates
#' @param level What should happen if there are duplicates. Can be "stop", "warning", "message" or "logical". Logical returns TRUE or FALSE. 
#' @return If level == "logical", TRUE/FALSE, otherwise the data frame is returned silently for further piping
#' @export
#' @import dplyr
#' @examples
#' # Make sure no duplicate cars were added since the last time we loaded the data
#' mtcars %>%
#'  mutate(make = row.names(.)) %>%
#'  dups_check(make)
#'
#' # Check whether any Species have the same average Sepal.Width
#' iris %>%
#'  group_by(Species) %>%
#'  summarise(ave_sepal_width = mean(Sepal.Width)) %>%
#'  dups_check(ave_sepal_width, level="warning") %>%
#'  select(Species) # Silly example of pipe-ability

check_dupes <- function(df, ..., level="stop"){
  
  # Note: Would be nice to be able to set the default level once...
  levels <- c("stop", "warning", "message", "logical")
  
  if (!(level %in% levels)){
    stop(sprintf("level must be one of %s", paste0(levels, collapse = ", ")))
  }
  
  vars <- lazyeval::lazy_dots(...)
  if (length(vars)>0){
    target <- dplyr::select_(df, .dots=vars)
  }
  else{
    target <- df
  }
  num_dups <- sum(duplicated(target))
  
  dup_message <- sprintf("There are %d duplicates", num_dups) # Would be nice to parse the df name instead of gettting "." when piped
  
  if (level == "logical"){
    return(num_dups == 0) # Logical returns true/false only
  }
  else if (num_dups == 0){
    return(invisible(df))
  }
  else if (level == "message"){
    message(dup_message)
  }
  else if (level == "warning"){
    warning(dup_message)
  }
  else if (level == "stop"){
    stop(dup_message)
  }
  invisible(df)
}
