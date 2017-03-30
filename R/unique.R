#' @title Simple assertion for jointly unique variable
#'
#' @description \code{check_unique} will check that variables are jointly unique and return TRUE/FALSE, 
#' along with a helpful message. \code{assert_unique} will stop if there are any duplicate rows.
#' 
#' @param df the input data.frame
#' @param ... unquoted variable names to check for joint uniqueness. Allows for non-standard evaluation
#' @seealso \code{\link{get_dupes}} which allows you to interactively extract duplicates when an assertion fails
#' @examples
#' library(dplyr)
#' # Month and day should be jointly unique in the airquality dataset
#' airquality %>% check_unique(Month, Day)
#'  
#' # As part of an analysis pipeline
#' airquality %>%
#'     assert_unique(Month, Day) %>%
#'     summarise(ave_temp = mean(Temp))
#'     
#' # Example of message on false:
#' airquality %>% check_unique(Month)
#'     
#' # A very common workflow is:
#' # 1. Read data in and assert that it has the right unique IDs
#' # 2. Join and assert that the join result has the right unique IDs
#' 
#' reporters_str <- 'reporter_id,first_name,last_name\n1,"John","Smith"\n2,"Patty","Johnson"'
#' articles_str <- 'article_id,reporter_id,title\n1,1,"First article by John"\n2,1,"Second article by John"\n3,2,"First article by Patty"'
#' 
#' reporters <- readr::read_csv(reporters_str) %>% assert_unique(reporter_id)
#' articles <- readr::read_csv(articles_str) %>% assert_unique(article_id)
#' 
#' # Join reporter names into the articles data.frame, 
#' # then check that you understood the relationship correctly and didn't create duplicates
#' articles_reporterinfo <- inner_join(articles, reporters, by = "reporter_id") %>%
#'     assert_unique(article_id)
#' 
#' @export
assert_unique <- function(df, ...){
  call_str <- deparse(sys.call(1L), nlines = 1L)
  vars <- lazyeval::lazy_dots(...)
  if (length(vars)==0L){
    target <- df
  }
  else{
    target <- dplyr::select_(df, .dots=vars)
  }
  
  dups <- duplicated(target)
  
  num_dups <- sum(dups)
  
  if (sum(num_dups)==0) return(invisible(df))
  
  # Format message components
  max_length <- 75
  max_examples <- 5
  index <- 1:nrow(target)
  examples <- index[dups]
  examples_str <- ifelse(length(examples) > max_examples, 
                         paste0(examples[1:5], collapse = ", "),
                         paste0(examples, collapse = ", "))
  
  # Get the data frame name
  df_name <- paste(substitute(df))
  if (df_name=="."){
    i <- 1
    while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
      i <- i+1
    }
    df_name <- deparse(parent.frame(i)$lhs)
  }
  
  
  if (nchar(call_str)>max_length){
    call_str <- paste0(substring(call_str, 1, max_length), "..")
  } 
  
  msg <- sprintf("Unique constraint is violated %d times in %s. Example rows: c(%s)", 
                 num_dups, df_name, examples_str)
  
  stop(simpleError(msg, call=call_str))  
}

#' @export
#' @rdname assert_unique
check_unique <- function(df, ...){
  call_str <- deparse(sys.call(1L), nlines = 1L)
  vars <- lazyeval::lazy_dots(...)
  if (length(vars)==0L){
    target <- df
  }
  else{
    target <- dplyr::select_(df,.dots=vars)
  }
  
  dups <- duplicated(target)
  
  num_dups <- sum(dups)
  
  if (sum(num_dups)==0) return(TRUE)
  
  # Format message components
  max_length <- 75
  max_examples <- 5
  index <- 1:nrow(target)
  examples <- index[dups]
  examples_str <- ifelse(length(examples) > max_examples, 
                         paste0(examples[1:5], collapse = ", "),
                         paste0(examples, collapse = ", "))
  
  # Get the data frame name
  df_name <- paste(substitute(df))
  if (df_name=="."){
    i <- 1
    while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
      i <- i+1
    }
    df_name <- deparse(parent.frame(i)$lhs)
  }
  
  
  if (nchar(call_str)>max_length){
    call_str <- paste0(substring(call_str, 1, max_length), "..")
  } 
  
  msg <- sprintf("Unique constraint is violated %d times in %s. Example rows: c(%s)", 
                 num_dups, df_name, examples_str)
  
  message(msg)
  return(FALSE)  
}
