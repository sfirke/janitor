#' @title Generate a table of a vector.
#'
#' @description
#' Get a frequency table of a variable as a data.frame, showing percentages and with or without including \code{NA} values.  A fully-featured alternative to \code{table()}.
#'
#' @param vec the vector to tabulate.
#' @param sort should the resulting table be sorted in descending order?
#' @param show_na should cases where the variable is NA be shown?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the tabulated variable.  Includes counts, percentages, and valid percentages (calculated omitting \code{NA} values, if present in the vector and \code{show_na = TRUE}.)
#' @export
#' @examples
#' tabyl(mtcars$cyl)
#' tabyl(mtcars$cyl, sort = TRUE)
#' # called with magrittr pipe:
#' library(dplyr)
#' mtcars %>% .$cyl %>% tabyl()
#' # illustrating show_na functionality:
#' my_cars <- rbind(mtcars, rep(NA, 11))
#' tabyl(my_cars$cyl)
#' tabyl(my_cars$cyl, show_na = FALSE)

# get counts and % in a data.frame, w/ or w/o NAs.  Like table(), kinda.
tabyl <- function(vec, sort = FALSE, show_na = TRUE) {
  # catch and adjust input variable name
  var_name <- deparse(substitute(vec))
  if(var_name == "."){var_name <- "x"} # for variables piped in to tabyl() - the column name "." was causing problems anyway
  var_name <- gsub("\\$", "_", var_name)

  # calculate initial counts table
  # convert vector to a 1 col data.frame
  if(mode(vec) %in% c("logical", "numeric", "character")) {
    d <- data.frame(vec, stringsAsFactors = is.factor(vec))
    result <- d %>% dplyr::count(vec, sort = sort)
    
    if(is.factor(vec)){
      result <- tidyr::complete(result, vec)
      if(sort){result <- dplyr::arrange(result, dplyr::desc(n))} # undo reorder caused by complete()
      }
    
  } else {stop("input must be a logical, numeric, or character vector")}
  
  # calculate percent, move NA row to bottom
  result <- result %>%
    dplyr::mutate(percent = n / sum(n, na.rm = TRUE)) %>%
    dplyr::arrange(is.na(vec))
  
  names(result)[1] <- var_name
  
  ## NA handling:
  # if there are NA values & show_na = T, calculate valid % as a new column
  if(show_na && sum(is.na(result[[1]])) > 0) {
    valid_total <- sum(result$n[!is.na(result[[1]])], na.rm = TRUE)
    result$valid_percent = result$n / valid_total
    result$valid_percent[is.na(result[[1]])] <- NA
    result
      } else { # don't show NA values, which necessitates adjusting the %s
    result %>%
      dplyr::filter(!is.na(.[1])) %>%
      dplyr::mutate(percent = n / sum(n, na.rm = TRUE)) # recalculate % without NAs
  }
}
