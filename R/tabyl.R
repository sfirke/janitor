#' @title Generate a frequency table from a vector.
#'
#' @description
#' Create a frequency table of a variable, returned as a data.frame, showing percentages and with or without including \code{NA} values.  A fully-featured alternative to \code{table()}.
#'
#' @param vec the vector to tabulate.
#' @param sort should the resulting table be sorted in descending order?
#' @param show_na should cases where the variable is NA be shown?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the tabulated variable.  Includes counts, percentages, and valid percentages (calculated omitting \code{NA} values, if present in the vector and \code{show_na = TRUE}.)
#' @examples
#' tabyl(mtcars$cyl)
#' tabyl(mtcars$cyl, sort = TRUE)
#' 
#' # called with magrittr pipe:
#' library(dplyr)
#' mtcars %>% tabyl(cyl)
#' 
#' # illustrating show_na functionality:
#' my_cars <- rbind(mtcars, rep(NA, 11))
#' tabyl(my_cars$cyl)
#' tabyl(my_cars$cyl, show_na = FALSE)

#' @export
tabyl <- function(...) UseMethod("tabyl")

#' @inheritParams tabyl
#' @describeIn tabyl Create a frequency table from a vector, returned as a data.frame, showing percentages and with or without including \code{NA} values.  A fully-featured alternative to \code{table()}.
#' @export
tabyl.default <- function(vec, sort = FALSE, show_na = TRUE, ...) {
  
  # catch and adjust input variable name.
  if(is.null(names(vec))) {
    var_name <- deparse(substitute(vec))
  } else {
    var_name <- names(vec)
  }
  
  # calculate initial counts table
  # convert vector to a 1 col data.frame
  if(mode(vec) %in% c("logical", "numeric", "character", "list") & !is.matrix(vec)) {
    if(is.list(vec)){ vec <- vec[[1]] } # to preserve factor properties when vec is passed in as a list from data.frame method
    dat <- data.frame(vec, stringsAsFactors = is.factor(vec))
    names(dat)[1] <- "vec"

    
    result <- dat %>% dplyr::count(vec, sort = sort)
  
    if(is.factor(vec)){
      result <- tidyr::complete(result, vec)
      if(sort){result <- dplyr::arrange(result, dplyr::desc(n))} # undo reorder caused by complete()
      }
    
  } else {stop("input must be a vector of type logical, numeric, character, list, or factor")}
  
  # calculate percent, move NA row to bottom
  result <- result %>%
    dplyr::mutate(percent = n / sum(n, na.rm = TRUE))
  
  # sort the NA row to the bottom, necessary to retain factor sorting  
  result <- result[order(is.na(result$vec)), ]
  result$is_na <- NULL

  # reassign correct variable name
  names(result)[1] <- var_name
  
  ## NA handling:
  # if there are NA values & show_na = T, calculate valid % as a new column
  if(show_na && sum(is.na(result[[1]])) > 0) {
    valid_total <- sum(result$n[!is.na(result[[1]])], na.rm = TRUE)
    result$valid_percent = result$n / valid_total
    result$valid_percent[is.na(result[[1]])] <- NA
      } else { # don't show NA values, which necessitates adjusting the %s
    result <- result %>%
      dplyr::filter(!is.na(.[1])) %>%
      dplyr::mutate(percent = n / sum(n, na.rm = TRUE)) # recalculate % without NAs
      }
  data.frame(result, check.names = FALSE)
}

#' @inheritParams tabyl.default
#' @param .data a data.frame.
#' @param ... arguments passed to tabyl.default.
#' @describeIn tabyl Create a frequency table from a variable in a data.frame, returned as a data.frame, showing percentages and with or without including \code{NA} values.  A fully-featured alternative to \code{table()}.
#' @export
tabyl.data.frame <- function(.data, ...){
  # collect dots
  dots <- as.list(substitute(list(...)))[-1L]
  n <- length(dots)
  
  # select columns from .data
  columns <- dots[1]
  x <- list()
  x[[deparse(columns[[1]])]] <- .data[, deparse(columns[[1]])]
  x <- as.data.frame(x, stringsAsFactors = is.factor(x[[1]]))
  
  # create args list to use with do.call
  arguments <- list()
  
  if(n > 1) arguments <- dots[2:n]
  arguments$vec <- x[1]
  
  do.call(tabyl.default,
          args = arguments)
  
}
