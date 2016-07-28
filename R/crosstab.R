#' @title Generate a crosstabulation of two vectors.
#'
#' @description
#' Create a crosstab, displaying either frequencies or percentages calculated by row, column, or overall.  Vectors don't have to be from the same data.frame, but typically are.
#'
#' @param vec1 the vector to place on the crosstab column.
#' @param vec2 the vector to place on the crosstab row.
#' @param percent which grouping to use for percentages, if desired (defaults to counts).  Must be one of "none", "row", "col", or "all".
#' @param show_na should cases where either variable is NA be included?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the crosstabulated variables.

#' @examples
#' crosstab(mtcars$cyl, mtcars$gear)
#' crosstab(mtcars$cyl, mtcars$gear, "row")
#'
#' # pipelined example
#' library(dplyr)
#' mtcars %>% crosstab(cyl, gear)

# Crosstab table of two variables
# Take two vectors and one of "none", "row", "col", and "all" to calculate %s
# Could also take a data.frame and two vector names, for pipeline, but this seems simpler
#' @export
crosstab <- function(...) UseMethod("crosstab")

#' @inheritParams crosstab
#' @describeIn crosstab Create a crosstab from two vectors,
#' displaying either frequencies or percentages calculated by row, column, or overall.
#' Vectors don't have to be from the same data.frame, but typically are.
#' @export
crosstab.default <- function(vec1, vec2, percent = "none", show_na = TRUE, ...){


  if(!mode(vec1) %in% c("logical", "numeric", "character", "list")){
    stop("vec1 must be a vector of type logical, numeric, character, list, or factor")}
  if(!mode(vec2) %in% c("logical", "numeric", "character","list")){
    stop("vec2 must be a vector of type logical, numeric, character, list, or factor")}
  if(length(vec1) != length(vec2)){ stop("the two vectors are not the same length")}

  dat <- data.frame(vec1 = vec1, vec2 = vec2, stringsAsFactors = FALSE)

  dat_col_names <- names(dat)

  if (is.null(names(vec1))) {
    var_name <- deparse(substitute(vec1))
  } else {
    var_name <- dat_col_names[[1]]
  }
  
  var_name <- gsub("[$]", "_", var_name) # for result to have underscore in name instead of .


  if(!show_na){
    dat <- dat[!is.na(dat[[1]]) & !is.na(dat[[2]]), ]
  }

  # create long data.frame with initial counts
  tabl <- dat %>%
    dplyr::count_(dat_col_names) %>%
    dplyr::ungroup()

  # calculate percentages, if specified
  if(percent == "row"){
    tabl <- tabl %>%
      dplyr::group_by_(dat_col_names[[1]]) %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE))
  } else if (percent == "col"){
    tabl <- tabl %>%
      dplyr::group_by_(dat_col_names[[2]]) %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE))
  } else if (percent == "all"){
    tabl <- tabl %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE))
  }

  # replace NA with string NA_ in vec2 to avoid invalid col name after spreading
  # if this col is a factor, need to add that level to the factor
  if(is.factor(tabl[[2]])){
    levels(tabl[[2]]) <- c(levels(tabl[[2]]), "NA_")
  }
  tabl[2][is.na(tabl[2])] <- "NA_"

  # spread to wide, ungroup() for cleanliness of result, and rename 1st col
  tabl %>%
    tidyr::spread_(dat_col_names[[2]], "n", fill = 0) %>%
    dplyr::ungroup() %>%
    stats::setNames(., c(var_name, names(.)[-1])) %>%
    data.frame(., check.names = FALSE)
}

#' @inheritParams crosstab.default
#' @param .data a data.frame.
#' @param ... arguments passed to crosstab.default.
#' @describeIn crosstab Create a crosstab from a data.frame,
#' displaying either frequencies or percentages calculated by row, column, or overall.
#' Vectors don't have to be from the same data.frame, but typically are.
#' @export
crosstab.data.frame <- function(.data, ...){
  # collect dots
  dots <- as.list(substitute(list(...)))[-1L] #
  n <- length(dots)

  # select columns from .data
  columns <- dots[1:2]

  x <- list()
  x[[deparse(columns[[1]])]] <- .data[,deparse(columns[[1]])]
  x[[deparse(columns[[2]])]] <- .data[,deparse(columns[[2]])]
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  # create args list to use with do.call
  arguments <- list()

  if(n > 2) arguments <- dots[3:n]

  arguments$vec1 <- x[1]
  arguments$vec2 <- x[2]

  do.call(crosstab.default,
          args = arguments)

}
