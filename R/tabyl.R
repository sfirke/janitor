#' @title Generate a frequency table from a vector.
#'
#' @description
#' Create a frequency table of a variable, returned as a data.frame.  It shows counts, percentages and, if \code{NA} values are present, valid percentages (calculated excluding \code{NA} values).  A fully-featured alternative to \code{table()}.
#' 
#' \code{tabyl} can be called in two ways:
#' 
#' 1) It can simply be called on a vector, like \code{tabyl(mtcars$gear)}.
#' 
#' 2) A data.frame can be provided as the first argument, followed by an unquoted column name to tabulate.  This enables passing in a data.frame from a \code{\%>\%} pipeline, like \code{mtcars \%>\% tabyl(gear)}. 
#' 
#' @param vec the vector to tabulate.  If supplying a data.frame, this should be an unquoted column name.
#' @param sort a logical value indicating whether the resulting table should be sorted in descending order of \code{n}.
#' @param show_na a logical value indicating whether the count of \code{NA} values should be displayed, along with an additional column showing valid percentages.
#' @param .data (optional) a data.frame, in which case \code{vec} should be an unquoted column name.
#' @param ... additional arguments, if calling \code{tabyl} on a data.frame.
#' @return Returns a data.frame with the frequencies and percentages of the tabulated variable.
#' @export
#' @examples
#' # Calling on a vector:
#' val <- c("hi", "med", "med", "lo")
#' tabyl(val)
#' tabyl(mtcars$cyl, sort = TRUE)
#' 
#' # Passing in a data.frame using a pipeline:
#' mtcars %>% tabyl(cyl, sort = TRUE)
#' 
#' # illustrating show_na functionality:
#' my_cars <- rbind(mtcars, rep(NA, 11))
#' tabyl(my_cars$cyl)
#' tabyl(my_cars$cyl, show_na = FALSE)

tabyl <- function(...) UseMethod("tabyl")

#' @inheritParams tabyl
#' @export
#' @rdname tabyl

tabyl.default <- function(vec, sort = FALSE, show_na = TRUE, ...) {
  
  # catch and adjust input variable name.
  if(is.null(names(vec))) {
    var_name <- deparse(substitute(vec))
  } else {
    var_name <- names(vec)
  }

  # useful error message if input vector doesn't exist
  if(is.null(vec)){stop(paste0("object ", var_name, " not found"))}
  # an odd variable name can be deparsed into a vector of length >1, rare but throws warning, see issue #87
  if(length(var_name) > 1){ var_name <- paste(var_name, collapse = "") }
  
  # calculate initial counts table
  # convert vector to a 1 col data.frame
  if(mode(vec) %in% c("logical", "numeric", "character", "list") & !is.matrix(vec)) {
    if(is.list(vec)){ vec <- vec[[1]] } # to preserve factor properties when vec is passed in as a list from data.frame method
    dat <- data.frame(vec, stringsAsFactors = is.factor(vec))
    names(dat)[1] <- "vec"
    
    
    result <- dat %>% dplyr::count(vec, sort = sort)
    
    if(is.factor(vec)){
      expanded <- tidyr::expand(result, vec)
      result <- merge(x = expanded, # can't use dplyr::left_join because as of 0.6.0, NAs don't match, and na_matches argument not present < 0.6.0
                      y = result,
                      by = "vec",
                      all.x = TRUE)
      result <- dplyr::arrange(result, vec) # restore sorting by factor level
      if(sort){result <- dplyr::arrange(result, dplyr::desc(n))} # undo reorder caused by complete()
    }
    
  } else {stop("input must be a vector of type logical, numeric, character, list, or factor")}
  
  # calculate percent, move NA row to bottom
  result <- result %>%
    dplyr::mutate(percent = n / sum(n, na.rm = TRUE))
  
  # sort the NA row to the bottom, necessary to retain factor sorting  
  result <- result[order(is.na(result$vec)), ]
  result$is_na <- NULL
  
  # replace all NA values with 0 - only applies to missing factor levels
  result <- tidyr::replace_na(result, replace = list(n = 0, percent = 0))
  
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
  
  # reassign correct variable name
  names(result)[1] <- var_name
  
  # in case input var name was "n" or "percent", call helper function to set unique names
  result <- handle_if_special_names_used(result)
  
  data.frame(result, check.names = FALSE)
}

#' @inheritParams tabyl
#' @export
#' @rdname tabyl 

tabyl.data.frame <- function(.data, ...){
  # collect dots
  dots <- as.list(substitute(list(...)))[-1L]
  n <- length(dots)
  
  # select columns from .data
  columns <- dots[1]
  x <- list()
  x[[deparse(columns[[1]])]] <- .data[, deparse(columns[[1]])]
  
  x <- data.frame(x,
                  stringsAsFactors = is.factor(x[[1]]),
                  check.names = FALSE) # preserve bad input names
  
  # create args list to use with do.call
  arguments <- list()
  
  if(n > 1) arguments <- dots[2:n]
  arguments$vec <- x[1]
  
  do.call(tabyl.default,
          args = arguments)
  
}

# function that checks if col 1 name is "n" or "percent",
## if so modifies the appropriate other column name to avoid duplicates
handle_if_special_names_used <- function(dat){
  if(names(dat)[1] == "n"){
    names(dat)[2] <- "n_n"
  } else if(names(dat)[1] == "percent"){
    names(dat)[3] <- "percent_percent"
  }
  dat
}
