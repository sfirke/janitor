#' @importFrom magrittr "%>%"
#'
#' @title Generate a table of a vector.
#'
#' @description
#' Get a frequency table of a variable as a data.frame, showing percentages and with or without including \code{NA} values.  A fully-featured alternative to \code{table()}.  Can be called on a vector, or used in dplyr pipelines, taking a data.frame and variable name.
#'
#' @param dat the data.frame name, if calling in a pipeline, otherwise the vector.
#' @param ... the unquoted name of the variable in the data.frame to tabulate.
#' @param show_na should cases where the variable is NA be shown?
#' @param sort should the resulting table be sorted in descending order?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the tabulated variable.  Includes counts, percentages, and valid percentages (calculated omitting \code{NA} values, if present in the vector and \code{show_na = TRUE}.)
#' @export
#' @examples
#' tabyl(mtcars$cyl)
#' tabyl(mtcars$cyl, sort = TRUE)
#' # called with magrittr pipe:
#' library(dplyr)
#' mtcars %>% tabyl(cyl)
#' # illustrating show_na functionality:
#' my_cars <- rbind(mtcars, rep(NA, 11))
#' my_cars %>% tabyl(cyl)
#' my_cars %>% tabyl(cyl, show_na = FALSE)
#'

# get counts and % in a data.frame, w/ or w/o NAs.  Like table(), kinda.  Use in pipelines or with vectors.
tabyl <- function(dat, ..., show_na = TRUE, sort = FALSE) {
  var_name <- deparse(substitute(dat))
  if(var_name == "."){var_name <- "x"} # for variables piped in to tabyl() - the column name "." was causing problems anyway

  # calculate initial counts table
  # handle calls where it is fed a vector by converting to a 1 col data.frame
  if(mode(dat) %in% c("logical", "numeric", "character")) {
    d <- data.frame(dat, stringsAsFactors = TRUE)
    result <- d %>% dplyr::count(dat, sort = sort)
    if(is.factor(dat)){
      result <- tidyr::complete(result, dat)
      if(sort){result <- dplyr::arrange(result, dplyr::desc(n))} # undo reorder caused by complete()
      }
    names(result)[1] <- var_name
  } else if(class(dat) == "data.frame") { # if given a data.frame
    dots  <- as.list(substitute(list(...)))[-1L]
    if(length(dots) == 0){stop("no variable name specified")}
    if(length(dots) > 1){stop("more than one variable name specified")}
    result <- dat %>%
      dplyr::count(..., sort = sort)
    if(is.factor(result[[1]])) {
      result <- tidyr::complete(result, ...)
      if(sort){result <- dplyr::arrange(result, dplyr::desc(n))} # undo reorder caused by complete()
      } # add back any missing factor categories
  } else {stop("input must be a logical, numeric, or character vector, or a data.frame with a column name specified in '...'")}
  
  # calculate percent, move NA row to bottom
  result <- result %>%
    dplyr::mutate(percent = n / sum(n, na.rm = TRUE)) %>%
    dplyr::arrange(is.na(.[1]))
  
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
