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

  # calculate initial counts table
  # handle calls where it is fed a vector by converting to a 1 col data.frame and counting
  if(is.vector(dat)) {
    var_name <- deparse(substitute(dat))
    dat <- data.frame(dat)
    result <- dat %>% dplyr::count(.[[1]], sort = sort)
    names(result)[1] <- var_name
  } else { # if given a data.frame
    result <- dat %>%
      dplyr::count(..., sort = sort)
  }

  result <- result %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    dplyr::arrange(is.na(.[1])) # put NA row back at bottom

  # if there are NA values and it is desired, calculate valid % as a new column
  if(show_na & sum(is.na(result[[1]])) > 0) {
    result %>%
      dplyr::group_by(is_na_var1 = is.na(.[1])) %>%
      dplyr::mutate(valid_percent = n / sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(valid_percent = ifelse(
        !is_na_var1, valid_percent, as.numeric(NA))) %>%
      dplyr::select(-is_na_var1)
  } else {
    result %>%
      dplyr::filter(!is.na(.[1])) %>%
      dplyr::mutate(percent = n / sum(n)) # recalculate % without NAs
  }
}
