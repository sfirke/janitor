#' Ensure that a vector has only a single value throughout.
#'
#' Missing values are replaced with the single value, and if all values are
#' missing, the first value in \code{missing} is used throughout.
#'
#' @param x The vector which should have a single value
#' @param missing The vector of values to consider missing in \code{x}
#' @param warn_if_all_missing Generate a warning if all values are missing?
#' @param info If more than one value is found, append this to the warning or
#'   error to assist with determining the location of the issue.
#' @return \code{x} as the scalar single value found throughout (or an error if
#'   more than one value is found).
#' @examples
#' single_value(c(NA, 1))
#' @export
single_value <- function(x, missing=NA, warn_if_all_missing=FALSE, info=NULL) {
  mask_found <- !(x %in% missing)
  found_values <- unique(x[mask_found])
  if (length(found_values) == 0) {
    missing[1]
  } else if (length(found_values) == 1) {
    found_values
  } else {
    if (!is.null(info)) {
      info <- paste(":", info)
    }
    stop("More than one (", length(found_values), ") value found (", paste(found_values, collapse=", "), ")", info)
  }
}
