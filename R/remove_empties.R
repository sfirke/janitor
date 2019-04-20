#' @title Remove empty rows and/or columns from a data.frame or matrix.
#'
#' @description
#' Removes all rows and/or columns from a data.frame or matrix that are composed entirely of \code{NA} values.
#'
#' @param dat the input data.frame or matrix.
#' @param which one of "rows", "cols", or \code{c("rows", "cols")}.  Where no value of which is provided, defaults to removing both empty rows and empty columns, declaring the behavior with a printed message.
#' @param na.rm Exclude NA for comparison of being constant?
#' @return Returns the object without its missing rows or columns.
#' @family remove functions
#' @seealso \code{\link[=remove_constant]{remove_constant()}} for removing constant
#' columns.
#' @export
#' @examples
#' # not run:
#' # dat %>% remove_empty("rows")

remove_empty <- function(dat, which = c("rows", "cols")) {
  if (missing(which) && !missing(dat)) {
    message("value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")")
    which <- c("rows", "cols")
  }
  if ((sum(which %in% c("rows", "cols")) != length(which)) && !missing(dat)) {
    stop("\"which\" must be one of \"rows\", \"cols\", or c(\"rows\", \"cols\")")
  }
  if ("rows" %in% which) {
    dat <- dat[rowSums(is.na(dat)) != ncol(dat), , drop = FALSE]
  }
  if ("cols" %in% which) {
    dat <- dat[,colSums(!is.na(dat)) > 0, drop = FALSE]
  }
  dat
}

## Remove constant columns


#' @title Remove constant columns from a data.frame or matrix.
#' @examples
#' remove_constant(data.frame(A=1, B=1:3))
#' 
#' # To find the columns that are constant
#' data.frame(A=1, B=1:3) %>%
#'   dplyr::select_at(setdiff(names(.), names(remove_constant(.)))) %>%
#'   unique()
#' @importFrom stats na.omit
#' @family remove functions
#' @seealso \code{\link[=remove_empty]{remove_empty()}} for removing empty 
#' columns or rows.
#' @export
remove_constant <- function(dat, na.rm=FALSE) {
  mask <-
    sapply(
      X=seq_len(ncol(dat)),
      FUN=function(idx) {
        if (na.rm) {
          all(is.na(dat[, idx])) ||
            all(
              is.na(dat[, idx]) |
                (dat[, idx] %in% stats::na.omit(dat[, idx])[1])
            )
        } else {
          all(dat[, idx] %in% dat[1, idx])
        }
      }
    )
  dat[ , !mask, drop=FALSE]
}

### Deprecated separate remove row/col functions

#' @title Removes empty rows from a data.frame.
#'
#' @description
#' This function is deprecated, use \code{remove_empty("rows")} instead.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty rows.
#' @export
#' @examples
#' # not run:
#' # dat %>% remove_empty_rows

remove_empty_rows <- function(dat) {
  .Deprecated("remove_empty(\"rows\")")
  remove_empty(dat, which = "rows")
}

#' @title Removes empty columns from a data.frame.
#'
#' @description
#' This function is deprecated, use \code{remove_empty("cols")} instead.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty columns.
#' @export
#' @examples
#' # not run:
#' # dat %>% remove_empty_cols
#'

remove_empty_cols <- function(dat) {
  .Deprecated("remove_empty(\"cols\")")
  remove_empty(dat, which = "cols")
}
