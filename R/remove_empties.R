#' @title Remove empty rows and/or columns from a data.frame or matrix.
#'
#' @description
#' Removes all rows and/or columns from a data.frame or matrix that are composed entirely of \code{NA} values.
#'
#' @param dat the input data.frame or matrix.
#' @param which one of "rows", "cols", or \code{c("rows", "cols")}.  Where no value of which is provided, defaults to removing both empty rows and empty columns, declaring the behavior with a printed message.
#' @return Returns the object without its missing rows or columns.
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


### Deprecated separate functions

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

#' Find rows and/or columns that are constant.
#'
#' @inheritParams remove_empty
#' @seealso \code{\link{remove_constant}}, \code{\link{keep_constant}}
#' @return A list with a "rows" and a "cols" element of logical vectors
#'   indicating if the rows or columns are constant.  If \code{which} does not
#'   include one of the list elements then that element is all \code{FALSE} (for
#'   example if \code{which="cols"}, the return for rows will be all
#'   \code{FALSE}).
#' @noRd
find_constant <- function(dat, which = c("rows", "cols")) 
  UseMethod("find_constant")

find_constant.data.frame <- function(dat, which = c("rows", "cols")) {
  which <- match.arg(which, several.ok=TRUE)
  mask_rows <-
    if ("rows" %in% which) {
      sapply(
        X=seq_len(nrow(dat)),
        FUN=function(idx) {
          the_row <- dat[idx,]
          if (is.list(the_row)) {
            the_row <- unlist(the_row)
          }
          all(the_row %in% dat[idx,1])
        }
      )
    } else {
      rep(FALSE, nrow(dat))
    }
  mask_cols <-
    if ("cols" %in% which) {
      sapply(
        X=seq_len(ncol(dat)),
        FUN=function(idx) {
          all(dat[,idx] %in% dat[1,idx])
        }
      )
    } else {
      rep(FALSE, ncol(dat))
    }
  list(
    rows=mask_rows,
    cols=mask_cols
  )
}

find_constant.matrix <- find_constant.data.frame

#' @describeIn remove_empty Remove constant columns from a data.frame or matrix.
#' @export
remove_constant <- function(dat, which = c("rows", "cols")) {
  masks <- find_constant(dat, which=which)
  ret <- dat[ , !masks$cols, drop=FALSE]
  ret[!masks$rows, , drop=FALSE]
}

#' @describeIn remove_empty Keep only constant columns from a data.frame or
#'   matrix (often followed by \code{unique()}).
#' @export
keep_constant <- function(dat, which = c("rows", "cols")) {
  masks <- find_constant(dat, which=which)
  ret <- dat[ , masks$cols, drop=FALSE]
  ret[masks$rows, , drop=FALSE]
}
