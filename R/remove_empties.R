#' @title Remove empty rows and/or columns from a data.frame or matrix.
#'
#' @description Removes all rows and/or columns from a data.frame or matrix that
#'   are composed entirely of \code{NA} values.
#'
#' @param dat the input data.frame or matrix.
#' @param which one of "rows", "cols", or \code{c("rows", "cols")}.  Where no
#'   value of which is provided, defaults to removing both empty rows and empty
#'   columns, declaring the behavior with a printed message.
#' @param quiet Should messages be suppressed (\code{TRUE}) or printed
#'   (\code{FALSE}) indicating the summary of empty columns or rows removed?
#' @return Returns the object without its missing rows or columns.
#' @family remove functions
#' @seealso \code{\link[=remove_constant]{remove_constant()}} for removing
#'   constant columns.
#' @examples
#' # not run:
#' # dat %>% remove_empty("rows")
#' @export

remove_empty <- function(dat, which = c("rows", "cols"), quiet=TRUE) {
  if (missing(which) && !missing(dat)) {
    message("value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")")
    which <- c("rows", "cols")
  }
  if ((sum(which %in% c("rows", "cols")) != length(which)) && !missing(dat)) {
    stop("\"which\" must be one of \"rows\", \"cols\", or c(\"rows\", \"cols\")")
  }
  if ("rows" %in% which) {
    mask_keep <- rowSums(is.na(dat)) != ncol(dat)
    if (!quiet) {
      remove_message(dat=dat, mask_keep=mask_keep, which="rows", reason="empty")
    }
    dat <- dat[mask_keep, , drop = FALSE]
  }
  if ("cols" %in% which) {
    mask_keep <- colSums(!is.na(dat)) > 0
    if (!quiet) {
      remove_message(dat=dat, mask_keep=mask_keep, which="columns", reason="empty")
    }
    dat <- dat[, mask_keep, drop = FALSE]
  }
  dat
}

## Remove constant columns

#' @title Remove constant columns from a data.frame or matrix.
#' @param dat the input data.frame or matrix.
#' @param na.rm should \code{NA} values be removed when considering whether a
#'   column is constant?  The default value of \code{FALSE} will result in a
#'   column not being removed if it's a mix of a single value and \code{NA}.
#' @param quiet Should messages be suppressed (\code{TRUE}) or printed
#'   (\code{FALSE}) indicating the summary of empty columns or rows removed?
#'
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
#'   columns or rows.
#' @export
remove_constant <- function(dat, na.rm = FALSE, quiet=TRUE) {
  mask <-
    sapply(
      X=seq_len(ncol(dat)),
      FUN=function(idx) {
        column_to_test <-
          if (is.matrix(dat)) {
            dat[, idx]
          } else {
            dat[[idx]]
          }
        length(unique(
          if (na.rm) {
            stats::na.omit(column_to_test)
          } else {
            column_to_test
          }
        )) <= 1 # the < is in case all values are NA with na.rm=TRUE
      }
    )
  if (!quiet) {
    remove_message(dat=dat, mask_keep=!mask, which="columns", reason="constant")
  }
  dat[ , !mask, drop=FALSE]
}


#' Generate the message describing columns or rows that are being removed.
#'
#' @inheritParams remove_empty
#' @param mask_keep A logical vector of rows or columns to keep (\code{TRUE}) or
#'   remove (\code{FALSE}).
#' @param reason The reason that rows are being removed (to be used in the
#'   message.
#' @noRd
remove_message <- function(dat, mask_keep, which=c("columns", "rows"), reason=c("empty", "constant")) {
  if (all(mask_keep)) {
    message("No ", reason, " ", which, " to remove.")
  } else {
    details <-
      if (which == "columns") {
        if (is.null(colnames(dat)) || any(colnames(dat) %in% "")) {
          sprintf("%0.3g%%", 100*sum(!mask_keep)/length(mask_keep))
        } else {
          sprintf("Removed: %s", paste(names(dat)[!mask_keep], collapse=", "))
        }
      } else {
        sprintf("%0.3g%%", 100*sum(!mask_keep)/length(mask_keep))
      }
    message(
      sprintf(
        "Removing %g %s %s of %g %s total (%s).",
        sum(!mask_keep), reason, which, length(mask_keep), which, details
      )
    )
  }
}

### Deprecated separate remove row/col functions

#' @title Removes empty rows from a data.frame.
#'
#' @description
#' This function is deprecated, use \code{remove_empty("rows")} instead.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with no empty rows.
#' @examples
#' # not run:
#' # dat %>% remove_empty_rows
#' @export

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
#' @examples
#' # not run:
#' # dat %>% remove_empty_cols
#' @export

remove_empty_cols <- function(dat) {
  .Deprecated("remove_empty(\"cols\")")
  remove_empty(dat, which = "cols")
}
