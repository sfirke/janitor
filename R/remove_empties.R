#' Remove empty rows and/or columns from a data.frame or matrix.
#'
#' Removes all rows and/or columns from a data.frame or matrix that
#'   are composed entirely of `NA` values.
#'
#' @param dat the input data.frame or matrix.
#' @param which one of "rows", "cols", or `c("rows", "cols")`. Where no
#'   value of which is provided, defaults to removing both empty rows and empty
#'   columns, declaring the behavior with a printed message.
#' @param cutoff a row/col should have more than this fraction of non-NA values to be
#'   retained.  E.g., `cutoff = 0.8` means that rows/cols that are 20% or more missing will be dropped.
#' @param quiet Should messages be suppressed (`TRUE`) or printed
#'   (`FALSE`) indicating the summary of empty columns or rows removed?
#' @return Returns the object without its missing rows or columns.
#' @family remove functions
#' @seealso [remove_constant()] for removing
#'   constant columns.
#' @examples
#' # not run:
#' # dat %>% remove_empty("rows")
#' # addressing a common untidy-data scenario where we have a mixture of
#' # blank values in some (character) columns and NAs in others:
#' library(dplyr)
#' dd <- tibble(
#'   x = c(LETTERS[1:5], NA, rep("", 2)),
#'   y = c(1:5, rep(NA, 3))
#' )
#' # remove_empty() drops row 5 (all NA) but not 6 and 7 (blanks + NAs)
#' dd %>% remove_empty("rows")
#' # solution: preprocess to convert whitespace/empty strings to NA,
#' # _then_ remove empty (all-NA) rows
#' dd %>%
#'   mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
#'   remove_empty("rows")
#' @export
remove_empty <- function(dat, which = c("rows", "cols"), cutoff = 1, quiet = TRUE) {
  if (missing(which) && !missing(dat)) {
    message("value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")")
    which <- c("rows", "cols")
  }
  if ((sum(which %in% c("rows", "cols")) != length(which)) && !missing(dat)) {
    stop("\"which\" must be one of \"rows\", \"cols\", or c(\"rows\", \"cols\")")
  }
  if (length(cutoff) != 1) {
    stop("cutoff must be a single value")
  } else if (!is.numeric(cutoff)) {
    stop("cutoff must be numeric")
  } else if (cutoff <= 0 | cutoff > 1) {
    stop("cutoff must be >0 and <= 1")
  } else if (length(which) > 1 & cutoff != 1) {
    stop("cutoff must be used with only one of which = 'rows' or 'cols', not both")
  }
  if ("rows" %in% which) {
    # Using different code with cutoff = 1 vs cutoff != 1 to avoid possible
    # floating point errors.
    mask_keep <-
      if (cutoff == 1) {
        rowSums(is.na(dat)) != ncol(dat)
      } else {
        (rowSums(!is.na(dat)) / ncol(dat)) > cutoff
      }
    if (!quiet) {
      remove_message(dat = dat, mask_keep = mask_keep, which = "rows", reason = "empty")
    }
    dat <- dat[mask_keep, , drop = FALSE]
  }
  if ("cols" %in% which) {
    # Using different code with cutoff = 1 vs cutoff != 1 to avoid possible
    # floating point errors.
    mask_keep <-
      if (cutoff == 1) {
        colSums(is.na(dat)) != nrow(dat)
      } else {
        (colSums(!is.na(dat)) / nrow(dat)) > cutoff
      }
    if (!quiet) {
      remove_message(dat = dat, mask_keep = mask_keep, which = "columns", reason = "empty")
    }
    dat <- dat[, mask_keep, drop = FALSE]
  }
  dat
}

## Remove constant columns

#' @title Remove constant columns from a data.frame or matrix.
#' @param dat the input data.frame or matrix.
#' @param na.rm should `NA` values be removed when considering whether a
#'   column is constant?  The default value of `FALSE` will result in a
#'   column not being removed if it's a mix of a single value and `NA`.
#' @param quiet Should messages be suppressed (`TRUE`) or printed
#'   (`FALSE`) indicating the summary of empty columns or rows removed?
#'
#' @examples
#' remove_constant(data.frame(A = 1, B = 1:3))
#'
#' # To find the columns that are constant
#' data.frame(A = 1, B = 1:3) %>%
#'   dplyr::select(!dplyr::all_of(names(remove_constant(.)))) %>%
#'   unique()
#' @importFrom stats na.omit
#' @family remove functions
#' @seealso [remove_empty()] for removing empty
#'   columns or rows.
#' @export
remove_constant <- function(dat, na.rm = FALSE, quiet = TRUE) {
  mask <-
    sapply(
      X = seq_len(ncol(dat)),
      FUN = function(idx) {
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
    remove_message(dat = dat, mask_keep = !mask, which = "columns", reason = "constant")
  }
  dat[, !mask, drop = FALSE]
}


#' Generate the message describing columns or rows that are being removed.
#'
#' @inheritParams remove_empty
#' @param mask_keep A logical vector of rows or columns to keep (`TRUE`) or
#'   remove (`FALSE`).
#' @param reason The reason that rows are being removed (to be used in the
#'   message.
#' @noRd
remove_message <- function(dat, mask_keep, which = c("columns", "rows"), reason = c("empty", "constant")) {
  if (all(mask_keep)) {
    message("No ", reason, " ", which, " to remove.")
  } else {
    details <-
      if (which == "columns") {
        if (is.null(colnames(dat)) || any(colnames(dat) %in% "")) {
          sprintf("%0.3g%%", 100 * sum(!mask_keep) / length(mask_keep))
        } else {
          sprintf("Removed: %s", paste(names(dat)[!mask_keep], collapse = ", "))
        }
      } else {
        sprintf("%0.3g%%", 100 * sum(!mask_keep) / length(mask_keep))
      }
    message(
      sprintf(
        "Removing %g %s %s of %g %s total (%s).",
        sum(!mask_keep), reason, which, length(mask_keep), which, details
      )
    )
  }
}
