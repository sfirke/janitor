#' Elevate a row to be the column names of a data.frame.
#'
#' @param dat The input data.frame
#' @param row_number The row of \code{dat} containing the variable names or the
#'   string \code{"find_header"} to use \code{find_header(dat=dat, ...)} to find
#'   the row_number.
#' @param ... Sent to \code{find_header()}, if
#'   \code{row_number = "find_header"}.  Otherwise, ignored.
#' @param remove_row Should the row \code{row_number} be removed from the
#'   resulting data.frame?
#' @param remove_rows_above If \code{row_number != 1}, should the rows above
#'   \code{row_number} - that is, between \code{1:(row_number-1)} - be removed
#'   from the resulting data.frame?
#' @return A data.frame with new names (and some rows removed, if specified)
#' @family Set names
#' @examples
#' x <- data.frame(X_1 = c(NA, "Title", 1:3),
#'                 X_2 = c(NA, "Title2", 4:6))
#' x %>%
#'   row_to_names(row_number = 2)
#'
#' x %>%
#'   row_to_names(row_number = "find_header")
#' @export
row_to_names <- function(dat, row_number, ..., remove_row = TRUE, remove_rows_above = TRUE) {
  # Check inputs
  if (!(is.logical(remove_row) & length(remove_row) == 1)) {
    stop("remove_row must be either TRUE or FALSE, not ", as.character(remove_row))
  } else if (!(is.logical(remove_rows_above) & length(remove_rows_above) == 1)) {
    stop("remove_rows_above must be either TRUE or FALSE, not ", as.character(remove_rows_above))
  } else if (length(row_number) != 1) {
    stop("row_number must be a scalar")
  }
  if (row_number %in% "find_header") {
    # no need to check if it is a character string, %in% will do that for us
    # (and will handle the odd-ball cases like someone sending in
    # factor("find_header")).
    row_number <- find_header(dat=dat, ...)
  } else if (is.numeric(row_number)) {
    extra_args <- list(...)
    if (length(extra_args) != 0) {
      stop("Extra arguments (...) may only be given if row_number = 'find_header'.")
    }
  } else {
    stop("row_number must be a numeric value or 'find_header'")
  }
  new_names <- as.character(unlist(dat[row_number, ], use.names = FALSE))
  if (any(duplicated(new_names))) {
    rlang::warn(
      message=paste("Row", row_number, "does not provide unique names. Consider running clean_names() after row_to_names()."),
      class="janitor_warn_row_to_names_not_unique"
    )
  }
  colnames(dat) <- new_names
  rows_to_remove <- c(
    if (remove_row) {
      row_number
    } else {
      c()
    },
    if (remove_rows_above) {
      seq_len(row_number - 1)
    } else {
      c()
    }
  )
  if (length(rows_to_remove)) {
    dat[-(rows_to_remove), , drop = FALSE]
  } else {
    dat
  }
}

#' Find the header row in a data.frame
#' 
#' @details
#' If \code{...} is missing, then the first row with no missing values is used.
#' 
#' When searching for a specified value or value within a column, the first row
#' with a match will be returned, regardless of the completeness of the rest of
#' that row.  If \code{...} has a single character argument, then the first
#' column is searched for that value.  If \code{...} has a named numeric
#' argument, then the column whose position number matches the value of that
#' argument is searched for the name (see the last example below).  If more than one
#' row is found matching a value that is searched for, the number of the first
#' matching row will be returned (with a warning).
#' 
#' @inheritParams row_to_names
#' @param ... See details
#' @return The row number for the header row
#' @family Set names
#' @examples
#' # the first row
#' find_header(data.frame(A="B"))
#' # the second row
#' find_header(data.frame(A=c(NA, "B")))
#' # the second row since the first has an empty value
#' find_header(data.frame(A=c(NA, "B"), B=c("C", "D")))
#' # The third row because the second column was searched for the text "E"
#' find_header(data.frame(A=c(NA, "B", "C", "D"), B=c("C", "D", "E", "F")), "E"=2)
#' @export
find_header <- function(dat, ...) {
  extra_args <- list(...)
  if (length(extra_args) == 0) {
    # Find the first complete row
    ret <- which(rowSums(is.na(dat)) == 0)
    if (length(ret) == 0) {
      stop("No complete rows (rows with zero NA values) were found.")
    }
    ret <- ret[1]
  } else if (length(extra_args) == 1) {
    if (is.null(names(extra_args))) {
      # Search for the argument in the first column
      column_to_search <- 1
      string_to_search <- extra_args[[1]]
    } else {
      # Search for the name of the argument in the indicated column
      column_to_search <- extra_args[[1]]
      string_to_search <- names(extra_args)
    }
    ret <- which(dat[[column_to_search]] %in% string_to_search)
    if (length(ret) == 0) {
      stop(sprintf(
        "The string '%s' was not found in column %g", string_to_search, column_to_search
      ))
    } else if (length(ret) > 1) {
      rlang::warn(
        message=
          sprintf(
            "The string '%s' was found %g times in column %g, using the first row where it was found",
            string_to_search, length(ret), column_to_search
          ),
        class="janitor_warn_find_header_not_unique"
      )
      ret <- ret[1]
    }
  } else {
    stop("Either zero or one arguments other than 'dat' may be provided.")
  }
  ret
}
