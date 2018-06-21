#' Elevate a row to be the column names of a data.frame.
#'
#' @param dat The input data.frame
#' @param row_number The row of \code{dat} containing the variable names
#' @param remove_row Should the row \code{row_number} be removed from the resulting data.frame?
#' @param remove_rows_above If \code{row_number != 1}, should the rows above \code{row_number} - that is, between
#'   \code{1:(row_number-1)} - be removed from the resulting data.frame?
#' @return A data.frame with new names (and some rows removed, if specified)
#' @examples
#' x <- data.frame(X_1 = c(NA, "Title", 1:3),
#'            X_2 = c(NA, "Title2", 4:6))
#' x %>%
#'   row_to_names(row_number = 2)
#' @export
row_to_names <- function(dat, row_number, remove_row = TRUE, remove_rows_above = TRUE) {
  # Check inputs
  if (length(row_number) != 1 | !is.numeric(row_number)) {
    stop("row_number must be a numeric of length 1")
  }
  new_names <- as.character(unlist(dat[row_number, ], use.names = FALSE))
  if (any(duplicated(new_names))) {
    warning("Row ", row_number, " does not provide unique names. Consider running clean_names() after row_to_names().")
  }
  names(dat) <- new_names
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
