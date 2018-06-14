#' Elevate a row to be the column names of a data.frame
#'
#' @param dat The input data.frame
#' @param row_number The row of \code{dat} to make into the header
#' @param remove_row Remove \code{row_number} from the return value?
#' @param remove_rows_above If \code{row_number != 1}, remove rows
#'   \code{1:(row_number-1)} from the return value?
#' @return A data frame with new names (and some rows removed, if requested)
#' @examples
#' data.frame(X__1=c(NA, "Title", 1:3),
#'            X__2=c(NA, "Title2", 4:6),
#'            stringsAsFactors=FALSE) %>%
#'   row_to_names(row_number=2)
#' @export
row_to_names <- function(dat, row_number=1, remove_row=TRUE, remove_rows_above=TRUE) {
  # Check inputs
  if (length(row_number) != 1) {
    stop("row_number must be a scalar")
  }
  new_names <- as.character(unlist(dat[row_number,], use.names=FALSE))
  if (any(duplicated(new_names))) {
    warning("Row ", row_number, " does not provide unique names (consider running clean_names() on the return value)")
  }
  names(dat) <- new_names
  rows_to_remove <- c(
    if(remove_row) {
      row_number
    } else {
      c()
    },
    if (remove_rows_above) {
      seq_len(row_number-1)
    } else {
      c()
    })
  if (length(rows_to_remove)) {
    dat[-(rows_to_remove),,drop=FALSE]
  } else {
    dat
  }
}
