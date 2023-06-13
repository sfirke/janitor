#' Find the list of columns that have a 1:1 mapping to each other
#'
#' @param dat A data.frame or similar object
#' @return A list with one element for each group of columns that map
#'   identically to each other.
#' @examples
#' foo <- data.frame(
#'   Lab_Test_Long = c("Cholesterol, LDL", "Cholesterol, LDL", "Glucose"),
#'   Lab_Test_Short = c("CLDL", "CLDL", "GLUC"),
#'   LOINC = c(12345, 12345, 54321),
#'   Person = c("Sam", "Bill", "Sam"),
#'   stringsAsFactors = FALSE
#' )
#' get_one_to_one(foo)
#' @export
get_one_to_one <- function(dat) {
  stopifnot(ncol(dat) > 0)
  stopifnot(!any(duplicated(names(dat))))
  dat_alt <- dat
  for (idx in seq_along(dat_alt)) {
    dat_alt[[idx]] <- get_one_to_one_value_order(dat_alt[[idx]])
  }
  remaining_cols <- names(dat_alt)
  ret <- list()
  while (length(remaining_cols) > 0) {
    nm1 <- remaining_cols[1]
    remaining_cols <- remaining_cols[-1]
    current_ret <- nm1
    for (nm2 in remaining_cols) {
      if (identical(dat_alt[[nm1]], dat_alt[[nm2]])) {
        current_ret <- c(current_ret, nm2)
        remaining_cols <- setdiff(remaining_cols, nm2)
      }
    }
    if (length(current_ret) > 1) {
      ret[[length(ret) + 1]] <- current_ret
    }
  }
  if (length(ret) == 0) {
    message("No columns in `", deparse(substitute(dat)), "` map to each other")
  }
  ret
}

get_one_to_one_value_order <- function(x) {
  # Convert the value to a factor so that any subtly different values become integers
  uvalues <- match(x, unique(x))
  new_value <- as.integer(factor(uvalues, levels = unique(uvalues)))
  new_value
}
