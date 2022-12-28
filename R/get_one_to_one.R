#' Find the list of columns that have a 1:1 mapping to each other
#'
#' @param dat A data.frame or similar object
#' @return A list with one element for each group of columns that map
#'   identically to each other.
#' @examples
#' foo <- data.frame(
#'   Lab_Test_Long=c("Cholesterol, LDL", "Cholesterol, LDL", "Glucose"),
#'   Lab_Test_Short=c("CLDL", "CLDL", "GLUC"),
#'   LOINC=c(12345, 12345, 54321),
#'   Person=c("Sam", "Bill", "Sam"),
#'   stringsAsFactors=FALSE
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
  for (nm1 in remaining_cols) {
    ret[[length(ret) + 1]] <- nm1
    remaining_cols <- setdiff(remaining_cols, nm1)
    for (nm2 in remaining_cols) {
      if (identical(dat_alt[[nm1]], dat_alt[[nm2]])) {
        ret[[length(ret)]] <- c(ret[[length(ret)]], nm2)
        remaining_cols <- setdiff(remaining_cols, nm2)
      }
    }
  }
  ret
}

get_one_to_one_value_order <- function(x) {
  if (any(is.na(x))) {
    new_value <- as.integer(factor(x))
    # Factor ordering starts at 1, so assign -1 to be a unique value for NA
    new_value[is.na(new_value)] <- -1L
    # redo the conversion so that NA values are in the same order as other
    # values
    ulevels <- unique(new_value)
    new_value <- as.integer(factor(new_value, levels = ulevels))
  } else {
    ulevels <- unique(x)
    new_value <- as.integer(factor(x, levels = ulevels))
  }
  new_value
}
