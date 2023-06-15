#' Like `paste()`, but missing values are omitted
#'
#' @details If all values are missing, the value from the first argument is
#'   preserved.
#'
#' @param ...,sep,collapse See [base::paste()}
#' @return A character vector of pasted values.
#' @examples
#' paste_skip_na(NA) # NA_character_
#' paste_skip_na("A", NA) # "A"
#' paste_skip_na("A", NA, c(NA, "B"), sep = ",") # c("A", "A,B")
#' @export
paste_skip_na <- function(..., sep = " ", collapse = NULL) {
  args <- list(...)
  if (length(args) <= 1) {
    if (length(args) == 0) {
      # match the behavior of paste
      paste(sep = sep, collapse = collapse)
    } else if (!is.null(collapse)) {
      if (all(is.na(args[[1]]))) {
        # Collapsing with all NA values results in NA
        NA_character_
      } else {
        # Collapsing without all NA values collapses the non-NA values
        paste(na.omit(args[[1]]), sep = sep, collapse = collapse)
      }
    } else {
      # as.character() to ensure that logical NA values are converted to
      # NA_character_
      as.character(args[[1]])
    }
  } else {
    # There are at least 2 arguments; paste the first two and recurse
    a1 <- args[[1]]
    a2 <- args[[2]]
    if (length(a1) != length(a2)) {
      if (length(a1) == 1) {
        a1 <- rep(a1, length(a2))
      } else if (length(a2) == 1) {
        a2 <- rep(a2, length(a1))
      } else {
        stop("Arguments must be the same length or one argument must be a scalar.")
      }
    }
    # Which arguments are NA, if any?
    mask1 <- !is.na(a1)
    mask2 <- !is.na(a2)
    mask_both <- mask1 & mask2
    mask_only2 <- (!mask1) & mask2
    firsttwo <- a1
    if (any(mask_only2)) {
      firsttwo[mask_only2] <- a2[mask_only2]
    }
    if (any(mask_both)) {
      # Collapse only occurs on the final pasting
      firsttwo[mask_both] <- paste(a1[mask_both], a2[mask_both], sep = sep, collapse = NULL)
    }
    # prepare to recurse, and recurse
    new_args <- append(list(firsttwo), args[-(1:2)])
    new_args$sep <- sep
    new_args$collapse <- collapse
    do.call(paste_skip_na, new_args)
  }
}
