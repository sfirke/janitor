#' @title Add presentation formatting to a crosstabulation table.
#'
#' @description
#' This function is deprecated, use the \code{adorn_} family of functions instead.
#' @param dat a data.frame with row names in the first column and numeric values in all other columns.  Usually the piped-in result of a call to  \code{crosstab} that included the argument \code{percent = "none"}.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param show_n should counts be displayed alongside the percentages?
#' @param digits how many digits should be displayed after the decimal point?
#' @param show_totals display a totals summary? Will be a row, column, or both depending on the value of \code{denom}.
#' @param rounding method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @return Returns a data.frame.
#' @export

adorn_crosstab <- function(dat, denom = "row", show_n = TRUE, digits = 1, show_totals = FALSE, rounding = "half to even") {
  .Deprecated("use the various adorn_ functions instead.  See the \"tabyl\" vignette for examples.")
  # some input checks
  if (!rounding %in% c("half to even", "half up")) {
    stop("'rounding' must be one of 'half to even' or 'half up'")
  }
  dat[[1]] <- as.character(dat[[1]]) # for type matching when binding the word "Total" on a factor.  Moved up to this line so that if only 1st col is numeric, the function errors
  if (sum(!unlist(lapply(dat, is.numeric))[-1]) > 0) {
    stop("all columns 2:n in input data.frame must be of class numeric")
  }

  showing_col_totals <- (show_totals & denom %in% c("col", "all"))
  showing_row_totals <- (show_totals & denom %in% c("row", "all"))

  if (showing_col_totals) {
    dat <- adorn_totals(dat, "col")
  }
  if (showing_row_totals) {
    dat <- adorn_totals(dat, "row")
  }
  n_col <- ncol(dat)

  percs <- adorn_percentages(dat, denom) # last argument only gets used in the "all" case = no harm in passing otherwise

  # round %s using specified method, add % sign
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(. * 100)) # since we'll be adding % sign - do this before rounding
  if (rounding == "half to even") {
    percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(round(., digits)))
  } else if (rounding == "half up") {
    percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(round_half_up(., digits)))
  }
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(format(., nsmall = digits, trim = TRUE))) # so that 0% prints as 0.0% or 0.00% etc.
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(paste0(., "%")))


  # paste Ns if needed
  if (show_n) {
    result <- paste_ns(percs, dat)
  } else {
    result <- percs
  }

  as.data.frame(result) # drop back to data.frame from tibble
}

## These are legacy helper sub-functions for adorn_crosstab().  They've been replaced with better helpers for adorn_ns().  Keep here until adorn_crosstab() is removed, then remove these also.

# takes data.frames of Ns and %s, pastes them together
paste_ns <- function(perc_df, n_df) {
  n_matrix <- as.matrix(n_df)
  perc_matrix <- as.matrix(perc_df)

  # paste the results together
  pasted <- paste(perc_matrix, " (", n_matrix, ")", sep = "") %>% # paste the matrices
    sapply(., fix_parens_whitespace) %>% # apply the whitespace cleaning function to the resulting vector
    matrix(., nrow = nrow(n_matrix), dimnames = dimnames(perc_matrix)) %>% # cast as matrix, then data.frame
    dplyr::as_data_frame()

  pasted[[1]] <- n_df[[1]] # undo the pasting in this 1st column
  pasted
}

# converts "50.0% ( 1)" to "50.0%  (1)" for nice printing
fix_parens_whitespace <- function(x) {
  culprit <- regmatches(x, regexpr("[(][ ]+", x)) # isolate the problematic string

  # if no problem, return unmodified
  if (length(culprit) == 0) {
    x
  }

  else {
    num_spaces <- length(gregexpr(" ", culprit)[[1]])
    gsub(culprit[[1]],
      paste0( # create replacement string
        paste0(rep(" ", num_spaces), collapse = ""), # generate the spaces
        "(",
        collapse = ""
      ),
      x,
      fixed = TRUE
    )
  }
}
