#' @title Convert a data.frame of counts to percentages.
#'
#' @description
#' This function defaults to excluding the first column of the input data.frame, assuming that it contains a descriptive variable, but this can be overridden by specifying the columns to adorn in the \code{...} argument.
#'
#' @param dat a \code{tabyl} or other data.frame with a tabyl-like layout.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).
#' @param denominator the direction to use for calculating percentages.  One of "row", "col", or "all".
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @param ... columns to adorn.  This takes a tidyselect specification.  By default, all numeric columns (besides the initial column, if numeric) are adorned, but this allows you to manually specify which columns should be adorned, for use on a data.frame that does not result from a call to \code{tabyl}.
#'
#' @return Returns a data.frame of percentages, expressed as numeric values between 0 and 1.
#' @export
#' @examples
#'
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages("col")
#'
#' # calculates correctly even with totals column and/or row:
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_totals("row") %>%
#'   adorn_percentages()
#'   
#' # Control the columns to be adorned with the ... variable selection argument
#' # If using only the ... argument, you can use empty commas as shorthand 
#' # to supply the default values to the preceding arguments:
#' 
#' cases <- data.frame(
#'   region = c("East", "West"),
#'   year = 2015,
#'   recovered = c(125, 87),
#'   died = c(13, 12)
#' )
#' 
#'cases %>%
#'  adorn_percentages(,,recovered:died)

adorn_percentages <- function(dat, denominator = "row", na.rm = TRUE, ...) {
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    purrr::map(dat, adorn_percentages, denominator, na.rm, ...)
  } else {
    # catch bad inputs
    if (!is.data.frame(dat)) {
      stop("adorn_percentages() must be called on a data.frame or list of data.frames")
    }
    if (!denominator %in% c("row", "col", "all")) {
      stop("'denominator' must be one of 'row', 'col', or 'all'")
    }
    
    dat <- as_tabyl(dat)

    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
    non_numeric_cols <- setdiff(1:ncol(dat), numeric_cols)
    numeric_cols <- setdiff(numeric_cols, 1) # assume 1st column should not be included so remove it from numeric_cols. Moved up to this line so that if only 1st col is numeric, the function errors
    explicitly_exempt_totals <- FALSE
    
    if(rlang::dots_n(...) == 0){
      cols_to_tally <- numeric_cols
    } else {
      expr <- rlang::expr(c(...))
      cols_to_tally <- tidyselect::eval_select(expr, data = dat)
      explicitly_exempt_totals <- !(ncol(dat) %in% cols_to_tally) # if not present, it's b/c user explicitly exempted it
      if(any(cols_to_tally %in% non_numeric_cols)){
        message("At least one non-numeric column was specified.  All non-numeric columns will be removed from percentage calculations.")
        cols_to_tally <- setdiff(cols_to_tally, non_numeric_cols)
      }
    }
    
    if ("col" %in% attr(dat, "totals")) {
      # if there's a totals col, don't use it to calculate the %s
      cols_to_tally <- setdiff(cols_to_tally, ncol(dat))
    }

    if (denominator == "row") {
      # if row-wise percentages and a totals column, need to exempt totals col and make it all 1s
      if ("col" %in% attr(dat, "totals") & !explicitly_exempt_totals) {
        dat[[ncol(dat)]] <- rep(1, nrow(dat))
      }
      row_sum <- rowSums(dat[cols_to_tally], na.rm = na.rm)
      dat[, cols_to_tally] <- dat[cols_to_tally] / row_sum
    } else if (denominator == "col") {
      # if col-wise percentages and a row column, need to exempt totals row and make it all 1s
      if ("row" %in% attr(dat, "totals")) {
        col_sum <- colSums(dat[-nrow(dat), ][cols_to_tally], na.rm = na.rm)
      } else {
        col_sum <- colSums(dat[cols_to_tally], na.rm = na.rm)
      }
      # add totals col back to be tallied, #357
      if ("col" %in% attr(dat, "totals") & !explicitly_exempt_totals) {
        cols_to_tally <- c(cols_to_tally, ncol(dat))
        if ("row" %in% attr(dat, "totals")) {
          col_sum <- c(col_sum, sum(dat[-nrow(dat), ncol(dat)]))
        } else {
          col_sum <- c(col_sum, sum(dat[ , ncol(dat)]))
        }
      }
      dat[cols_to_tally] <- sweep(dat[cols_to_tally], 2, col_sum, `/`) # from http://stackoverflow.com/questions/9447801/dividing-columns-by-colsums-in-r
    } else if (denominator == "all") {
      # if all-wise percentages, need to exempt any totals col or row
      if ("row" %in% attr(dat, "totals")) {
        complete_n <- sum(dat[-nrow(dat), cols_to_tally], na.rm = TRUE)
      } else {
        complete_n <- sum(dat[, cols_to_tally], na.rm = TRUE)
      }
      # add totals col back to be tallied, #357
      if ("col" %in% attr(dat, "totals") & !explicitly_exempt_totals) {
        cols_to_tally <- c(cols_to_tally, ncol(dat))
      }
      dat[cols_to_tally] <- dat[cols_to_tally] / complete_n
    }
    dat
  }
}
