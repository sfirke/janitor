#' @title Add presentation formatting to a crosstabulation table.
#'
#' @description
#' Formats a data.frame containing counts of co-occurences of two variables (i.e., a contingency table or crosstab).  Adds a mix of percentages, Ns, totals row/column, and custom rounding to a table of integer counts, in the style of a Microsoft Excel PivotTable.  The result is no longer clean data, but is an audience-friendly way to report results.
#' 
#' Designed to run on the output of a call to \code{janitor::crosstab}, but can be called on any data.frame containing a contingency table, e.g., the result of \code{dplyr::count()} followed by \code{tidyr::spread()}.
#'
#' @param dat a data.frame with row names in the first column and numeric values in all other columns.  Usually the piped-in result of a call to  \code{crosstab} that included the argument \code{percent = "none"}.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param show_n should counts be displayed alongside the percentages?
#' @param digits how many digits should be displayed after the decimal point?
#' @param show_totals display a totals summary? Will be a row, column, or both depending on the value of \code{denom}.
#' @param rounding method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @return Returns a data.frame.
#' @examples
#'mtcars %>%
#'  crosstab(gear, cyl) %>%
#'  adorn_crosstab(denom = "all")
#'  
#' # showing with all parameters
#'mtcars %>%
#'  crosstab(gear, cyl) %>%
#'  adorn_crosstab(., denom = "col", rounding = "half up", show_n = FALSE, digits = 2)

# to illustrate rounding half up - 12.5 becomes 13
#'mtcars %>%
#'  crosstab(cyl, am) %>%
#'  adorn_crosstab(., denom = "all", digits = 0, rounding = "half up") 

# take result of a crosstab() call and print a nice result
#' @export
adorn_crosstab <- function(dat, denom = "row", show_n = TRUE, digits = 1, show_totals = FALSE, rounding = "half to even"){
  # some input checks
  if(! rounding %in% c("half to even", "half up")){stop("'rounding' must be one of 'half to even' or 'half up'")}
  dat[[1]] <- as.character(dat[[1]]) # for type matching when binding the word "Total" on a factor.  Moved up to this line so that if only 1st col is numeric, the function errors
  if(sum(!unlist(lapply(dat, is.numeric))[-1]) > 0){stop("all columns 2:n in input data.frame must be of class numeric")} 
  
  showing_col_totals <- (show_totals & denom %in% c("col", "all"))
  showing_row_totals <- (show_totals & denom %in% c("row", "all"))
  
  complete_n <- sum(dat[, -1], na.rm = TRUE) # capture for percent calcs before any totals col/row is added
  
  if(showing_col_totals){ dat <- adorn_totals(dat, "col") }
  if(showing_row_totals){ dat <- adorn_totals(dat, "row") }
  n_col <- ncol(dat)
  
  percs <- ns_to_percents(dat, denom, total_n = complete_n) # last argument only gets used in the "all" case = no harm in passing otherwise
  
  # round %s using specified method, add % sign
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(. * 100)) # since we'll be adding % sign - do this before rounding
  if(rounding == "half to even"){ percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(round(., digits))) }
  else if(rounding == "half up"){ percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(round_half_up(., digits)))}
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(format(., nsmall = digits, trim = TRUE))) # so that 0% prints as 0.0% or 0.00% etc.
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(paste0(., "%")))

  
    # paste Ns if needed
  if(show_n){
    result <- paste_ns(percs, dat)
  } else{ result <- percs}
  
  as.data.frame(result) # drop back to data.frame from tibble
  
}


