#' @title Format a data.frame of decimals as percentages.
#'
#' @description
#' Numeric columns get multiplied by 100 and formatted as percentages according to user specifications.  This function defaults to excluding the first column of the input data.frame, assuming that it contains a descriptive variable, but this can be overridden by specifying the columns to adorn in the \code{...} argument.  Non-numeric columns are always excluded.
#' 
#' The decimal separator character is the result of \code{getOption("OutDec")}, which is based on the user's locale.  If the default behavior is undesirable,
#' change this value ahead of calling the function, either by changing locale or with \code{options(OutDec = ",")}.  This aligns the decimal separator character with that used in \code{base::print()}.
#'
#' @param dat a data.frame with decimal values, typically the result of a call to \code{adorn_percentages} on a \code{tabyl}.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).
#' @param digits how many digits should be displayed after the decimal point?
#' @param rounding method to use for rounding - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @param affix_sign should the \% sign be affixed to the end?
#' @param ... columns to adorn.  This takes a tidyselect specification.  By default, all numeric columns (besides the initial column, if numeric) are adorned, but this allows you to manually specify which columns should be adorned, for use on a data.frame that does not result from a call to \code{tabyl}.
#'
#' @return a data.frame with formatted percentages
#' @export
#' @examples
#'
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages("col") %>%
#'   adorn_pct_formatting()
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
#'  adorn_percentages("col",,recovered:died) %>%
#'  adorn_pct_formatting(,,,recovered:died)
#'
adorn_pct_formatting <- function(dat, digits = 1, rounding = "half to even", affix_sign = TRUE, ...) {
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    purrr::map(dat, adorn_pct_formatting, digits, rounding, affix_sign)
  } else {
    # catch bad inputs
    if (!is.data.frame(dat)) {
      stop("adorn_pct_formatting() must be called on a data.frame or list of data.frames")
    }
    if (!rounding %in% c("half to even", "half up")) {
      stop("'rounding' must be one of 'half to even' or 'half up'")
    }
    original <- dat # used below to record original instances of NA and NaN

    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
    non_numeric_cols <- setdiff(1:ncol(dat), numeric_cols)
    numeric_cols <- setdiff(numeric_cols, 1) # assume 1st column should not be included so remove it from numeric_cols. Moved up to this line so that if only 1st col is numeric, the function errors
    
    if(rlang::dots_n(...) == 0){
      cols_to_adorn <- numeric_cols
    } else {
      expr <- rlang::expr(c(...))
      cols_to_adorn <- tidyselect::eval_select(expr, data = dat)
      if(any(cols_to_adorn %in% non_numeric_cols)){
        # don't need to print a message, adorn_rounding will
        cols_to_adorn <- setdiff(cols_to_adorn, non_numeric_cols)
      }
    }
    
    
    if ("one_way" %in% attr(dat, "tabyl_type")) {
      cols_to_adorn <- setdiff(numeric_cols, 2) # so that it works on a one-way tabyl
    }

    if (length(cols_to_adorn) == 0) {
      stop("at least one targeted column must be of class numeric")
    }

    dat[cols_to_adorn] <- lapply(dat[cols_to_adorn], function(x) x * 100)
    dat <- adorn_rounding(dat, digits = digits, rounding = rounding, ...)
    dat[cols_to_adorn] <- lapply(dat[cols_to_adorn], function(x) format(x,
                                                                        nsmall = digits,
                                                                        decimal.mark = getOption("OutDec"),
                                                                        trim = TRUE)) # so that 0% prints as 0.0% or 0.00% etc.
    if (affix_sign) {
      dat[cols_to_adorn] <- lapply(dat[cols_to_adorn], function(x) paste0(x, "%"))
    }
    dat[cols_to_adorn][is.na(original[cols_to_adorn])] <- "-" # NA and NaN values in the original should be simply "-" for printing of results
    dat
  }
}
