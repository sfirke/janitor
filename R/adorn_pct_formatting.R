#' @title Format a data.frame of decimals as percentages.
#'
#' @description
#' Numeric columns get multiplied by 100 and formatted as percentages according to user specifications.  This function excludes the first column of the input data.frame, assuming that it contains a descriptive variable.  Other non-numeric columns are also excluded.
#'
#' @param dat a data.frame with decimal values, typically the result of a call to \code{adorn_percentages} on a \code{tabyl}.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).
#' @param digits how many digits should be displayed after the decimal point?
#' @param rounding method to use for rounding - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @param affix_sign should the \% sign be affixed to the end?
#' 
#' @return a data.frame with formatted percentages
#' @export
#' @examples
#' 
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages("col") %>%
#'   adorn_pct_formatting()

adorn_pct_formatting <- function(dat, digits = 1, rounding = "half to even", affix_sign = TRUE){
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if(is.list(dat) & !is.data.frame(dat)){
    purrr::map(dat, adorn_pct_formatting, digits, rounding, affix_sign)
  } else{
    # catch bad inputs
    if(!is.data.frame(dat)){ stop("adorn_pct_formatting() must be called on a data.frame or list of data.frames") }
    if(! rounding %in% c("half to even", "half up")){stop("'rounding' must be one of 'half to even' or 'half up'")}
    original <- dat # used below to record original instances of NA and NaN
    numeric_cols <- which(unlist(lapply(dat, is.numeric)))
    numeric_cols <- setdiff(numeric_cols, 1) # assume 1st column should not be included so remove it from numeric_cols
    if("one_way" %in% attr(dat, "tabyl_type")){
      numeric_cols <- setdiff(numeric_cols, 2) # so that it works on a one-way tabyl
    }
    
    if(sum(unlist(lapply(dat, is.numeric))[-1]) == 0){ stop("at least one one of columns 2:n must be of class numeric") }
    
    dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) x * 100)
    dat[numeric_cols] <- adorn_rounding(dat[numeric_cols], digits = digits, rounding = rounding, skip_first_col = FALSE)
    dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) format(x, nsmall = digits, trim = TRUE)) # so that 0% prints as 0.0% or 0.00% etc.
    if(affix_sign){ dat[numeric_cols] <- lapply(dat[numeric_cols], function(x) paste0(x, "%")) }
    dat[numeric_cols][is.na(original[numeric_cols])] <- "-" # NA and NaN values in the original should be simply "-" for printing of results
    dat
  }
}