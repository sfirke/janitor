#' @title Add underlying Ns to a tabyl displaying percentages.
#'
#' @description
#' If percentages were calculated using \code{adorn_percentages()}, the result is a \code(tabyl) that retains its underlying values.  This function adds those Ns back, for displaying both Ns and percentages together.  
#'
#' @param dat a data.frame with decimal values, typically the result of a call to \code{adorn_percentages} on a \code{tabyl}.
#' @param digits how many digits should be displayed after the decimal point?
#' @param rounding method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15. 
#' @param affix_sign should the % sign be affixed to the end?
#' 
#' @return a data.frame with Ns appended
#' @export
#' @examples
#' 
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_percentages("col") %>%
#'   adorn_pct_formatting() %>%
#'   adorn_ns(position = "first")

adorn_ns <- function(dat, position = "rear"){
  #TODO: validate inputs
  ns <- attr(dat, "core")
  if(!is.null(attr(dat, "totals"))){ # add totals row/col to core for pasting, if applicable
    ns <- adorn_totals(ns, attr(dat, "totals"))
  }
  
  attrs <- attributes(dat) # save these to re-append later
  
  if(position == "rear"){
    result <- paste_ns(dat, ns)
  } else if(position == "front"){
    result <- paste_ns(ns, dat)
  }
  attributes(result) <- attrs
  result
}


### Helper functions called by adorn_ns

# takes data.frames of Ns and %s, pastes them together
paste_ns <- function(perc_df, n_df){
  n_matrix <- as.matrix(n_df)
  perc_matrix <- as.matrix(perc_df)
  
  # paste the results together
  pasted <- paste(perc_matrix, " (", n_matrix, ")", sep = "") %>% # paste the matrices
    sapply(., fix_parens_whitespace) %>% # apply the whitespace cleaning function to the resulting vector
    matrix(., nrow = nrow(n_matrix), dimnames = dimnames(perc_matrix)) %>% # cast as matrix, then data.frame
    dplyr::as_data_frame(pasted)
  
  pasted[[1]] <- n_df[[1]] # undo the pasting in this 1st column
  pasted
}

# converts "50.0% ( 1)" to "50.0%  (1)" for nice printing 
fix_parens_whitespace <- function(x){
  culprit <- regmatches(x, regexpr("[(][ ]+", x)) # isolate the problematic string
  
  # if no problem, return unmodified
  if(length(culprit) == 0){ x }
  
  else{
    num_spaces <- length(gregexpr(" ", culprit)[[1]])
    gsub(culprit[[1]],
         paste0( # create replacement string
           paste0(rep(" ", num_spaces), collapse = ""), # generate the spaces
           "(",
           collapse = ""),
         x,
         fixed = TRUE)
  }
  
}