##### Helper functions for adorn_crosstab()
# should not be exported, except for round_half_up


#' @title Round a numeric vector; ties will be rounded up, ala Microsoft Excel. 
#'
#' @description
#' In base R \code{round()}, halves are rounded to even, e.g., 12.5 and 11.5 are both rounded to 12.  This function rounds 12.5 to 13 (assuming \code{digits = 0}).  Negative halves are rounded away from zero, e.g., -0.5 is rounded to -1.
#' 
#' This may skew subsequent statistical analysis of the data, but may be desirable in certain contexts.  This function is implemented exactly from \url{http://stackoverflow.com/a/12688836}; see that question and comments for discussion of this issue.
#'
#' @param x a numeric vector to round.
#' @param digits how many digits should be displayed after the decimal point?
#' @export
#' @examples
#' round_half_up(12.5, 0)
#' round_half_up(1.125, 2)
#' round_half_up(1.125, 1)
#' round_half_up(-0.5, 0) # negatives get rounded away from zero
#' 
round_half_up <- function(x, digits){
  posneg = sign(x)
  z = abs(x) * 10 ^ digits
  z = z + 0.5
  z = trunc(z)
  z = z / 10 ^ digits
  z * posneg
}

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