##### Helper functions for adorn_crosstab()
# should not be exported

# From http://stackoverflow.com/a/12688836/4470365
round_half_up <- function(x, n){
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# takes data.frames of Ns and %s, pastes them together
paste_ns <- function(perc_df, n_df){
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