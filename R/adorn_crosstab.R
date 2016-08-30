#' @title Add formatting to a crosstabulation table.
#'
#' @description
#' Designed to run on the output of a call to \code{crosstab}, this adds formatting, percentage sign, Ns, and custom rounding to a table of numeric values.  The result is no longer clean data, but it saves time in reporting Ns and percentages at the same time.
#'
#' @param crosstab a data.frame with row names in the first column and numeric values in all other columns.  Usually the piped-in result of a call to  \code{crosstab} that included the argument \code{percent = "none"}.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param show_n should counts be displayed alongside the percentages?
#' @param digits how many digits should be displayed after the decimal point?
#' @param show_totals display a totals summary? Will be a row, column, or both depending on the value of \code{denom}.
#' @param rounding method to use for truncating percentages - either "half to even", the base R default method, or "half up", where 14.5 rounds up to 15.
#' @return Returns a data.frame.
#' @examples
#' library(dplyr) # for the %>% pipe
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
adorn_crosstab <- function(crosstab, denom = "row", show_n = TRUE, digits = 1, show_totals = FALSE, rounding = "half to even"){
  # some input checks
  if(! rounding %in% c("half to even", "half up")){stop("'rounding' must be one of 'half to even' or 'half up'")}
  check_all_cols_after_first_are_numeric(crosstab)
  
  crosstab[[1]] <- as.character(crosstab[[1]]) # for type matching when binding the word "Total" on a factor
  
  showing_col_totals <- (show_totals & denom %in% c("col", "all"))
  showing_row_totals <- (show_totals & denom %in% c("row", "all"))
  
  complete_n <- complete_n <- sum(crosstab[, -1], na.rm = TRUE) # capture for percent calcs before any totals col/row is added
  
  if(showing_col_totals){ crosstab <- add_totals_col(crosstab) }
  if(showing_row_totals){ crosstab <- add_totals_row(crosstab) }
  n_col <- ncol(crosstab)
  
  percs <- ns_to_percents(crosstab, denom, total_n = complete_n) # last argument only gets used in the "all" case = no harm in passing otherwise
  
  # round %s using specified method, add % sign
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(. * 100)) # since we'll be adding % sign - do this before rounding
  if(rounding == "half to even"){ percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(round(., digits))) }
  else if(rounding == "half up"){ percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(round_half_up(., digits)))}
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(format(., nsmall = digits, trim = TRUE))) # so that 0% prints as 0.0% or 0.00% etc.
  percs <- dplyr::mutate_at(percs, dplyr::vars(2:n_col), dplyr::funs(paste0(., "%")))

  
    # paste Ns if needed
  if(show_n){
    result <- paste_ns(percs, crosstab)
  } else{ result <- percs}
  
  as.data.frame(result) # drop back to data.frame from tibble
  
}


##### Helper functions

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

# check that all columns in a data.frame beyond the first one are numeric
check_all_cols_after_first_are_numeric <- function(x){
  non_numeric_count <- x %>%
    dplyr::select(-1) %>%
    lapply(function(x) !is.numeric(x)) %>%
    unlist %>%
    sum
  if(non_numeric_count > 0){
  stop("all columns after the first one must be numeric")
  }
}
