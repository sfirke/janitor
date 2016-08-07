#' @title Add formatting to a crosstabulation table.
#'
#' @description
#' Designed to run on the output of a call to \code{crosstab}, this adds formatting, percentage sign, Ns, and custom rounding to a table of numeric values.  The result is no longer clean data, but it saves time in reporting Ns and percentages at the same time.
#'
#' @param crosstab a data.frame with row names in the first column and numeric values in all other columns.  Usually the piped-in result of a call to  \code{crosstab} that included the argument \code{percent = "none"}.
#' @param denom the denominator to use for calculating percentages.  One of "row", "col", or "all".
#' @param show_n should counts be displayed alongside the percentages?
#' @param digits how many digits should be displayed after the decimal point?
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
adorn_crosstab <- function(crosstab, denom = "row", show_n = TRUE, digits = 1, rounding = "half to even"){
  # some input checks
  if(! denom %in% c("row", "col", "all")){stop("'denom' must be one of 'row', 'col', or 'all'")}
  if(! rounding %in% c("half to even", "half up")){stop("'rounding' must be one of 'half to even' or 'half up'")}
  
  n_col <- ncol(crosstab)
  
  # calculate %s
  percs <- crosstab
  if(denom == "row"){
    row_sum <- rowSums(crosstab[, 2:n_col], na.rm = TRUE)
    percs[, 2:n_col] <- percs[, 2:n_col] / row_sum 
  } else if(denom == "col"){
    col_sum <- colSums(crosstab[, 2:n_col], na.rm = TRUE)
    percs <- crosstab
    percs[, 2:n_col] <- sweep(percs[, 2:4], 2, col_sum,`/`) # from http://stackoverflow.com/questions/9447801/dividing-columns-by-colsums-in-r
  } else if(denom == "all"){
    all_sum <- sum(crosstab[, 2:n_col], na.rm = TRUE)
    percs[, 2:n_col] <- percs[, 2:n_col] / all_sum 
  }

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

# From http://stackoverflow.com/questions/12688717/round-up-from-5-in-r
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
  
  pasted <- matrix(paste(perc_matrix, " (", n_matrix, ")", sep = ""), 
                   nrow = nrow(n_matrix),
                   dimnames = dimnames(perc_matrix))
  
  pasted <- dplyr::as_data_frame(pasted)
  pasted[[1]] <- n_df[[1]] # undo the pasting in this 1st column
  pasted
}