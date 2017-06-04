#' @title Append a totals row and/or column to a data.frame.
#'
#' @description
#' This function excludes the first column of the input data.frame, assuming it's a descriptive variable not to be summed.  It also excludes other non-numeric columns.
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param where one of "row", "col", or \code{c("row", "col")} 
#' @param fill if there are multiple non-numeric columns, what string should fill the bottom row of those columns?
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame augmented with a totals row, column, or both.  The data.frame is now also of class \code{tabyl} and stores information about the attached totals and underlying data in the tabyl attributes. 
#' @export
#' @examples
#' mtcars %>%
#'   crosstab(am, cyl) %>%
#'   adorn_totals()


adorn_totals <- function(dat, where = "row", fill = "-", na.rm = TRUE){
  numeric_cols <- which(unlist(lapply(dat, is.numeric)))
  numeric_cols <- setdiff(numeric_cols, 1) # assume 1st column should not be included so remove it from numeric_cols
  if(length(numeric_cols) == 0){stop("at least one one of columns 2:n must be of class numeric")}
  if(sum(where %in% c("row", "col")) != length(where)){ stop("\"where\" must be one of \"row\", \"col\", or c(\"row\", \"col\")") }
  
  if("grouped_df" %in% class(dat)){ dat <- dplyr::ungroup(dat) } # grouped_df causes problems, #97
  dat <- as_tabyl(dat)
  
  # set totals attribute
  if(sum(where %in% attr(dat, "totals")) > 0){ # if either of the values of "where" are already in totals attribute
    stop("trying to re-add a totals dimension that is already been added")
  } else if(length(attr(dat, "totals")) == 1){ # if totals row OR col has already been adorned, append new axis to the current attribute
    attr(dat, "totals") <- c(attr(dat, "totals"), where)
  } else{ attr(dat, "totals") <- where }
  
  if("row" %in% where){
    dat[[1]] <- as.character(dat[[1]]) # for type matching when binding the word "Total" on a factor when adding Totals row
    # creates the totals row to be appended
    col_vec <- function(a_col, na_rm = na.rm){
      if(is.numeric(a_col)){ # can't do this with if_else because it doesn't like the sum() of a character vector, even if that clause is not reached
        sum(a_col, na.rm = na_rm)
      } else {fill}
    }
    
    col_totals <- lapply(dat, col_vec) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      stats::setNames(names(dat))
    
    col_totals[nrow(col_totals), 1] <- "Total" # replace final row, first column with "Total"
    dat[(nrow(dat) + 1), ] <- col_totals[1, ] # insert totals_col as last row in dat
  }
  
  if("col" %in% where){
    # Add totals col
    clean_dat <- clean_names(dat) # bad names will make select_if choke; this may get fixed, see https://github.com/hadley/dplyr/issues/2243 but work around it for now w/ this line
    row_totals <- clean_dat %>%
      dplyr::select(-1) %>% # don't include the first column, even if numeric
      dplyr::select_if(is.numeric) %>%
      dplyr::transmute(Total = rowSums(., na.rm = na.rm))
    
    dat$Total <- row_totals$Total
  }
  
  dat
}

### Deprecated functions -----------------------------
#' @title Append a totals row to a data.frame.
#'
#' @description
#' This function is deprecated, use \code{adorn_totals} instead. 
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param fill if there are more than one non-numeric columns, what string should fill the bottom row of those columns?
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals row, consisting of "Total" in the first column and column sums in the others.
#' @export


add_totals_row <- function(dat, fill = "-", na.rm = TRUE){
  .Deprecated("adorn_totals(\"row\")")
  adorn_totals(dat, where = "row", fill = fill, na.rm = na.rm)
  
}

#' @title Append a totals column to a data.frame.
#'
#' @description
#' This function is deprecated, use \code{adorn_totals} instead.
#'
#' @param dat an input data.frame with at least one numeric column.
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @return Returns a data.frame with a totals column containing row-wise sums.
#' @export

add_totals_col <- function(dat, na.rm = TRUE){
  .Deprecated("adorn_totals(\"col\")")
  adorn_totals(dat, where = "col", fill = "-", na.rm = na.rm)
}

