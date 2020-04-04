#' @title Append a totals row and/or column to a data.frame.
#'
#' @description
#' This function defaults to excluding the first column of the input data.frame, assuming that it contains a descriptive variable, but this can be overridden by specifying the columns to be totaled in the \code{...} argument.  Non-numeric columns are converted to character class and have a user-specified fill character inserted in the totals row.
#'
#' @param dat an input data.frame with at least one numeric column.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).
#' @param where one of "row", "col", or \code{c("row", "col")}
#' @param fill if there are non-numeric columns, what string should fill the bottom row of those columns?
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @param name name of the totals column or row 
#' @param ... columns to total.  This takes a tidyselect specification.  By default, all numeric columns (besides the initial column, if numeric) are included in the totals, but this allows you to manually specify which columns should be included, for use on a data.frame that does not result from a call to \code{tabyl}. 
#' @return Returns a data.frame augmented with a totals row, column, or both.  The data.frame is now also of class \code{tabyl} and stores information about the attached totals and underlying data in the tabyl attributes.
#' @export
#' @examples
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_totals()


adorn_totals <- function(dat, where = "row", fill = "-", na.rm = TRUE, name = "Total", ...) {
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    purrr::map(dat, adorn_totals, where, fill, na.rm, name)
  } else {
    if (!is.data.frame(dat)) {
      stop("adorn_totals() must be called on a data.frame or list of data.frames")
    }
    
    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
    non_numeric_cols <- setdiff(1:ncol(dat), numeric_cols)
     
    if(rlang::dots_n(...) == 0){
      numeric_cols <- setdiff(numeric_cols, 1) # by default 1st column is not totaled so remove it from numeric_cols and add to non_numeric_cols
      non_numeric_cols <- unique(c(1, non_numeric_cols))
      cols_to_total <- numeric_cols
    } else {
      expr <- rlang::expr(c(...))
      cols_to_total <- tidyselect::eval_select(expr, data = dat)
      if(any(cols_to_total %in% non_numeric_cols)){
        cols_to_total <- setdiff(cols_to_total, non_numeric_cols)
      }
    }
    
    if (length(cols_to_total) == 0) {
      stop("at least one targeted column must be of class numeric.  Control target variables with the ... argument. adorn_totals should be called before other adorn_ functions.")
    }
    
    if (sum(where %in% c("row", "col")) != length(where)) {
      stop("\"where\" must be one of \"row\", \"col\", or c(\"row\", \"col\")")
    }

    # grouped_df causes problems, #97
    if ("grouped_df" %in% class(dat)) {
      dat <- dplyr::ungroup(dat)
    }
    dat <- as_tabyl(dat)

    # set totals attribute
    if (sum(where %in% attr(dat, "totals")) > 0) { # if either of the values of "where" are already in totals attribute
      stop("trying to re-add a totals dimension that is already been added")
    } else if (length(attr(dat, "totals")) == 1) { # if totals row OR col has already been adorned, append new axis to the current attribute
      attr(dat, "totals") <- c(attr(dat, "totals"), where)
    } else {
      attr(dat, "totals") <- where
    }

    if ("row" %in% where) {
      # to allow binding of "Total" and "-" onto date & factor columns 
      dat[non_numeric_cols] <- lapply(dat[non_numeric_cols], as.character)
      
      # creates the totals row to be appended
      col_sum <- function(a_col, na_rm = na.rm) {
        if (is.numeric(a_col)) { # can't do this with if_else because it doesn't like the sum() of a character vector, even if that clause is not reached
          sum(a_col, na.rm = na_rm)
        } else {
          fill
        }
      }

      col_totals <- purrr::map_df(dat, col_sum)
      col_totals[setdiff(1:length(col_totals), cols_to_total)] <- fill # reset numeric columns that weren't to be totaled
      if(! 1 %in% cols_to_total){ # give users the option to total the first column??  Up to them I guess
        col_totals[1, 1] <- name # replace first column value with name argument
      } else {
        message("Because the first column was specified to be totaled, it does not contain the label 'Total' (or user-specified name) in the totals row")
      }
      dat[(nrow(dat) + 1), ] <- col_totals[1, ] # insert totals_col as last row in dat
    }

    if ("col" %in% where) {
      # Add totals col
      clean_dat <- clean_names(dat) # bad names will make select_if choke; this may get fixed, see https://github.com/hadley/dplyr/issues/2243 but work around it for now w/ this line
      row_totals <- clean_dat %>%
        dplyr::select(cols_to_total) %>%
        dplyr::select_if(is.numeric) %>%
        dplyr::transmute(Total = rowSums(., na.rm = na.rm))

      dat[[name]] <- row_totals$Total
    }

    dat
  }
}