#' @title Append a totals row and/or column to a data.frame.
#'
#' @description
#' This function defaults to excluding the first column of the input data.frame, assuming that it contains a descriptive variable, but this can be overridden by specifying the columns to be totaled in the \code{...} argument.  Non-numeric columns are converted to character class and have a user-specified fill character inserted in the totals row.
#'
#' @param dat an input data.frame with at least one numeric column.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).
#' @param where one of "row", "col", or \code{c("row", "col")}
#' @param fill if there are non-numeric columns, what should fill the bottom row of those columns? If a string, relevant columns will be coerced to character. If `NA` then column types are preserved. 
#' @param na.rm should missing values (including NaN) be omitted from the calculations?
#' @param name name of the totals row and/or column.  If both are created, and \code{name} is a single string, that name is applied to both. If both are created and \code{name} is a vector of length 2, the first element of the vector will be used as the row name (in column 1), and the second element will be used as the totals column name. Defaults to "Total".
#' @param ... columns to total.  This takes a tidyselect specification.  By default, all numeric columns (besides the initial column, if numeric) are included in the totals, but this allows you to manually specify which columns should be included, for use on a data.frame that does not result from a call to \code{tabyl}. 
#' @return Returns a data.frame augmented with a totals row, column, or both.  The data.frame is now also of class \code{tabyl} and stores information about the attached totals and underlying data in the tabyl attributes.
#' @export
#' @examples
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_totals()


adorn_totals <- function(dat, where = "row", fill = "-", na.rm = TRUE, name = "Total", ...) {
  if("both" %in% where){
    where <- c("row", "col")
  }
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    purrr::map(dat, adorn_totals, where, fill, na.rm, name)
  } else {
    if (!is.data.frame(dat)) {
      stop("adorn_totals() must be called on a data.frame or list of data.frames")
    }
    
    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
    non_numeric_cols <- setdiff(1:ncol(dat), numeric_cols)
    
    if (rlang::dots_n(...) == 0) {
      numeric_cols <- setdiff(numeric_cols, 1) # by default 1st column is not totaled so remove it from numeric_cols and add to non_numeric_cols
      non_numeric_cols <- unique(c(1, non_numeric_cols))
      cols_to_total <- numeric_cols
    } else {
      expr <- rlang::expr(c(...))
      cols_to_total <- tidyselect::eval_select(expr, data = dat)
      if (any(cols_to_total %in% non_numeric_cols)) {
        cols_to_total <- setdiff(cols_to_total, non_numeric_cols)
      }
    }
    
    if (length(cols_to_total) == 0) {
      stop("at least one targeted column must be of class numeric.  Control target variables with the ... argument. adorn_totals should be called before other adorn_ functions.")
    }
    
    if (sum(where %in% c("row", "col")) != length(where)) {
      stop("\"where\" must be one of \"row\", \"col\", or c(\"row\", \"col\")")
    }
    
    if (length(name) == 1) name <- rep(name, 2)

    
    # grouped_df causes problems, #97
    if ("grouped_df" %in% class(dat)) {
      dat <- dplyr::ungroup(dat)
    }
    
    dat <- as_tabyl(dat) # even a tabyl needs to be recast as a tabyl to reset the core in case it's been sorted
    
    # set totals attribute
    if (sum(where %in% attr(dat, "totals")) > 0) { # if either of the values of "where" are already in totals attribute
      stop("trying to re-add a totals dimension that is already been added")
    } else if (length(attr(dat, "totals")) == 1) { # if totals row OR col has already been adorned, append new axis to the current attribute
      attr(dat, "totals") <- c(attr(dat, "totals"), where)
    } else {
      attr(dat, "totals") <- where
    }
    
    if ("row" %in% where) {
      # capture factor levels if relevant, #494
      factor_input <- is.factor(dat[[1]])
      if(factor_input) {
        col1_backup <- dat[[1]][1]
      }
      # creates the totals row to be appended
      col_sum <- function(a_col, na_rm = na.rm) {
        if (is.numeric(a_col)) { # can't do this with if_else because it doesn't like the sum() of a character vector, even if that clause is not reached
          sum(a_col, na.rm = na_rm)
        } else {
          if (!is.character(fill)) { #if fill isn't a character string, use NA consistent with data types
            switch(typeof(a_col),
                   "character" = NA_character_,
                   "integer" = NA_integer_,
                   "double" = if(inherits(a_col, "Date") || inherits(a_col, "POSIXt")) {
                     as.Date(NA_real_, origin = "1970-01-01")
                   } else {
                     NA_real_
                   },
                   "complex" = NA_complex_,
                   NA)
          } else {
            fill # otherwise just use the string provided
          }
        }
      }
      
      if (is.character(fill)) { # if fill is a string, keep original implementation
        col_totals <- purrr::map_df(dat, col_sum)
        not_totaled_cols <- setdiff(1:length(col_totals), cols_to_total)
        col_totals[not_totaled_cols] <- fill # reset numeric columns that weren't to be totaled
        dat[not_totaled_cols] <- lapply(dat[not_totaled_cols], as.character) # type compatibility for bind_rows
      } else { 
        
        cols_idx <- seq_along(dat) # get col indexes
        names(cols_idx) <- names(dat) # name them using dat names
        
        col_totals <- purrr::map_df(cols_idx, function(i) {
          if (is.numeric(dat[[i]]) && !i %in% cols_to_total) { # check if numeric and not to be totaled
            switch(typeof(dat[[i]]), # and set to NA
                   "integer" = NA_integer_,
                   "double" = NA_real_,
                   NA)
          } else { # otherwise run col_sum on the rest
            col_sum(dat[[i]])
          }
        })
        
        if (!is.character(dat[[1]]) && !1 %in% cols_to_total) { # convert first col to character so that name can be appended
          dat[[1]] <- as.character(dat[[1]])
          col_totals[[1]] <- as.character(col_totals[[1]])
        }
        
      }
      
      if (! 1 %in% cols_to_total) { # give users the option to total the first column??  Up to them I guess
        col_totals[1, 1] <- name[1] # replace first column value with name argument
      } else {
        message("Because the first column was specified to be totaled, it does not contain the label 'Total' (or user-specified name) in the totals row")
      }
      dat[(nrow(dat) + 1), ] <- col_totals[1, ] # insert totals_col as last row in dat
      if(factor_input) { # restore factor/ordered info, #494
        dat[[1]] <- factor(dat[[1]], levels = c(levels(col1_backup), name[1]), ordered = is.ordered(col1_backup))
      }
    }
    
    if ("col" %in% where) {
      # Add totals col
      row_totals <- dat %>%
        dplyr::select(dplyr::all_of(cols_to_total)) %>%
        dplyr::select_if(is.numeric) %>%
        dplyr::transmute(Total = rowSums(., na.rm = na.rm))
      
      dat[[name[2]]] <- row_totals$Total
    }
    
    dat
  }
}
