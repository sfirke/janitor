#' @title Add column name to the top of a two-way tabyl.
#'
#' @description
#' This function adds the column variable name to the top of a \code{tabyl} for a complete display of information.  This makes the tabyl prettier, but renders the data.frame less useful for further manipulation.
#'
#' @param dat a data.frame of class \code{tabyl}.
#' @param placement whether the column name should be added to the top of the tabyl in an otherwise-empty row \code{"top"} or appended to the already-present row name variable (\code{"combined"}).  The formatting in the \code{"top"} option has the look of base R's \code{table()}; it also wipes out the other column names, making it hard to further use the data.frame besides formatting it for reporting.  The \code{"combined"} option is more conservative in this regard.
#' @return the input tabyl, augmented with the column title. 
#' @export
#' @examples
#' 
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_col_title(placement = "top")

adorn_col_title <- function(dat, placement = "top", col_title = NULL){
  #TODO: validate inputs
  #TODO: validate and use col_title input
  if(! placement %in% c("top", "combined")){stop("\"placement\" must be one of \"top\" or \"combined\"")}
  
  col_var <- attr(dat, "var_names")$col
  
  if(placement == "top"){
    top <- dat[1, ]
    top[1, ] <- names(top)
    
    out <- bind_rows(
      top, dat %>%
        mutate_if(is.numeric, as.character)
    ) %>%
      setNames(c("", col_var, "", ""))
  }
  if(placement == "combined"){
    row_var <- attr(dat, "var_names")$row
    out <- dat
    names(out)[1] <- paste(row_var, col_var, sep = "/")
  }
  out
}