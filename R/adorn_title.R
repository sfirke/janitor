#' @title Add column name to the top of a two-way tabyl.
#'
#' @description
#' This function adds the column variable name to the top of a \code{tabyl} for a complete display of information.  This makes the tabyl prettier, but renders the data.frame less useful for further manipulation.
#'
#' @param dat a data.frame of class \code{tabyl}.
#' @param placement whether the column name should be added to the top of the tabyl in an otherwise-empty row \code{"top"} or appended to the already-present row name variable (\code{"combined"}).  The formatting in the \code{"top"} option has the look of base R's \code{table()}; it also wipes out the other column names, making it hard to further use the data.frame besides formatting it for reporting.  The \code{"combined"} option is more conservative in this regard.
#' @param row_name (optional) default behavior is to pull the row name from the attributes of the input `tabyl` object.  If you wish to override that text, or if your input is not a `tabyl`, supply a string here.  
#' @param col_name (optional) default behavior is to pull the column_name from the attributes of the input `tabyl` object.  If you wish to override that text, or if your input is not a `tabyl`, supply a string here.  
#' @return the input tabyl, augmented with the column title. 
#' @export
#' @examples
#' 
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_title(placement = "top")
#'   
#' # Adding a title to a non-tabyl
#' library(tidyr); library(dplyr)
#' mtcars %>%
#'   group_by(gear, am) %>%
#'   summarise(avg_mpg = mean(mpg)) %>%
#'   spread(gear, avg_mpg) %>%
#'   adorn_title("top", row_name = "Gears", col_name = "Cylinders")

adorn_title <- function(dat, placement = "top", row_name, col_name){
  if(!is.data.frame(dat)){stop("\"dat\" must be a data.frame")}
  if(! placement %in% c("top", "combined")){stop("\"placement\" must be one of \"top\" or \"combined\"")}
  
  if(missing(col_name)){
    if(! "tabyl" %in% class(dat)){stop("When input is not a data.frame of class tabyl, a value must be specified for the col_name argument")}
    col_var <- attr(dat, "var_names")$col
  } else{
    if(!is.character(col_name)){stop("col_name must be a string")}
    col_var <- col_name
  }
  
  if(!missing(row_name)){
    if(!is.character(row_name)){stop("row_name must be a string")}
    names(dat)[1] <- row_name
    row_var <- row_name
  } else{
    if("tabyl" %in% class(dat)){
      row_var <- attr(dat, "var_names")$row
    } else {
      row_var <- names(dat)[1] # for non-tabyl input, if no row_name supplied, use first existing name 
    }
  }
  
  if(placement == "top"){
    top <- dat[1, ]
    top[1, ] <- names(top)
    
    out <- dplyr::bind_rows(
      top, dat %>%
        dplyr::mutate_if(is.numeric, as.character)
    )
    out <- stats::setNames(out, c("", col_var, rep("", ncol(out)-2)))
  }
  if(placement == "combined"){
    out <- dat
    names(out)[1] <- paste(row_var, col_var, sep = "/")
  }
  as.data.frame(out) # "top" text isn't printing if input (and thus the output) is a tibble
}
