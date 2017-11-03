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

adorn_col_title <- function(dat, placement = "top"){
  #TODO: validate inputs
  if(! placement %in% c("top", "combined")){stop("\"placement\" must be one of \"top\" or \"combined\"")}
  
  ######## CONTINUE HERE ####################
  if(is.null(ns)){stop("argument \"ns\" cannot be null; if not calling adorn_ns() on a data.frame of class \"tabyl\", pass your own value for ns")}
  if("one_way" %in% attr(dat, "tabyl_type")){warning("adorn_ns() is meant to be called on a two_way tabyl; consider combining columns of a one_way tabyl with tidyr::unite()")}
  attrs <- attributes(dat) # save these to re-append later
  
  # If ns argument is not the default "core" attribute, validate that it's a data.frame and has correct right dimensions
  if(!is.data.frame(ns)){ stop("if supplying a value to the ns argument, it must be of class data.frame") }
  if((!identical(ns, attr(dat, "core"))) & !identical(dim(ns), dim(dat))){ # user-supplied Ns must include values for totals row/col if present 
    stop("if supplying your own data.frame of Ns to append, its dimensions must match those of the data.frame in the \"dat\" argument")
  }
  
  # If appending the default Ns from the core, and there are totals rows/cols, append those values to the Ns table
  # Custom inputs to ns argument will need to calculate & format their own totals row/cols 
  if(identical(ns, attr(dat, "core"))){
    if(!is.null(attr(dat, "totals"))){ # add totals row/col to core for pasting, if applicable
      ns <- adorn_totals(ns, attr(dat, "totals"))
    }
  }

  if(position == "rear"){
    result <- paste_matrices(dat, ns%>%
                               dplyr::mutate_all(as.character) %>%
                               dplyr::mutate_all(wrap_parens) %>%
                               dplyr::mutate_all(standardize_col_width))
    
  } else if(position == "front"){
    result <- paste_matrices(ns, dat %>%
                               dplyr::mutate_all(as.character) %>%
                               dplyr::mutate_all(wrap_parens) %>%
                               dplyr::mutate_all(standardize_col_width))
  }
  attributes(result) <- attrs
  result
}

### Helper functions called by adorn_ns

# takes two matrices, pastes them together, keeps spacing of the two columns aligned
paste_matrices <- function(front, rear){
  front_matrix <- as.matrix(front)
  rear_matrix <- as.matrix(rear)
  
  # paste the results together
  pasted <- paste(front_matrix, " ", rear_matrix, sep = "") %>% # paste the matrices
    matrix(., nrow = nrow(front_matrix), dimnames = dimnames(rear_matrix)) %>% # cast as matrix, then data.frame
    dplyr::as_data_frame(pasted)
  pasted[[1]] <- front[[1]] # undo the pasting in this 1st column
  pasted
}


# Padding function to standardize a column's width by pre-pending whitespace 
standardize_col_width <- function(x){
  width = max(nchar(x))
  sprintf(paste0("%", width, "s"), x)
}

# Wrap a string in parentheses
wrap_parens <- function(x){
  paste0("(", x, ")")
}