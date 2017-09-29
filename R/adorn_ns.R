#' @title Add underlying Ns to a tabyl displaying percentages.
#'
#' @description
#' This function adds back the underlying Ns to a \code{tabyl} whose percentages were calculated using \code{adorn_percentages()}, to display the Ns and percentages together.  
#'
#' @param dat a data.frame of class \code{tabyl}, typically the result of a call to \code{adorn_percentages} on a \code{tabyl}.
#' @param position should the N go in the front, or in the rear, of the percentage? 
#' 
#' @return a data.frame with Ns appended
#' @export
#' @examples
#' 
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages("col") %>%
#'   adorn_pct_formatting() %>%
#'   adorn_ns(position = "front")

adorn_ns <- function(dat, position = "rear"){
  #TODO: validate inputs
  if(! position %in% c("rear", "front")){stop("\"position\" must be one of \"front\" or \"rear\"")}
  if(! "tabyl" %in% class(dat)){stop("adorn_ns() can only be called on a data.frame of class \"tabyl\"")}
  if(! "two_way" %in% attr(dat, "tabyl_type")){stop("adorn_ns() can only be called on a two_way tabyl; consider combining columns of a one_way tabyl with tidyr::unite()")}
  attrs <- attributes(dat) # save these to re-append later
  
  ns <- attr(dat, "core")
  if(!is.null(attr(dat, "totals"))){ # add totals row/col to core for pasting, if applicable
    ns <- adorn_totals(ns, attr(dat, "totals"))
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