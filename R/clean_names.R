#' @title Cleans names of a data.frame.
#'
#' @description
#' 
#' \code{clean_names()} takes a \code{data.frame} and alters the column names to adhere to  a specified capitalization preference.
#' 
#' Resulting names are unique and consist only of the \code{_} character, numbers, and letters.
#' Capitalization preferences can be specified using the \code{case} parameter.
#'
#' Accented characters are
#' transliterated to ASCII.  For example, an "o" with a German umlaut over it becomes "o", and the Spanish character "enye" becomes "n".
#' 
#' This function takes and returns a data.frame, for ease of piping with  \code{`\%>\%`}.  
#' For the underlying function that works on a character vector of names,
#' see \code{\link[janitor]{make_clean_names}}. 
#'
#' @param dat the input data.frame.
#' @inheritDotParams make_clean_names -string
#' @return Returns the data.frame with clean names.
#' 
#' @details \code{clean_names()} is intended to be used on \code{data.frames} and \code{data.frame} like objects. For this reason there are methods to support using \code{clean_names()} on \code{sf} and \code{tbl_graph} (from \code{tidygraph}) objects. For cleaning named lists and vectors, consider using \code{make_clean_names()}.
#' 
#' 
#' @export
#' @examples
#' # not run:
#' # clean_names(poorly_named_df)
#'
#' # or pipe in the input data.frame:
#' # poorly_named_df %>% clean_names()
#'
#' # if you prefer camelCase variable names:
#' # poorly_named_df %>% clean_names(., "small_camel")
#'
#' # not run:
#' # library(readxl)
#' # read_excel("messy_excel_file.xlsx") %>% clean_names()

# create new clean_names method
clean_names <- function(dat, ...) {
  UseMethod("clean_names")
}

#' @rdname clean_names
#' @export
clean_names.data.frame <- function(dat, ...) {
  stats::setNames(dat, make_clean_names(names(dat), ...))
}

#' @rdname clean_names
#' @export
clean_names.default <- function(dat, ...) {
  stop( "clean_names() must be called on a data.frame.  Consider janitor::make_clean_names() for other cases of manipulating vectors of names.") 
}

#' @rdname clean_names
#' @export
# create method for sf object
clean_names.sf <- function(dat, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) { # nocov start
    stop(
      "Package \"sf\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  } # nocov end
  # get old names
  sf_names <- names(dat) 
  # identify ending column index to clean
  n_cols <- length(dat)-1 
  # clean all but last column
  sf_cleaned <- make_clean_names(sf_names[1:n_cols], ...) 
  # rename original df
  names(dat)[1:n_cols] <- sf_cleaned 
  
  return(dat)
}


#' @export
# create method for tbl_graph objects

clean_names.tbl_graph <- function(dat, case = c(
  "snake", "lower_camel", "upper_camel", "screaming_snake",
  "lower_upper", "upper_lower", "all_caps", "small_camel",
  "big_camel", "old_janitor", "parsed", "mixed"
)) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) { 
    stop("Package \"tidygraph\" needed for this function to work. Please install it.", 
         call. = FALSE) 
  } 
  
  dplyr::rename_all(dat, make_clean_names)
}
