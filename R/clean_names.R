#' @title Cleans names of an object (usually a data.frame).
#'
#' @description
#' Resulting names are unique and consist only of the \code{_} character, numbers, and letters.
#' Capitalization preferences can be specified using the \code{case} parameter.
#'
#' Accented characters are transliterated to ASCII.  For example, an "o" with a
#' German umlaut over it becomes "o", and the Spanish character "enye" becomes
#' "n".
#' 
#' This function takes and returns a data.frame, for ease of piping with
#' \code{`\%>\%`}. For the underlying function that works on a character vector
#' of names, see \code{\link[janitor]{make_clean_names}}.  \code{clean_names} 
#' relies on the versatile function \code{\link[snakecase]{to_any_case}}, which 
#' accepts many arguments.  See that function's documentation for ideas on getting 
#' the most out of \code{clean_names}.  A few examples are included below.
#'
#' @param dat the input data.frame.
#' @inheritDotParams make_clean_names -string
#' @return Returns the data.frame with clean names.
#' 
#' @details \code{clean_names()} is intended to be used on \code{data.frames}
#'   and \code{data.frame}-like objects. For this reason there are methods to
#'   support using \code{clean_names()} on \code{sf} and \code{tbl_graph} (from
#'   \code{tidygraph}) objects. For cleaning other named objects like named lists 
#'   and vectors, use \code{make_clean_names()}.
#' 
#' @export
#' @family Set names
#' @examples
#' 
#' # --- Simple Usage ---
#' x <- data.frame(caseID = 1, DOB = 2, Other = 3)
#' clean_names(x)
#'
#' # or pipe in the input data.frame:
#' x %>%
#'   clean_names()
#'
#' # if you prefer camelCase variable names:
#' x %>%
#'   clean_names(., "lower_camel")
#'
#' # (not run) run clean_names after reading in a spreadsheet:
#' # library(readxl)
#' # read_excel("messy_excel_file.xlsx") %>%
#' #   clean_names()
#' 
#' # --- Taking advantage of the underlying snakecase::to_any_case arguments ---
#' 
#' # Restore column names to Title Case, e.g., for plotting
#' mtcars %>%
#'   clean_names(case = "title")
#'   
#' # Tell clean_names to leave certain abbreviations untouched:
#' x %>%
#'   clean_names(case = "upper_camel", abbreviations = c("ID", "DOB")) 
#'   
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
  stop(
    "No `clean_names()` method exists for the class ", paste(class(dat), collapse=", "),
    "\nConsider janitor::make_clean_names() for other cases of manipulating vectors of names."
  )
}

#' @rdname clean_names
#' @export
clean_names.sf <- function(dat, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) { # nocov start
    stop(
      "Package 'sf' needed for this function to work. Please install it.",
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

#' @rdname clean_names
#' @export
#' @importFrom dplyr rename_all
clean_names.tbl_graph <- function(dat, ...) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) { # nocov start
    stop(
      "Package 'tidygraph' needed for this function to work. Please install it.", 
      call. = FALSE
    )
  } # nocov end
  dplyr::rename_all(dat, .funs=make_clean_names, ...)
}
