#' @title Cleans names of a data.frame.
#'
#' @description
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
#' @param case The desired target case (default is \code{"snake"}), indicated by these possible values:
#' \itemize{
#'  \item{\code{"snake"} produces snake_case}
#'  \item{\code{"lower_camel"} or \code{"small_camel"} produces lowerCamel}
#'  \item{\code{"upper_camel"} or \code{"big_camel"} produces UpperCamel}
#'  \item{\code{"screaming_snake"} or \code{"all_caps"} produces ALL_CAPS}
#'  \item{\code{"lower_upper"} produces lowerUPPER}
#'  \item{\code{"upper_lower"} produces UPPERlower}
#'  \item{\code{old_janitor}: legacy compatibility option to preserve behavior of \code{clean_names} prior to addition of the "case" argument(janitor versions <= 0.3.1 )}.  Provided as a quick fix for old scripts broken by the changes to \code{clean_names} in janitor v1.0.
#'  \item{\code{"parsed"}, \code{"mixed"}, \code{"none"}, \code{"internal_parsing"}: less-common cases offered by \code{snakecase::to_any_case}.  See \code{\link[snakecase]{to_any_case}} for details.}
#'  }
#'
#' @return Returns the data.frame with clean names.
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

clean_names <- function(dat, case = c(
                        "snake", "lower_camel", "upper_camel", "screaming_snake",
                        "lower_upper", "upper_lower", "all_caps", "small_camel",
                        "big_camel", "old_janitor", "parsed", "mixed"
                      )) {
  if(!is.data.frame(dat)){stop( "clean_names() must be called on a data.frame.  Consider janitor::make_clean_names() for other cases of manipulating vectors of names.") }
  stats::setNames(dat, make_clean_names(names(dat), case = case))
}

# sf method for clean names

clean_names.sf <- function(x, ...) {
    ret <- clean_names(x)
    st_geometry(ret) <- names(ret)[ncol(ret)] # geometry is always last column
    ret
}


