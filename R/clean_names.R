#' @title Cleans names of a data.frame.
#'
#' @description
#' Resulting names are unique and consist only of the \code{_} character, numbers and letters regarding the specified \code{case}.
#' 
#' Accented characters are
#' transliterated to ASCII.  For example, an "o" with a german umlaut over it becomes "o", and the Spanish character "enye" becomes "n". 
#'
#' @param dat the input data.frame.
#' @param case The desired target case (default is \code{"snake"}), provided as one of the following:
#' \itemize{
#'  \item{snake_case: \code{"snake"}}
#'  \item{lowerCamel: \code{"lower_camel"} or \code{"small_camel"}}
#'  \item{UpperCamel: \code{"upper_camel"} or \code{"big_camel"}}
#'  \item{ALL_CAPS: \code{"screaming_snake"} or \code{"all_caps"}}
#'  \item{lowerUPPER: \code{"lower_upper"}}
#'  \item{UPPERlower: \code{"upper_lower"}}
#'  }
#'
#'  There are three "special" cases available:
#' \itemize{
#'  \item{\code{"parsed"}: Every substring a string consists of, surrounded by an underscore. No lower or 
#'  upper case pattern from the input string are changed.}
#'  \item{\code{"mixed"}: Almost the same as \code{case = "parsed"}. Every letter which is not at the start
#'  or behind an underscore is turned into lowercase.}
#'  \item{\code{"none"}: This case is just available as an artifact from the underlying snakecase-pkg, but shouldn't be called within the context of \code{clean_names()}.}
#'  }
#'  
#' @return Returns the data.frame with clean names.
#' @export
#' @examples
#' # not run:
#' # clean_names(poorly_named_df)
#' 
#' # or with the pipe character from dplyr:
#' # poorly_named_df %>% clean_names()
#'
#' # not run:
#' # library(readxl)
#' # readxl("messy_excel_file.xlsx") %>% clean_names()

clean_names <- function(dat, case = "snake"){

  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("'", "", .) %>% # remove quotation marks
    gsub("\"", "", .) %>% # remove quotation marks
    gsub("%", ".percent_", .) %>% # starting with "." as a workaround, to make
    # ".percent" a valid name. The "." will be replaced in the call to to_any_case
    # via the preprocess argument anyway.
    gsub("^[ ]+", "", .) %>%
    make.names(.) %>%
    # Handle dots, multiple underscores, case conversion, string transliteration
    snakecase::to_any_case(case = case, preprocess = "\\.", 
                replace_special_characters = c("Latin-ASCII"))
  
  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- vapply(1:length(new_names), function(i) { 
    sum(new_names[i] == new_names[1:i]) }, integer(1))
  
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}
