#' @title Cleans names of a data.frame.
#'
#' @description
#' Resulting names are unique and consist only of the \code{_} character, numbers, and letters.
#' Capitalization preferences can be specified using the \code{case} parameter.
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
#'  \item{old_janitor: legacy compatibility option to preserve behavior of \code{clean_names} in janitor versions <= 0.3.1 (prior to addition of the "case" argument)}
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
#' # if you prefer camelCase variable names:
#' # poorly_named_df %>% clean_names(., "small_camel")
#'
#' # not run:
#' # library(readxl)
#' # readxl("messy_excel_file.xlsx") %>% clean_names()

clean_names <- function(dat, case = c("snake", "small_camel", "big_camel", "screaming_snake", 
                                      "parsed", "mixed", "lower_upper", "upper_lower",
                                      "all_caps", "lower_camel", "upper_camel", "old_janitor")) {
  
  # old behavior, to provide easy fix for people whose code breaks with the snakecase integration
  case <- match.arg(case)
  if (case == "old_janitor"){
    return(old_clean_names(dat))  
  }
  
  ### new behaviour with snakecase integration
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("'", "", .) %>% # remove single quotation marks
    gsub("\"", "", .) %>% # remove double quotation marks
    gsub("%", ".percent_", .) %>% # starting with "." as a workaround, to make
    # ".percent" a valid name. The "." will be replaced in the call to to_any_case
    # via the preprocess argument anyway.
    gsub("#", ".number_", .) %>%
    gsub("^[[:space:][:punct:]]+", "", .) %>% # remove leading spaces & punctuation
    make.names(.) %>%
    # Handle dots, multiple underscores, case conversion, string transliteration
    snakecase::to_any_case(case = case, sep_in = "\\.", 
                           transliterations = c("Latin-ASCII"))
  
  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- vapply(1:length(new_names), function(i) { 
    sum(new_names[i] == new_names[1:i]) }, integer(1))
  
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}
                    
# copy of clean_names from janitor v0.3 on CRAN, to preserve old behavior
old_clean_names <- function(dat){

  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("'", "", .) %>% # remove quotation marks
    gsub("\"", "", .) %>% # remove quotation marks
    gsub("%", "percent", .) %>%
    gsub("^[ ]+", "", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>% # convert 1+ periods to single _
    gsub("[_]+", "_", .) %>% # fix rare cases of multiple consecutive underscores
    tolower(.) %>%
    gsub("_$", "", .) # remove string-final underscores
  
  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- sapply(1:length(new_names), function(i) { sum(new_names[i] == new_names[1:i]) })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}
