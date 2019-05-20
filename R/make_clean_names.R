#' @title Cleans a vector of text, typically containing the names of an object.
#'
#' @description
#' Resulting strings are unique and consist only of the \code{_} character, numbers, and letters.
#' Capitalization preferences can be specified using the \code{case} parameter.
#'
#' For use on the names of a data.frame, e.g., in a \code{`\%>\%`} pipeline,
#' call the convenience function \code{\link[janitor]{clean_names}}.
#' 
#' Accented characters are transliterated to ASCII.  For example, an "o" 
#' with a German umlaut over it becomes "o", and the Spanish character "enye" becomes "n".
#'
#' @param string A character vector of names to clean.
#'
#' @inheritParams clean_names
#'
#' @return Returns the "cleaned" character vector.
#' @export
#' @examples
#' 
#' # cleaning the names of a vector:
#' x <- structure(1:3, names = c("name with space", "TwoWords", "total $ (2009)"))
#' x
#' names(x) <- make_clean_names(names(x))
#' x # now has cleaned names

#' # if you prefer camelCase variable names:
#' make_clean_names(names(x), "small_camel")
#'
#' # similar to janitor::clean_names(poorly_named_df):
#' # not run:
#' # make_clean_names(names(poorly_named_df))
#' 
make_clean_names <- function(string, case = c(
  "snake", "lower_camel", "upper_camel", "screaming_snake",
  "lower_upper", "upper_lower", "all_caps", "small_camel",
  "big_camel", "old_janitor", "parsed", "mixed", "none")) {
  
  # Handling "old_janitor" case for backward compatibility
  case <- match.arg(case)
  if (case == "old_janitor") {
    return(old_make_clean_names(string))
  }
  
  # Replacement of "meaningful" characters
  string <- gsub("'", "", string) # quotation marks
  string <- gsub("\"", "", string) # double quotation marks
  string <- gsub("%", ".percent_", string) # starting with "." as a workaround, to make ".percent" a valid name. The "." will bereplaced in the call to to_any_case via the sep_in argument anyway.
  string <- gsub("#", ".number_", string)
  
  # Ensuring validity of names
  string <- gsub("^[[:space:][:punct:]]+", "", string) # removes leading spaces & punctuation as otherwise make.names might return improper results
  string <- make.names(string)
  
  # Case conversion
  to_any_case(
    string = string, 
    case = case, 
    abbreviations = NULL, 
    sep_in = "[^[:alnum:]]", 
    parsing_option = 1, 
    transliterations = "Latin-ASCII", 
    numerals = "asis", 
    sep_out = NULL, 
    unique_sep = NULL,
    empty_fill = NULL,
    prefix = "",
    postfix = ""
  )
  
  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- vapply(seq_along(string), function(i) {
    sum(string[i] == new_names[1:i])
  }, integer(1))
  
  string[dupe_count > 1] <- paste(
    string[dupe_count > 1],
    dupe_count[dupe_count > 1],
    sep = "_"
  )
  
  # Return
  string
}
# copy of clean_names from janitor v0.3 on CRAN, to preserve old behavior
old_make_clean_names <- function(string) {
  
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- string
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
  dupe_count <- vapply(seq_along(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  }, integer(1))
  
  new_names[dupe_count > 1] <- paste(
    new_names[dupe_count > 1],
    dupe_count[dupe_count > 1],
    sep = "_"
  )
  new_names
}
