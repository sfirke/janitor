#' @title converts a string, possibly containing the names of a data.frame, into a cleaner version.s
#'
#' @description
#' Resulting strings are unique and consist only of the \code{_} character, numbers, and letters.
#' Capitalization preferences can be specified using the \code{case} parameter.
#'
#' Accented characters are
#' transliterated to ASCII.  For example, an "o" with a German umlaut over it becomes "o", and the Spanish character "enye" becomes "n".
#'
#' @param string A string (for example names of a data frame).
#'
#' @inheritParams clean_names
#'
#' @return Returns the "cleaned" character vector.
#' @export
#' @examples
#' # not run:
#' # make_clean_names(names(poorly_named_df))
#'
#' # or pipe in the input data.frame:
#' # poorly_named_df %>% names() %>% make_clean_names()
#'
#' # if you prefer camelCase variable names:
#' # poorly_named_df %>% names() %>% make_clean_names(., "small_camel")
#'
#' # not run:
#' # library(readxl)
#' # read_excel("messy_excel_file.xlsx") %>% names() %>% make_clean_names()

make_clean_names <- function(string, case = c(
  "snake", "lower_camel", "upper_camel", "screaming_snake",
  "lower_upper", "upper_lower", "all_caps", "small_camel",
  "big_camel", "old_janitor", "parsed", "mixed"
)) {
  
  # old behavior, to provide easy fix for people whose code breaks with the snakecase integration
  case <- match.arg(case)
  if (case == "old_janitor") {
    return(old_make_clean_names(string))
  }
  
  ### new behaviour with snakecase integration
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- string
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
    # Parsing option 4 removes underscores around numbers, #153
    snakecase::to_any_case(.,
      case = case, sep_in = "\\.",
      transliterations = c("Latin-ASCII"), parsing_option = 1,
      numerals = "asis"
    )
  
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
