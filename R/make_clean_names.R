#' @title Cleans a vector of text, typically containing the names of an object.
#'
#' @description
#' Resulting strings are unique and consist only of the \code{_} character,
#' numbers, and letters. By default, the resulting strings will only consist of
#' ASCII characters, but non-ASCII (e.g. Unicode) may be allowed by setting
#' `ascii=FALSE`.  Capitalization preferences can be specified using the
#' \code{case} parameter.
#'
#' For use on the names of a data.frame, e.g., in a \code{`\%>\%`} pipeline,
#' call the convenience function \code{\link[janitor]{clean_names}}.
#' 
#' When `ascii=TRUE` (the default), accented characters are transliterated to
#' ASCII.  For example, an "o" with a German umlaut over it becomes "o", and the
#' Spanish character "enye" becomes "n".
#' 
#' The order of operations is: `replace`, (optional) ASCII conversion, removing
#' initial spaces and punctuation, apply `base::make.names()`, apply
#' `snakecase::to_any_case()`, and add numeric suffixes to duplicates.
#'
#' @param string A character vector of names to clean.
#' @param case The desired target case (default is \code{"snake"}), indicated by
#'   these possible values:
#' \itemize{
#'  \item{\code{"snake"} produces snake_case}
#'  \item{\code{"lower_camel"} or \code{"small_camel"} produces lowerCamel}
#'  \item{\code{"upper_camel"} or \code{"big_camel"} produces UpperCamel}
#'  \item{\code{"screaming_snake"} or \code{"all_caps"} produces ALL_CAPS}
#'  \item{\code{"lower_upper"} produces lowerUPPER}
#'  \item{\code{"upper_lower"} produces UPPERlower}
#'  \item{\code{old_janitor}: legacy compatibility option to preserve behavior
#'    of \code{clean_names} prior to addition of the "case" argument(janitor
#'    versions <= 0.3.1 )}.  Provided as a quick fix for old scripts broken by
#'    the changes to \code{clean_names} in janitor v1.0.
#'  \item{\code{"parsed"}, \code{"mixed"}, \code{"none"}: less-common cases
#'    offered by \code{snakecase::to_any_case}.  See
#'    \code{\link[snakecase]{to_any_case}} for details.}
#'  }
#' @param replace A named character vector where the name is replaced by the
#'   value.
#' @param ascii Convert the names to ASCII (\code{TRUE}, default) or not
#'   (\code{FALSE}).
#'
#' @return Returns the "cleaned" character vector.
#' @export
#' @seealso \code{\link[snakecase]{to_any_case}}
#' @examples
#' 
#' # cleaning the names of a vector:
#' x <- structure(1:3, names = c("name with space", "TwoWords", "total $ (2009)"))
#' x
#' names(x) <- make_clean_names(names(x))
#' x # now has cleaned names
#'
#' # if you prefer camelCase variable names:
#' make_clean_names(names(x), "small_camel")
#'
#' # similar to janitor::clean_names(poorly_named_df):
#' # not run:
#' # make_clean_names(names(poorly_named_df))
#'
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_replace str_replace_all
make_clean_names <- function(string,
                             case = c(
                               "snake", "lower_camel", "upper_camel", "screaming_snake",
                               "lower_upper", "upper_lower", "all_caps", "small_camel",
                               "big_camel", "old_janitor", "parsed", "mixed", "none"
                             ),
                             replace=
                               c(
                                 "'"="",
                                 "\""="",
                                 "%"=".percent_",
                                 "#"=".number_"
                               ),
                             ascii=TRUE) {
  
  # Handling "old_janitor" case for backward compatibility
  case <- match.arg(case)
  if (case == "old_janitor") {
    return(old_make_clean_names(string))
  }

  replaced_names <-
    stringr::str_replace_all(
      str=string,
      pattern=replace
    )
  transliterated_names <-
    if (ascii) {
      stringi::stri_trans_general(
        replaced_names,
        id="Greek-Latin;Latin-ASCII;Accents-Any;Any-ASCII"
      )
    } else {
      replaced_names
    }
  # Remove starting spaces and punctuation
  good_start <-
    stringr::str_replace(
      str=transliterated_names,
      pattern="\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
      replacement="\\1"
    )
  # make.names() is dependent on the locale and therefore will return different
  # system-dependent values.
  made_names <- make.names(good_start)

  # Handle dots, multiple underscores, case conversion, string transliteration
  # Parsing option 4 removes underscores around numbers, #153
  cased_names <-
    snakecase::to_any_case(
      made_names,
      case = case,
      sep_in = "\\.",
      transliterations = "Latin-ASCII",
      parsing_option = 1,
      numerals = "asis"
    )
  
  # Handle duplicated names - they mess up dplyr pipelines.  This appends the
  # column number to repeated instances of duplicate variable names.
  dupe_count <-
    vapply(
      seq_along(cased_names), function(i) {
        sum(cased_names[i] == cased_names[1:i])
      },
      1L
    )
  
  cased_names[dupe_count > 1] <-
    paste(
      cased_names[dupe_count > 1],
      dupe_count[dupe_count > 1],
      sep = "_"
    )
  cased_names
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
