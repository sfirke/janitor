#' @title Cleans names of a data.frame.
#'
#' @description
#' Resulting names are unique and consist only of the \code{_} character, lowercase letters, and numbers.
#' 
#' Accented characters are
#' transliterated to ASCII.  For example, an "o" with a german umlaut over it becomes "o", and the Spanish character "enye" becomes "n". 
#'
#' @param dat the input data.frame.
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

clean_names <- function(dat){

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
    gsub("_$", "", .) %>% # remove string-final underscores
    stringi::stri_trans_general("latin-ascii") 

  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- sapply(1:length(new_names), function(i) { sum(new_names[i] == new_names[1:i]) })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  stats::setNames(dat, new_names)
}
