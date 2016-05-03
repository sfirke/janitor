#' @title Cleans names of a data.frame.
#'
#' @description
#' Resulting names are unique and consist only of the \code{_} character, lowercase letters, and numbers.
#'
#' @param dat the input data.frame.
#' @return Returns the data.frame with clean names.
#' @export
#' @examples
#' # not run:
#' # clean_names(poorly_named_df)
#'
#' # library(dplyr) ; library(readxl)
#' # not run:
#' # readxl("messy_excel_file.xlsx") %>% clean_names()

clean_names <- function(dat){

  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)

  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- sapply(1:length(new_names), function(i) { sum(new_names[i] == new_names[1:i]) })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  setNames(dat, new_names)
}
