#' @title Get rows of a \code{data.frame} with identical values for the specified variables.
#'
#' @description
#' For hunting duplicate records during data cleaning.  Specify the data.frame and the variable combination to search for duplicates and get back the duplicated rows.
#'
#' @param dat the input data.frame.
#' @param ... unquoted variable names to search for duplicates.
#' @return Returns a data.frame (actually a \code{tbl_df}) with the full records where the specified variables have duplicated values, as well as a variable \code{dupe_count} showing the number of rows sharing that combination of duplicated values.
#' @export
#' @examples
#' get_dupes(mtcars, mpg, hp)
#' # or called with the magrittr pipe %>% :
#' mtcars %>% get_dupes(wt)
#'

get_dupes <- function(dat, ...) {
  uq_names <- as.list(substitute(list(...)))[-1L] # unquoted names for NSE calls, need quoted names separately for messages + warnings
  df_name <- deparse(substitute(dat))

  if (length(uq_names) == 0) { # if called on an entire data.frame with no specified variable names
    var_names <- names(dat)
    nms <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    nms <- rlang::quos(...) %>%
      rlang::quos_auto_name()
    var_names <- names(nms)
  }

  # check that each variable name provided is present in names(dat); if not, throw error
  check_vars_in_df(dat, df_name, var_names)
  dupe_count <- NULL # to appease NOTE for CRAN; does nothing.

  # calculate counts to join back to main df
  counts <- dat %>%
    dplyr::count(!!! nms)

  names(counts)[ncol(counts)] <- "dupe_count"
  # join new count vector to main data.frame
  dupes <- suppressMessages(dplyr::inner_join(counts, dat))

  dupes <- dupes %>%
    dplyr::filter(dupe_count > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!! nms)

  # shorten error message for large data.frames
  if (length(var_names) > 10) {
    var_names <- c(var_names[1:9], paste("... and", length(var_names) - 9, "other variables"))
  }
  if (nrow(dupes) == 0) {
    message(paste0("No duplicate combinations found of: ", paste(var_names, collapse = ", ")))
  }
  dupes
}

# takes a data.frame and vector of variable names, confirms that all var names match data.frame names
check_vars_in_df <- function(dat, dat_name, names_vec) {
  in_df <- unlist(lapply(names_vec, function(x) x %in% names(dat)))
  if (sum(in_df) != length(in_df)) {
    stop(paste0(
      paste0("These variables do not match column names in ", dat_name, ": "),
      paste(names_vec[!in_df], collapse = ", ")
    ))
  }
}
