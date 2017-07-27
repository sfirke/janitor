#' @title Generate a frequency table (1-, 2-, or 3-way).
#'
#' @description
#' A fully-featured alternative to \code{table()}.  Results are data.frames and can be formatted and enhanced with janitor's family of \code{adorn_} functions.
#' 
#' Specify a data.frame and the one, two, or three unquoted column names you want to tabulate.  Three variables generates a list of 2-way tabyls, split by the third variable.
#' 
#' Alternatively, you can tabulate a single variable that isn't in a data.frame by calling \code{tabyl} on a vector, e.g., \code{tabyl(mtcars$gear)}.
#' 
#' @param dat a data.frame containing the variables you wish to count.  Or, a vector you want to tabulate.
#' @param var1 the column name of the first variable.
#' @param var2 (optional) the column name of the second variable (the rows in a 2-way tabulation).
#' @param var3 (optional) the column name of the third variable (the list in a 3-way tabulation).
#' @param show_na should counts of \code{NA} values be displayed?  In a one-way tabyl, the presence of \code{NA} values triggers an additional column showing valid percentages(calculated excluding \code{NA} values).
#' @param show_missing_levels should counts of missing levels of factors be displayed?  These will be rows and/or columns of zeroes.  Useful for keeping consistent output dimensions even when certain factor levels may not be present in the data.
#' @param ... the arguments to tabyl.
#' @return Returns a data.frame with frequencies and percentages of the tabulated variable(s).  A 3-way tabulation returns a list of data.frames.
#' @export
#' @examples
#' 
#' tabyl(mtcars, cyl)
#' tabyl(mtcars, cyl, gear)
#' tabyl(mtcars, cyl, gear, am)
#' 
#' # or using the %>% pipe
#' mtcars %>%
#'   tabyl(cyl, gear)
#' 
#' # illustrating show_na functionality:
#' my_cars <- rbind(mtcars, rep(NA, 11))
#' my_cars %>% tabyl(cyl)
#' my_cars %>% tabyl(cyl, show_na = FALSE)
#' 
#' # Calling on a single vector not in a data.frame:
#' val <- c("hi", "med", "med", "lo")
#' tabyl(val)


tabyl <- function(dat, ...) UseMethod("tabyl")


#' @inheritParams tabyl
#' @export
#' @rdname tabyl
# retain this method for calling tabyl() on plain vectors

tabyl.default <- function(dat, show_na = TRUE, show_missing_levels = TRUE, ...) {
  
  # catch and adjust input variable name.
  if(is.null(names(dat))) {
    var_name <- deparse(substitute(dat))
  } else {
    var_name <- names(dat)
  }
  
  # useful error message if input vector doesn't exist
  if(is.null(dat)){stop(paste0("object ", var_name, " not found"))}
  # an odd variable name can be deparsed into a vector of length >1, rare but throws warning, see issue #87
  if(length(var_name) > 1){ var_name <- paste(var_name, collapse = "") }
  
  # calculate initial counts table
  # convert vector to a 1 col data.frame
  if(mode(dat) %in% c("logical", "numeric", "character", "list") & !is.matrix(dat)) {
    if(is.list(dat)){ dat <- dat[[1]] } # to preserve factor properties when vec is passed in as a list from data.frame method
    dat_df <- data.frame(dat, stringsAsFactors = is.factor(dat))
    names(dat_df)[1] <- "dat"
    
    
    result <- dat_df %>% dplyr::count(dat)
    
    if(is.factor(dat) & show_missing_levels){
      expanded <- tidyr::expand(result, dat)
      result <- merge(x = expanded, # can't use dplyr::left_join because as of 0.6.0, NAs don't match, and na_matches argument not present < 0.6.0
                      y = result,
                      by = "dat",
                      all.x = TRUE)
      result <- dplyr::arrange(result, dat) # restore sorting by factor level
    }
    
  } else {stop("input must be a vector of type logical, numeric, character, list, or factor")}
  
  # calculate percent, move NA row to bottom
  result <- result %>%
    dplyr::mutate(percent = n / sum(n, na.rm = TRUE))
  
  # sort the NA row to the bottom, necessary to retain factor sorting  
  result <- result[order(is.na(result$dat)), ]
  result$is_na <- NULL
  
  # replace all NA values with 0 - only applies to missing factor levels
  result <- tidyr::replace_na(result, replace = list(n = 0, percent = 0))
  
  ## NA handling:
  # if there are NA values & show_na = T, calculate valid % as a new column
  if(show_na && sum(is.na(result[[1]])) > 0) {
    valid_total <- sum(result$n[!is.na(result[[1]])], na.rm = TRUE)
    result$valid_percent = result$n / valid_total
    result$valid_percent[is.na(result[[1]])] <- NA
  } else { # don't show NA values, which necessitates adjusting the %s
    result <- result %>%
      dplyr::filter(!is.na(.[1])) %>%
      dplyr::mutate(percent = n / sum(n, na.rm = TRUE)) # recalculate % without NAs
  }
  
  # reassign correct variable name
  names(result)[1] <- var_name
  
  # in case input var name was "n" or "percent", call helper function to set unique names
  result <- handle_if_special_names_used(result)
  
  data.frame(result, check.names = FALSE) %>%
    as_tabyl(1)
}


#' @inheritParams tabyl
#' @export
#' @rdname tabyl
# Main dispatching function to underlying functions depending on whether "..." contains 1, 2, or 3 variables
tabyl.data.frame <- function(dat, var1, var2, var3, show_na = TRUE, show_missing_levels = TRUE, ...){
  if("data.frame" %in% class(dat) &
     missing(var1) & missing(var2) & missing(var3)){stop("if calling on a data.frame, specify unquoted column names(s) to tabulate.  Did you mean to call tabyl() on a vector?")}
  if(dplyr::is_grouped_df(dat)){ dat <- dplyr::ungroup(dat) } 

  if(missing(var2) & missing(var3)){
    tabyl_1way(dat, rlang::enquo(var1), show_na = show_na, show_missing_levels = show_missing_levels)
  } else if(missing(var3)){
    tabyl_2way(dat, rlang::enquo(var1), rlang::enquo(var2), show_na = show_na, show_missing_levels = show_missing_levels)
  } else if(!missing(var1) &
            !missing(var2) &
            !missing(var3)){
    tabyl_3way(dat, rlang::enquo(var1), rlang::enquo(var2), rlang::enquo(var3), show_na = show_na, show_missing_levels = show_missing_levels)
  } else {
    stop("please specify var1 OR var1 & var2 OR var1 & var2 & var3")
  }
  
}

# a one-way frequency table; this was called "tabyl" in janitor <= 0.3.0
tabyl_1way <- function(dat, var1, show_na = TRUE, show_missing_levels = TRUE){
  x <- dplyr::select(dat, !! var1)
  
  # gather up arguments, pass them to tabyl.default
  arguments <- list()
  arguments$dat <- x[1]
  arguments$show_na <- show_na
  arguments$show_missing_levels <- show_missing_levels
  
  do.call(tabyl.default,
          args = arguments)
  
}


# a two-way frequency table; this was called "crosstab" in janitor <= 0.3.0
tabyl_2way <- function(dat, var1, var2, show_na = TRUE, show_missing_levels = TRUE){
  
  dat <- dplyr::select(dat, !! var1, !! var2)
  
  if(!show_na){
    dat <- dat[!is.na(dat[[1]]) & !is.na(dat[[2]]), ]
  }
  
  tabl <- dat %>%
    dplyr::count(!! var1, !! var2)
  
  # Optionally expand missing factor levels.  Inspired by https://stackoverflow.com/a/10954773/4470365
  if(sum(unlist(lapply(dat, is.factor))) > 0 & show_missing_levels){
    if(!is.factor(tabl[[1]])){tabl[[1]] <- factor(tabl[[1]])} # no harm in converting to factors if not currently; makes the expand.grid simpler
    if(!is.factor(tabl[[2]])){tabl[[2]] <- factor(tabl[[2]])}
    combos <- expand.grid(levels(tabl[[1]]), levels(tabl[[2]]))
    names(combos) <- names(tabl)[1:2]
    tabl <- suppressMessages(dplyr::full_join(tabl, combos))
  }

  # replace NA with string NA_ in vec2 to avoid invalid col name after spreading
  # if this col is a factor, need to add that level to the factor
  if(is.factor(tabl[[2]])){
    levels(tabl[[2]]) <- c(levels(tabl[[2]]), "NA_")
  }
  tabl[2][is.na(tabl[2])] <- "NA_"
  
  result <- tabl %>%
    tidyr::spread_(rlang::quo_name(var2), "n", fill = 0)
  
  if("NA_" %in% names(result)){ result <- result[c(setdiff(names(result), "NA_"), "NA_")] } # move NA_ column to end, from http://stackoverflow.com/a/18339562
  
  result %>%
    data.frame(., check.names = FALSE) %>%
    as_tabyl(2)
}


# a list of two-way frequency tables, split into a list on a third variable
tabyl_3way <- function(dat, var1, var2, var3, show_na = TRUE, show_missing_levels = TRUE){
  
  split(dat, dat[[rlang::quo_name(var3)]]) %>%
    purrr::map(tabyl_2way, var1, var2, show_na = show_na, show_missing_levels = show_missing_levels)
}



# function that checks if col 1 name is "n" or "percent",
## if so modifies the appropriate other column name to avoid duplicates
handle_if_special_names_used <- function(dat){
  if(names(dat)[1] == "n"){
    names(dat)[2] <- "n_n"
  } else if(names(dat)[1] == "percent"){
    names(dat)[3] <- "percent_percent"
  }
  dat
}
