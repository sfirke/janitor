
#' @title Update shared columns between two data.frames. 
#'
#' @description
#' Update all the columns in a data.frame using identically-named columns in another.  Where the second data.frame has \code{NA} values, the original value will be retained.  This is a wrapper function for calling \code{dplyr::coalesce} on many columns at once; understand that function to understand this one. 
#' 
#' @param x the data.frame to update
#' @param y the data.frame from which to draw updates.  Must be join-able to \code{x}.  Columns to update must be identically named in the two data.frames. 
#' @param vars_to_update either "all" (the default) to update all variables that share names, or a character vector of column names if not all identically-named vectors should be updated.
#' @param precedence if there are non-\code{NA} values for a vector in both data.frames, which value should be selected?
#' @param by a character vector of variables to join by.  Gets passed on to \code{dplyr::left_join}.
#' @param ... other parameters passed on to \code{dplyr::left_join}.
#'
#' @return the coalesced version of the input data.frame \code{x}.
#' @export
#'
#' @examples
#' t1 <- data.frame(id = 1, lo = as.numeric(NA), med = 3, hi = as.numeric(NA), other = "still here")
#' t2 <- data.frame(id = 1, med = 4, hi = 10, lo = 1)
#' coalesce_dfs(t1, t2, by = "id")
#' coalesce_dfs(t1, t2, vars_to_update = c("lo"), by = "id") # update just the specified col

coalesce_dfs <- function(x, y, vars_to_update = "all", precedence = "x", by = NULL, ...){
  
  if(vars_to_update[[1]] == "all"){
    vars_to_update <- intersect(names(x), names(y))[!intersect(names(x), names(y)) %in% by] # need better way to avoid manipulating the by variable(s), right now okay if joining on names that match across the data.frames
  }
  result <- dplyr::left_join(x,
                      y[, c(by, vars_to_update)],
                      by = by,
                      ...)
  for(i in seq_along(vars_to_update)){
  result <- result %>%
    coalesce_joined_var(., vars_to_update[i], precedence = precedence)
  }
  message(paste0("Coalesced: ", paste(vars_to_update, collapse = ", ")))
  result %>%
    dplyr::select(dplyr::one_of(names(x)))
}

# coalesce two variables .x and .y given the original name, and drop the .x and .y
# takes a data.frame and name of a column, returns a data.frame
# undocumented helper function called by janitor::coalesce_dfs
coalesce_joined_var <- function(dat, var_name, precedence){
  # var_name <- deparse(substitute(var))
  var_name_x <- paste0(var_name, ".x")
  var_name_y <- paste0(var_name, ".y")
  
  #### WHEN THIS CALL TO COALESCE ERRORS DUE TO TYPE MISTAMCH, IT SHOULD ID THE PROBLEM VARIABLE
  if(precedence == "x"){
    dat <- dat %>%
      dplyr::mutate(temp___var = dplyr::coalesce(dat[[var_name_x]],
                                          dat[[var_name_y]]))
  } else if(precedence == "y"){
    dat <- dat %>%
      dplyr::mutate(temp___var = dplyr::coalesce(dat[[var_name_y]],
                                          dat[[var_name_x]]))
  }
  names(dat)[names(dat) == "temp___var"] <- var_name # put back the correct variable name
  
  dat <- dat[, !names(dat) %in% c(var_name_x, var_name_y)] # drop the .x and .y variables

}
