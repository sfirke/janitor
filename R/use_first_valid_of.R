#' @title Returns first non-NA value from a set of vectors.
#'
#' @description
#' At each position of the input vectors, iterates through in order and returns the first non-NA value.  This is a robust replacement of the common \code{ifelse(!is.na(x), x, ifelse(!is.na(y), y, z))}.  It's more readable and handles problems like \code{ifelse}'s inability to work with dates in this way.
#'
##' @section Warning: Deprecated, do not use in new code. Use \code{dplyr::coalesce()} instead.
#' @param ... the input vectors.  Order matters: these are searched and prioritized in the order they are supplied.
#' @param if_all_NA what value should be used when all of the vectors return \code{NA} for a certain index?  Default is NA.
#' @return Returns a single vector with the selected values.
#' @seealso janitor_deprecated
#' @export
# EXCLUDE COVERAGE START
use_first_valid_of <- function(..., if_all_NA = NA){
  .Deprecated("dplyr::coalesce()")
  
  vars <- list(...)
  vec_length <- length(vars[[1]])
  num_vars <- length(vars)
  
  # check var lengths - must all be equal
  if(length(unique(lapply(vars, length))) > 1){ stop("All input vectors must have the same length") }
  
  # check that no variables are lists - in particular, this function does not support POSIXlt
  if(sum(vapply(vars, is.list, FUN.VALUE = logical(1))) > 0){
    if(sum(vapply(vars, function(x) {"POSIXlt" %in% class(x)}, FUN.VALUE = logical(1))) > 0){
      stop("At least one input vector is of class POSIXlt, which is a list; convert to POSIXct")
    } else{
      stop("At least one of the given inputs is a list; supply only vectors to this function")
    }
  }
  
  # check var types
  if(length(unique(lapply(vars, class))) > 1){
    warning("Input vectors do not share a single class - all input vectors will be coerced to class `character`")
    vars <- lapply(vars, as.character)
  }
  
  # coerce factors to character - returns numeric levels otherwise, and it's too complex to guess at what the result factor levels should be
  if(sum(class(vars[[1]]) %in% "factor") > 0){
    warning("Input vectors are of class 'factor' and will be coerced to class 'character' before combining")
    vars <- lapply(vars, as.character)
  }
  
  
  # initialize results vector of appropriate length
  # it is logical class by default, and then it can switch via coercion as values are assigned, except for Date handled separately
  result <- rep(NA, length = vec_length)

  # change result vector type to the appropriate type, to handle Date and POSIXct classes. At this point can assume that all vectors have the same class 
  var_class <- class(vars[[1]])
  class(result) <- var_class

    # fill it using two for loops
  for(i in 1:vec_length){ # loop down the length of the vector
    
    for(j in 1:num_vars){ # check through vectors in order of priority
      
      # if valid, return and break
      if(!is.na(vars[[j]][i])){
        result[i] <- vars[[j]][i]
        break
      }  
    }
  }

  # overwrite any still-NA cases with if_all_NA value
  if(!is.na(if_all_NA)){
    if(class(if_all_NA) != class(result)){stop("class(if_all_NA) does not match class of resulting vector")}
    result[is.na(result)] <- if_all_NA
  }
 
  result
}
# EXCLUDE COVERAGE END