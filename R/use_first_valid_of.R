#' @title Returns first non-NA value from a set of vectors.
#'
#' @description
#' At each position of the input vectors, iterates through in order and returns the first non-NA value.  This is a robust replacement of the common \code{ifelse(!is.na(x), x, ifelse(!is.na(y), y, z))}.  It's more readable and handles problems like \code{ifelse}'s inability to work with dates in this way.
#'
#' @param ... the input vectors.  Order matters: these are searched and prioritized in the order they are supplied.
#' @param if_all_NA what value should be used when all of the vectors return \code{NA} for a certain index?  Default is NA.
#' @return Returns a single vector with the selected values.
#' @export
#' @examples
#' x <- c(1, NA, NA); y <- c(2, 2, NA); z <- c(3, 3, 3)
#' use_first_valid_of(x, y, z)
#' use_first_valid_of(y, x, if_all_NA = 0)


use_first_valid_of <- function(..., if_all_NA = NA){
  vars <- list(...)
  vec_length <- length(vars[[1]])
  num_vars <- length(vars)
  
  # check var lengths - must all be equal
  if(length(unique(lapply(vars, length))) > 1){ stop("All input vectors must have the same length") }
  
  # check var types
  if(length(unique(lapply(vars, class))) > 1){
    warning("Input vectors do not share a single class - all input vectors will be coerced to class `character`")
    vars <- lapply(vars, as.character)
  }
  
  # coerce factors to character - returns numeric levels otherwise, and it's too complex to guess at what the result factor levels should be
  if(class(vars[[1]]) == "factor"){
    warning("Input vectors are of class 'factor' and will be coerced to class 'character' before combining")
    vars <- lapply(vars, as.character)
  }
  
  
  # initialize results vector of appropriate length
  # it is logical class by default, and then it can switch via coercion as values are assigned, except for Date handled separately
  result <- rep(NA, length = vec_length)

  # change result vector type to Date if appropriate - at his point can assume that all vectors have the same class 
  if(class(vars[[1]]) == "Date"){
    class(result) <- "Date"
  }
  
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
