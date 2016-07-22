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

# testing

library(tibble)
dat <- data_frame(
  a = c(1, NA, NA),
  b = c(2, 2, NA),
  c = c(3, 3, 3),
  x = c("hi", "med", "lo"),
  d1 = c(as.Date("1999-01-01"), as.Date("1999-02-02"), NA),
  d2 = c(as.Date("2016-01-01"), as.Date("2016-02-02"), as.Date("2016-03-03"))
)

#calls
use_first_valid_of(dat$a, dat$b, dat$c)
use_first_valid_of(dat$d1, dat$d2, force_class = "date")

dat %>%
  mutate(new_var = use_first_valid_of(a, b, c))

dat %>%
  mutate(new_var = use_first_valid_of(d1, d2, force_class = "date"))

use_first_valid_of(dat$a, dat$b, if_all_NA = 0)

# Test error cases
use_first_valid_of(dat$a, dat$d1, if_all_NA = "Missing")
use_first_valid_of(dat$a, if_all_NA = "Missing")

# replicating:
ifelse(!is.na(dat$a), dat$a,
       ifelse(!is.na(dat$b), dat$b, 
              dat$c))

# check type conversion from numeric to character
ifelse(!is.na(dat$a), dat$a,
       ifelse(!is.na(dat$b), dat$b, 
              dat$x))
