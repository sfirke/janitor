use_first_valid_of <- function(..., if_all_NA = NA, force_class = NA){
  vars <- list(...)
  
  vec_length <- length(vars[[1]])
  num_vars <- length(vars)
  # check var lengths - must all be equal
  # check var types?  Initial requirement is that they should be same class, maybe eventually coerce to character
  # check input types
  
  # initialize results vector of appropriate length
  # okay to make it logical by default, and then it can switch via coercion as values are assigned?
  result <- rep(NA, length = vec_length)

  if(!is.na(force_class) & force_class == "date"){
    class(result) <- "Date"
  }
  # fill it using 2 for loops
  
  for(i in 1:vec_length){ # loop down the length of the vector
    
    for(j in 1:num_vars){ # check through vectors in order of priority
      
      # if valid, return and break
      if(!is.na(vars[[j]][i])){
        result[i] <- vars[[j]][i]
        break
      }  
    }
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
  d1 = c(as.Date("2006-01-01"), as.Date("2006-01-01"), NA),
  d2 = c(as.Date("2016-01-01"), as.Date("2016-01-01"), as.Date("2016-03-03"))
)

#calls
use_first_valid_of(dat$a, dat$b, dat$c)
use_first_valid_of(dat$d1, dat$d2, force_class = "date")

# replicating:
ifelse(!is.na(dat$a), dat$a,
       ifelse(!is.na(dat$b), dat$b, 
              dat$c))