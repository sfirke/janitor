# Tests for use_first_valid function

library(janitor)
library(dplyr)
context("use_first_valid_of()")


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


test_that("bad inputs are handled properly", {

  
})