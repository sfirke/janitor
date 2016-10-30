# Tests for crosstab function

library(janitor)
library(dplyr)
context("coalesece_dfs()")

df1 <- data.frame(id = 1, lo = as.numeric(NA), med = 3, hi = as.numeric(NA), other = "still here", stringsAsFactors = FALSE)
df2 <- data.frame(id = 1, med = 4, hi = 10, lo = 1, other = as.character(NA), stringsAsFactors = FALSE)
coalesce_dfs(df1, df2, by = "id")
coalesce_dfs(df1, df2, precedence = "y", by = "id")
#' coalesce_dfs(t1, t2, vars_to_update = c("lo"), by = "id") # update just the specified col

# need to add error catching in the function, and then test that erroring behavior here
test_that("column ordering remains the same", {

})

test_that("multiple vectors update correctly", {
  
})

test_that("only the specified vectors update", {
  
})

test_that("message prints correctly", {
  
})

test_that("precedence works correctly", {
  
})



