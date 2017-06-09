# # Tests the ns_to_percents() function
# 
# library(janitor)
# context("ns_to_percents()")
# 
# library(dplyr)
# 
# source1 <- mtcars %>%
#   crosstab(cyl, am)
#   
# test_that("calculations are accurate", {
#   expect_equal(ns_to_percents(source1), # default parameter is denom = "row"
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = c(3/11, 4/7, 12/14),
#                           `1` = c(8/11, 3/7, 2/14),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
#   expect_equal(ns_to_percents(source1, denom = "col"),
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = c(3/19, 4/19, 12/19),
#                           `1` = c(8/13, 3/13, 2/13),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
#   expect_equal(ns_to_percents(source1, denom = "all"),
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = c(3/32, 4/32, 12/32),
#                           `1` = c(8/32, 3/32, 2/32),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
# })
# 
# source2 <- source1
# source2[2, 2] <- NA
# 
# test_that("NAs handled correctly with na.rm = TRUE", {
#   expect_equal(ns_to_percents(source2), # row
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = c(3/11, NA, 12/14),
#                           `1` = c(8/11, 1, 2/14),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
#   expect_equal(ns_to_percents(source2, denom = "col"),
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = c(3/15, NA, 12/15),
#                           `1` = c(8/13, 3/13, 2/13),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
# })
# 
# test_that("NAs handled correctly with na.rm = FALSE", {
#   expect_equal(ns_to_percents(source2, na.rm = FALSE), # row
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = c(3/11, NA, 12/14),
#                           `1` = c(8/11, NA, 2/14),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
#   expect_equal(ns_to_percents(source2, denom = "col", na.rm = FALSE),
#                data.frame(cyl = c(4, 6, 8),
#                           `0` = as.numeric(c(NA, NA, NA)),
#                           `1` = c(8/13, 3/13, 2/13),
#                           check.names = FALSE,
#                           stringsAsFactors = FALSE)
#   )
# })
# 
# test_that("data.frames with no numeric columns beyond the first cause failure", {
#   expect_error(ns_to_percents(data.frame(a = 1:2, b = c("hi", "lo"))),
#                "at least one one of columns 2:n must be of class numeric")
# })
# 
# test_that("non-numeric argument to total_n fails", {
#   expect_error(ns_to_percents(source1, "all", total_n = "a bear"),
#                "override_n must be numeric")
# })
# 
# test_that("override value total_n functions correctly", {
#   expect_equal(ns_to_percents(source1, total_n = 100), # nothing should happen with default denom of "row"
#                ns_to_percents(source1))
#   
#   expect_equal(ns_to_percents(source1, denom = "all", total_n = 320),
#                cbind(data.frame(cyl = c(4, 6, 8)),
#                      ns_to_percents(source1, denom = "all")[, -1] / 10) # divide by 10 because the mtcars n = 32
#   )
# })
# 
# test_that("works with a single numeric column per #89", {
#   dat <- data.frame(Operation = c("Login", "Posted", "Deleted"), `Total Count` = c(5, 25, 40), check.names = FALSE)
#   expect_equal(dat %>% ns_to_percents("col"),
#                data.frame(Operation = c("Login", "Posted", "Deleted"),
#                           `Total Count` = c(5/70, 25/70, 40/70),
#                           check.names = FALSE)
#   )
# })