# Tests for data.frame renaming function

library(janitor)
context("data.frame name cleaning")

test_df <- data.frame(matrix(ncol = 10) %>% as.data.frame())
names(test_df) <- c("sp ace", "repeat", "a**#@", "%", "#", "!",
                    "d(!)9", "REPEAT", "can\"'t", "hi_`there`")

clean <- clean_names(test_df)

test_that("Names are cleaned appropriately", {
  expect_equal(names(clean)[1], "sp_ace") # spaces
  expect_equal(names(clean)[2], "repeat") # first instance of repeat
  expect_equal(names(clean)[3], "a") # multiple special chars, trailing special chars
  expect_equal(names(clean)[4], "percent") # converting % to percent
  expect_equal(names(clean)[5], "x") # 100% invalid name
  expect_equal(names(clean)[6], "x_2") # repeat of invalid name
  expect_equal(names(clean)[7], "d_9") # multiple special characters
  expect_equal(names(clean)[8], "repeat_2") # uppercase, 2nd instance of repeat
  expect_equal(names(clean)[9], "cant") # uppercase, 2nd instance of repeat
  expect_equal(names(clean)[10], "hi_there") # double-underscores to single
})

test_that("Returns a data.frame", {
 expect_is(clean, "data.frame") 
})