# Tests for encoding checker

library(janitor)
context("check_encoding")

check_encoding_true <- function(x, actual){
  expect_equal(x %>% check_encoding(encoding = actual), x) # Should pass x silently
  expect_true(check_encoding(x, encoding=actual, level="logical")) 
}

check_encoding_false <- function(x, bad_guess){
  expect_false(check_encoding(x, encoding=bad_guess, level="logical"))
  expect_error(check_encoding(x, encoding=bad_guess))
  expect_warning(check_encoding(x, encoding=bad_guess, level="warning"))
  expect_message(check_encoding(x, encoding=bad_guess, level="message"))
}


test_that("latin1 example runs correctly", {
  x <- "var1\nÉmigré cause célèbre déjà vu."
  
  check_encoding_true(x, "UTF-8")
  check_encoding_false(x, "ISO-8859-1")
  
  y <- stringi::stri_conv(x, "UTF-8", "Latin1")
  
  check_encoding_true(y, "ISO-8859-1")
  check_encoding_false(y, "UTF-8")
})


test_that("Simple text recognized", {
  # Note that, ideally, guess_encoding would recognize this as potentially utf-8...
  x <- "asdf\nasdf\n"
  check_encoding_true(x, "ASCII")
})


  
  



