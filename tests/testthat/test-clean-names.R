# Tests for data.frame renaming function

library(janitor)
context("data.frame name cleaning")

test_df <- data.frame(matrix(ncol = 15) %>% as.data.frame())
names(test_df) <- c("sp ace", "repeated", "a**#@", "%", "#", "!",
                    "d(!)9", "REPEATED", "can\"'t", "hi_`there`", "  leading spaces",
                    "€", "ação", "Farœ", "a b c d e f")

clean <- clean_names(test_df, "snake")

test_that("Names are cleaned appropriately", {
  expect_equal(names(clean)[1], "sp_ace") # spaces
  expect_equal(names(clean)[2], "repeated") # first instance of repeat
  expect_equal(names(clean)[3], "a") # multiple special chars, trailing special chars
  expect_equal(names(clean)[4], "percent") # converting % to percent
  expect_equal(names(clean)[5], "x") # 100% invalid name
  expect_equal(names(clean)[6], "x_2") # repeat of invalid name
  expect_equal(names(clean)[7], "d_9") # multiple special characters
  expect_equal(names(clean)[8], "repeated_2") # uppercase, 2nd instance of repeat
  expect_equal(names(clean)[9], "cant") # uppercase, 2nd instance of repeat
  expect_equal(names(clean)[10], "hi_there") # double-underscores to single
  expect_equal(names(clean)[11], "leading_spaces") # leading spaces
  expect_equal(names(clean)[12], "x_3") # euro sign, invalid
  expect_equal(names(clean)[13], "acao") # accented word, transliterated to latin,
  expect_equal(names(clean)[14], "faroe") # œ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
                                          # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
  expect_equal(names(clean)[15], "a_b_c_d_e_f") # for testing alternating cases below with e.g., case = "upper_lower"
})

test_that("Returns a data.frame", {
 expect_is(clean, "data.frame") 
})

test_that("Tests for cases beyond default snake", {
  expect_equal(names(clean_names(test_df, "small_camel")),
               c("spAce", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2", 
                 "cant", "hiThere", "leadingSpaces", "x_3", "acao", "faroe", "aBCDEF"))
  expect_equal(names(clean_names(test_df, "big_camel")),
               c("SpAce", "Repeated", "A", "Percent", "X", "X_2", "D9", "Repeated_2", 
                 "Cant", "HiThere", "LeadingSpaces", "X_3", "Acao", "Faroe", "ABCDEF"))
  expect_equal(names(clean_names(test_df, "all_caps")),
               c("SP_ACE", "REPEATED", "A", "PERCENT", "X", "X_2", "D_9", "REPEATED_2", 
                 "CANT", "HI_THERE", "LEADING_SPACES", "X_3", "ACAO", "FAROE", "A_B_C_D_E_F"))
  expect_equal(names(clean_names(test_df, "lower_upper")),
               c("spACE", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2", 
                 "cant", "hiTHERE", "leadingSPACES", "x_3", "acao", "faroe", "aBcDeF"))
  expect_equal(names(clean_names(test_df, "upper_lower")),
               c("SPace", "REPEATED", "A", "PERCENT", "X", "X_2", "D9", "REPEATED_2", 
                 "CANT", "HIthere", "LEADINGspaces", "X_3", "ACAO", "FAROE", "AbCdEf"))
})
