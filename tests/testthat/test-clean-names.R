# Tests for data.frame renaming function

library(janitor)
context("clean_names")

test_df <- data.frame(matrix(ncol = 20) %>% as.data.frame())
names(test_df) <- c(
  "sp ace", "repeated", "a**^@", "%", "*", "!",
  "d(!)9", "REPEATED", "can\"'t", "hi_`there`", "  leading spaces",
  "€", "ação", "Farœ", "a b c d e f", "testCamelCase", "!leadingpunct",
  "average # of days", "jan2009sales", "jan 2009 sales"
)

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
  expect_equal(names(clean)[16], "test_camel_case") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[17], "leadingpunct") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[18], "average_number_of_days") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[19], "jan2009sales") # no separator around number-word boundary if not existing already
  expect_equal(names(clean)[20], "jan_2009_sales") # yes separator around number-word boundary if it existed
})

test_that("Returns a data.frame", {
  expect_is(clean, "data.frame")
})

test_that("Tests for cases beyond default snake", {
  expect_equal(
    names(clean_names(test_df, "small_camel")),
    c(
      "spAce", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2",
      "cant", "hiThere", "leadingSpaces", "x_3", "acao", "faroe", "aBCDEF", "testCamelCase", "leadingpunct", "averageNumberOfDays", "jan2009Sales", "jan2009Sales_2"
    )
  )
  expect_equal(
    names(clean_names(test_df, "big_camel")),
    c(
      "SpAce", "Repeated", "A", "Percent", "X", "X_2", "D9", "Repeated_2",
      "Cant", "HiThere", "LeadingSpaces", "X_3", "Acao", "Faroe", "ABCDEF", "TestCamelCase", "Leadingpunct", "AverageNumberOfDays", "Jan2009Sales", "Jan2009Sales_2"
    )
  )
  expect_equal(
    names(clean_names(test_df, "all_caps")),
    c(
      "SP_ACE", "REPEATED", "A", "PERCENT", "X", "X_2", "D_9", "REPEATED_2",
      "CANT", "HI_THERE", "LEADING_SPACES", "X_3", "ACAO", "FAROE", "A_B_C_D_E_F", "TEST_CAMEL_CASE", "LEADINGPUNCT", "AVERAGE_NUMBER_OF_DAYS", "JAN2009SALES", "JAN_2009_SALES"
    )
  )
  expect_equal(
    names(clean_names(test_df, "lower_upper")),
    c(
      "spACE", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2",
      "cant", "hiTHERE", "leadingSPACES", "x_3", "acao", "faroe", "aBcDeF", "testCAMELcase", "leadingpunct", "averageNUMBERofDAYS", "jan2009SALES", "jan2009SALES_2"
    )
  )
  expect_equal(
    names(clean_names(test_df, "upper_lower")),
    c(
      "SPace", "REPEATED", "A", "PERCENT", "X", "X_2", "D9", "REPEATED_2",
      "CANT", "HIthere", "LEADINGspaces", "X_3", "ACAO", "FAROE", "AbCdEf", "TESTcamelCASE", "LEADINGPUNCT", "AVERAGEnumberOFdays", "JAN2009sales", "JAN2009sales_2"
    )
  )
  expect_equal(
    names(clean_names(test_df, "old_janitor")),
    c(
      "sp_ace", "repeated", "a", "percent", "x", "x_2", "d_9", "repeated_2",
      "cant", "hi_there", "leading_spaces", "x_3", "ação", "farœ",
      "a_b_c_d_e_f", "testcamelcase", "x_leadingpunct", "average_of_days", "jan2009sales", "jan_2009_sales"
    )
  )
  # check that alias arguments yield identical outputs
  expect_equal(names(clean_names(test_df, "screaming_snake")), names(clean_names(test_df, "all_caps")))
  expect_equal(names(clean_names(test_df, "big_camel")), names(clean_names(test_df, "upper_camel")))
  expect_equal(names(clean_names(test_df, "small_camel")), names(clean_names(test_df, "lower_camel")))
})

test_that("errors if not called on a data.frame", {
  expect_error(clean_names(1:3), "clean_names() must be called on a data.frame.  Consider janitor::make_clean_names() for other cases of manipulating vectors of names.", fixed = TRUE)
})