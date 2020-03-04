# Tests for data.frame renaming function

library(janitor)
context("clean_names")

testing_vector <-
  c(
    "sp ace",
    "repeated",
    "a**^@",
    "%",
    "*",
    "!",
    "d(!)9",
    "REPEATED",
    "can\"'t",
    "hi_`there`",
    "  leading spaces",
    "â‚¬",
    "aÃ§Ã£o",
    "FarÅ“",
    "a b c d e f",
    "testCamelCase",
    "!leadingpunct",
    "average # of days",
    "jan2009sales",
    "jan 2009 sales",
    "not_first_unicode_µ",
    "µ_first_unicode"
  )
result_vector_noascii <-
  c(
    "sp_ace", # spaces
    "repeated", # first instance of repeat
    "a", # multiple special chars, trailing special chars
    "percent", # converting % to percent
    "x", # 100% invalid name
    "x_2", # repeat of invalid name
    "d_9", # multiple special characters
    "repeated_2", # uppercase, 2nd instance of repeat
    "cant", # uppercase, 2nd instance of repeat
    "hi_there", # double-underscores to single
    "leading_spaces", # leading spaces
    "x_3", # euro sign, invalid
    "acao", # accented word, transliterated to latin,
    "faroe", # Å“ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
    # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
    "a_b_c_d_e_f", # for testing alternating cases below with e.g., case = "upper_lower"
    "test_camel_case", # for testing alternating cases below with e.g., case = "upper_lower"
    "leadingpunct", # for testing alternating cases below with e.g., case = "upper_lower"
    "average_number_of_days", # for testing alternating cases below with e.g., case = "upper_lower"
    "jan2009sales", # no separator around number-word boundary if not existing already
    "jan_2009_sales", # yes separator around number-word boundary if it existed
    "not_first_unicode_µ",
    "µ_first_unicode"
  )

result_vector_ascii <-
  c(
    "sp_ace", # spaces
    "repeated", # first instance of repeat
    "a", # multiple special chars, trailing special chars
    "percent", # converting % to percent
    "x", # 100% invalid name
    "x_2", # repeat of invalid name
    "d_9", # multiple special characters
    "repeated_2", # uppercase, 2nd instance of repeat
    "cant", # uppercase, 2nd instance of repeat
    "hi_there", # double-underscores to single
    "leading_spaces", # leading spaces
    "ac_a_a", # euro sign, converted
    "a_a_a_o", # accented word, transliterated to latin,
    "far_a_a", # Å“ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
    # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
    "a_b_c_d_e_f", # for testing alternating cases below with e.g., case = "upper_lower"
    "test_camel_case", # for testing alternating cases below with e.g., case = "upper_lower"
    "leadingpunct", # for testing alternating cases below with e.g., case = "upper_lower"
    "average_number_of_days", # for testing alternating cases below with e.g., case = "upper_lower"
    "jan2009sales", # no separator around number-word boundary if not existing already
    "jan_2009_sales", # yes separator around number-word boundary if it existed
    "not_first_unicode_µ",
    "µ_first_unicode"
  )

# Tests for make_clean_names ####

test_that("make_clean_names are tested appropriately", {
  expect_equal(
    make_clean_names(testing_vector, ascii=TRUE),
    result_vector_ascii
  )
  expect_equal(
    make_clean_names(testing_vector, ascii=FALSE),
    result_vector_noascii
  )
  expect_equal(
    expect_warning(
      make_clean_names("µ"),
      regexp="Names have been converted to ASCII"
    ),
    "x"
  )
  expect_equal(
    expect_silent(
      make_clean_names("µ", ascii=TRUE)
    ),
    "x"
  )
  expect_equal(
    expect_silent(
      make_clean_names("µ", ascii=FALSE)
    ),
    "x"
  )
  expect_equal(
    make_clean_names("Ã", ascii=FALSE),
    "a"
  )
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
  expect_equal(names(clean)[14], "faroe") # Å“ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
  # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
  expect_equal(names(clean)[15], "a_b_c_d_e_f") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[16], "test_camel_case") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[17], "leadingpunct") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[18], "average_number_of_days") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[19], "jan2009sales") # no separator around number-word boundary if not existing already
  expect_equal(names(clean)[20], "jan_2009_sales") # yes separator around number-word boundary if it existed
  expect_equal(names(clean)[21], "not_first_unicode_µ")
  expect_equal(names(clean)[22], "µ_first_unicode")
})

test_df <- as.data.frame(matrix(ncol = 22))
names(test_df) <- testing_vector
clean <- clean_names(test_df, "snake", ascii=TRUE)
clean_noascii <- clean_names(test_df, "snake", ascii=FALSE)

test_that("Names are cleaned appropriately", {
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
    names(clean_names(test_df, "none")),
    c(
      "sp_ace", "repeated", "a", "percent", "X", "X_2", "d_9", "REPEATED",
      "cant", "hi_there", "leading_spaces", "X_3", "acao", "Faroe", "a_b_c_d_e_f", 
      "testCamelCase", "leadingpunct", "average_number_of_days", 
      "jan2009sales", "jan_2009_sales"
    )
  )
  expect_equal(
    names(clean_names(test_df, "old_janitor")),
    c(
      "sp_ace", "repeated", "a", "percent", "x", "x_2", "d_9", "repeated_2",
      "cant", "hi_there", "leading_spaces", "x_3", "aÃ§Ã£o", "farÅ“",
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


#------------------------------------------------------------------------------# 
#---------------------------- Tests for sf method -----------------------------####
#------------------------------------------------------------------------------#

context("clean_names.sf")

test_that("Names are cleaned appropriately without attaching sf", {
  skip_if_not_installed("sf")
  nc    <- sf::st_read(system.file("shape/nc.shp", package="sf"))
  clean <- clean_names(nc, "snake")
  
  expect_equal(names(clean)[4], "cnty_id")
})

test_that("Names are cleaned appropriately", {
  skip_if_not_installed("sf")
  library(sf)
  test_df <- data.frame(matrix(ncol = 22) %>% as.data.frame())
  
  names(test_df) <- c(
    "sp ace", "repeated", "a**^@", "%", "*", "!",
    "d(!)9", "REPEATED", "can\"'t", "hi_`there`", "  leading spaces",
    "â‚¬", "aÃ§Ã£o", "FarÅ“", "a b c d e f", "testCamelCase", "!leadingpunct",
    "average # of days", "jan2009sales", "jan 2009 sales", "long", "lat"
  )
  
  test_df["long"] <- -80
  test_df["lat"] <- 40
  
  test_df <- st_as_sf(test_df, coords = c("long", "lat"))
  names(test_df)[21] <- "Geometry"
  st_geometry(test_df) <- "Geometry"
  
  clean <- clean_names(test_df, "snake")
  
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
  expect_equal(names(clean)[14], "faroe") # Å“ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
  # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
  expect_equal(names(clean)[15], "a_b_c_d_e_f") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[16], "test_camel_case") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[17], "leadingpunct") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[18], "average_number_of_days") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[19], "jan2009sales") # no separator around number-word boundary if not existing already
  expect_equal(names(clean)[20], "jan_2009_sales") # yes separator around number-word boundary if it existed
})
test_that("Returns a sf object", {

test_that("Returns a sf object", {
  expect_is(clean, "sf")
})



#------------------------------------------------------------------------------# 
#------------------------ Tests for tbl_graph method --------------------------#####
#------------------------------------------------------------------------------#

library(tidygraph)
context("clean_names.tbl_graph")

# create test graph to test clean_names
test_graph <- play_erdos_renyi(10, 0.5) %>% 
  # create nodes wi
  bind_nodes(test_df) %>% 
  mutate_all(replace_na, 1)

# create a graph with clean names
clean_graph <- clean_names(test_graph, case = "snake")

# extract the names from the tbl_graph objevt
# this was a little difficult stole some lines from print.tbl_graph
# https://github.com/thomasp85/tidygraph/blob/master/R/tbl_graph.R

arg_list <- list()
top <- do.call(trunc_mat, modifyList(arg_list, list(x = as_tibble(clean_graph), n = 0)))

# get clean names
clean <- names(top$mcf)

test_that("Names are cleaned appropriately", {
  expect_equal(clean[1], "sp_ace") # spaces
  expect_equal(clean[2], "repeated") # first instance of repeat
  expect_equal(clean[3], "a") # multiple special chars, trailing special chars
  expect_equal(clean[4], "percent") # converting % to percent
  expect_equal(clean[5], "x") # 100% invalid name
  expect_equal(clean[6], "x_2") # repeat of invalid name
  expect_equal(clean[7], "d_9") # multiple special characters
  expect_equal(clean[8], "repeated_2") # uppercase, 2nd instance of repeat
  expect_equal(clean[9], "cant") # uppercase, 2nd instance of repeat
  expect_equal(clean[10], "hi_there") # double-underscores to single
  expect_equal(clean[11], "leading_spaces") # leading spaces
  expect_equal(clean[12], "x_3") # euro sign, invalid
  expect_equal(clean[13], "acao") # accented word, transliterated to latin,
  expect_equal(clean[14], "faroe") # œ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
  # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
  expect_equal(clean[15], "a_b_c_d_e_f") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(clean[16], "test_camel_case") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(clean[17], "leadingpunct") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(clean[18], "average_number_of_days") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(clean[19], "jan2009sales") # no separator around number-word boundary if not existing already
  expect_equal(clean[20], "jan_2009_sales") # yes separator around number-word boundary if it existed
})

test_that("Returns a tbl_graph object", {
  expect_is(clean_graph, "tbl_graph")
})

