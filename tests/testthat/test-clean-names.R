context("clean_names")

# Do not run the tests if not all transliterators are available

options(janitor_warn_transliterators=NULL)
check_warning_transliterator <-
  tryCatch(
    available_transliterators(c("Any-Latin", "Greek-Latin", "Latin-ASCII")),
    error=function(e) e,
    warning=function(w) w
  )
skip_if_not(
  is.character(check_warning_transliterator),
  message="Not all transliterators are available, so some tests will fail.  Skipping `clean_names()` tests."
)

# Tests for make_clean_names ####

test_that("All scenarios for make_clean_names", {
  expect_equal(
    make_clean_names("sp ace"),
    "sp_ace"
  )
  expect_equal(
    make_clean_names(c("repeated", "repeated", "REPEATED")),
    paste0("repeated", c("", "_2", "_3"))
  )
  expect_equal(
    make_clean_names("a**^@"),
    "a"
  )
  expect_equal(
    make_clean_names("%"),
    "percent"
  )
  expect_equal(
    make_clean_names("%", replace=c("%"="foo")),
    "foo",
    info="Verify that `replace` works."
  )
  expect_equal(
    make_clean_names("*"),
    "x"
  )
  expect_equal(
    make_clean_names("!"),
    "x"
  )
  expect_equal(
    make_clean_names(c("*", "!")),
    c("x", "x_2")
  )
  expect_equal(
    make_clean_names("d(!)9"),
    "d_9"
  )
  expect_equal(
    make_clean_names("can\"'t"),
    "cant"
  )
  expect_equal(
    make_clean_names("hi_`there`"),
    "hi_there"
  )
  expect_equal(
    make_clean_names("  leading spaces"),
    "leading_spaces"
  )
  expect_equal(
    make_clean_names("€"),
    "x"
  )
  # This test will fail for some locales because the ascii translation is
  # required to make the function locale-independent.
  expect_equal(
    make_clean_names("ação", ascii=FALSE),
    "acao"
  )
  expect_equal(
    make_clean_names("ação"),
    "acao"
  )
  expect_equal(
    make_clean_names("Farœ"),
    "faroe"
  )
  expect_equal(
    make_clean_names("a b c d e f"),
    "a_b_c_d_e_f"
  )
  expect_equal(
    make_clean_names("testCamelCase"),
    "test_camel_case"
  )
  expect_equal(
    make_clean_names("!leadingpunct"),
    "leadingpunct"
  )
  expect_equal(
    make_clean_names("average # of days"),
    "average_number_of_days"
  )
  expect_equal(
    make_clean_names("jan2009sales"),
    "jan2009sales"
  )
  expect_equal(
    make_clean_names("jan 2009 sales"),
    "jan_2009_sales"
  )
  expect_equal(
    make_clean_names("not_first_unicode_µ"),
    "not_first_unicode_m"
  )
  expect_equal(
    make_clean_names("µ_first_unicode"),
    "m_first_unicode"
  )
  expect_equal(
    make_clean_names("a/b"),
    "a_b",
    info="No custom replacement"
  )
  expect_equal(
    make_clean_names("a/b", replace=c("/"="_per_")),
    "a_per_b",
    info="Custom replacement"
  )
  
  expect_equal(
    make_clean_names("m\xb6"),
    "m",
    info="ASCII that is not in the expected range without being replaceable is removed"
  )
  
  # Fix issue #388 (that issue was specific to \xb2)
  # expect_equal(
  #   make_clean_names("m\x83\x84\x85\x86\x87\xa1"),
  #   "mf",
  #   info="extended ASCII test 1"
  # )
  # expect_equal(
  #   make_clean_names("m\xa9\xaa\xae\xb2\xb3\xb5\xbc\xbd\xbe\xc0"),
  #   "m_c_a_r_23m1_41_23_4a",
  #   info="extended ASCII test 2"
  # )
  expect_equal(
    make_clean_names("m\u00b2"),
    "m2",
    info="Convert Unicode superscript 2 to regular 2"
  )
})

test_that("locale-specific make_clean_names tests", {
  orig_locale <- Sys.getlocale(category="LC_CTYPE")
  Sys.setlocale(locale="C")
  expect_equal(
    make_clean_names("介護_看護_女"),
    "jie_hu_kan_hu_nu",
    info="Unicode transliteration happens with make.names()"
  )
  expect_equal(
    make_clean_names("介護_看護_女", use_make_names=FALSE, ascii=FALSE),
    "介護_看護_女",
    info="Unicode transliteration does not happen without make.names() and without ascii"
  )
  expect_equal(
    make_clean_names("μ"),
    "m",
    info="lower-case mu is transliterated to an 'm'"
  )
  expect_equal(
    make_clean_names("µ", ascii=FALSE, use_make_names=FALSE),
    "µ",
    info="lower-case mu is not transliterated to an 'm' and uses the "
  )
  Sys.setlocale(locale=orig_locale)
})

test_that("do not create duplicates (fix #251)", {
  expect_equal(
    make_clean_names(c("a", "a", "a_2")),
    c("a", "a_2", "a_2_2")
  )
})

# Tests for clean_names ####
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
    "€",
    "ação",
    "Farœ",
    "a b c d e f",
    "testCamelCase",
    "!leadingpunct",
    "average # of days",
    "jan2009sales",
    "jan 2009 sales",
    "not_first_unicode_µ",
    "µ_first_unicode"
  )

test_df <- as.data.frame(matrix(ncol = 22))
names(test_df) <- testing_vector
clean <- clean_names(test_df, "snake", ascii=TRUE)
clean_noascii <- clean_names(test_df, "snake", ascii=FALSE)

test_that("Returns a data.frame", {
  expect_is(clean, "data.frame")
})

test_that("Tests for cases beyond default snake", {
  expect_equal(
    names(clean_names(test_df, "small_camel")),
    c(
      "spAce", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2",
      "cant", "hiThere", "leadingSpaces", "x_3", "acao", "faroe", "aBCDEF",
      "testCamelCase", "leadingpunct", "averageNumberOfDays",
      "jan2009Sales", "jan2009Sales_2", "notFirstUnicodeM", "mFirstUnicode"
    )
  )
  expect_equal(
    names(clean_names(test_df, "big_camel")),
    c(
      "SpAce", "Repeated", "A", "Percent", "X", "X_2", "D9", "Repeated_2",
      "Cant", "HiThere", "LeadingSpaces", "X_3", "Acao", "Faroe", "ABCDEF", "TestCamelCase",
      "Leadingpunct", "AverageNumberOfDays", "Jan2009Sales", "Jan2009Sales_2",
      "NotFirstUnicodeM", "MFirstUnicode"
    )
  )
  expect_equal(
    names(clean_names(test_df, "all_caps")),
    c(
      "SP_ACE", "REPEATED", "A", "PERCENT", "X", "X_2", "D_9", "REPEATED_2",
      "CANT", "HI_THERE", "LEADING_SPACES", "X_3", "ACAO", "FAROE", "A_B_C_D_E_F",
      "TEST_CAMEL_CASE", "LEADINGPUNCT", "AVERAGE_NUMBER_OF_DAYS", "JAN2009SALES",
      "JAN_2009_SALES",
      "NOT_FIRST_UNICODE_M", "M_FIRST_UNICODE"
    )
  )
  expect_equal(
    names(clean_names(test_df, "lower_upper")),
    c(
      "spACE", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2",
      "cant", "hiTHERE", "leadingSPACES", "x_3", "acao", "faroe", "aBcDeF",
      "testCAMELcase", "leadingpunct", "averageNUMBERofDAYS", "jan2009SALES",
      "jan2009SALES_2", "notFIRSTunicodeM", "mFIRSTunicode"
    )
  )
  expect_equal(
    names(clean_names(test_df, "upper_lower")),
    c(
      "SPace", "REPEATED", "A", "PERCENT", "X", "X_2", "D9", "REPEATED_2",
      "CANT", "HIthere", "LEADINGspaces", "X_3", "ACAO", "FAROE", "AbCdEf",
      "TESTcamelCASE", "LEADINGPUNCT", "AVERAGEnumberOFdays", "JAN2009sales",
      "JAN2009sales_2", "NOTfirstUNICODEm", "MfirstUNICODE"
    )
  )
  expect_equal(
    names(clean_names(test_df, "none")),
    c(
      "sp_ace", "repeated", "a", "percent", "X", "X_2", "d_9", "REPEATED",
      "cant", "hi_there", "leading_spaces", "X_3", "acao", "Faroe", "a_b_c_d_e_f", 
      "testCamelCase", "leadingpunct", "average_number_of_days", 
      "jan2009sales", "jan_2009_sales", "not_first_unicode_m", "m_first_unicode"
    )
  )
  expect_equal(
    names(clean_names(test_df, "old_janitor")),
    c(
      "sp_ace", "repeated", "a", "percent", "x", "x_2", "d_9", "repeated_2",
      "cant", "hi_there", "leading_spaces", "x_3", "ação", "farœ",
      "a_b_c_d_e_f", "testcamelcase", "x_leadingpunct", "average_of_days",
      "jan2009sales", "jan_2009_sales", "not_first_unicode_µ", "µ_first_unicode"
    )
  )
  # check that alias arguments yield identical outputs
  expect_equal(names(clean_names(test_df, "screaming_snake")), names(clean_names(test_df, "all_caps")))
  expect_equal(names(clean_names(test_df, "big_camel")), names(clean_names(test_df, "upper_camel")))
  expect_equal(names(clean_names(test_df, "small_camel")), names(clean_names(test_df, "lower_camel")))
})

test_that("errors if not called on a data.frame", {
  expect_error(
    clean_names(1:3),
    regexp="No `clean_names()` method exists for the class integer",
    fixed=TRUE
  )
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
  expect_is(clean, "sf")
})

test_that("Names are cleaned appropriately", {
  skip_if_not_installed("sf")
  test_df <- data.frame(matrix(ncol = 22) %>% as.data.frame())
  
  names(test_df) <- c(
    "sp ace", "repeated", "a**^@", "%", "*", "!",
    "d(!)9", "REPEATED", "can\"'t", "hi_`there`", "  leading spaces",
    "€", "ação", "Farœ", "a b c d e f", "testCamelCase", "!leadingpunct",
    "average # of days", "jan2009sales", "jan 2009 sales", "long", "lat"
  )
  
  test_df["long"] <- -80
  test_df["lat"] <- 40
  
  test_df <- sf::st_as_sf(test_df, coords = c("long", "lat"))
  names(test_df)[21] <- "Geometry"
  sf::st_geometry(test_df) <- "Geometry"
  
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
  expect_equal(names(clean)[14], "faroe") # œ character was failing to convert on Windows, should work universally for stringi 1.1.6 or higher
  # https://github.com/sfirke/janitor/issues/120#issuecomment-303385418
  expect_equal(names(clean)[15], "a_b_c_d_e_f") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[16], "test_camel_case") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[17], "leadingpunct") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[18], "average_number_of_days") # for testing alternating cases below with e.g., case = "upper_lower"
  expect_equal(names(clean)[19], "jan2009sales") # no separator around number-word boundary if not existing already
  expect_equal(names(clean)[20], "jan_2009_sales") # yes separator around number-word boundary if it existed
})

test_that("Tests for cases beyond default snake for sf objects", {
  skip_if_not_installed("sf")
  test_df <- data.frame(matrix(ncol = 22) %>% as.data.frame())
  
  names(test_df) <- c(
    "sp ace", "repeated", "a**^@", "%", "*", "!",
    "d(!)9", "REPEATED", "can\"'t", "hi_`there`", "  leading spaces",
    "€", "ação", "Farœ", "a b c d e f", "testCamelCase", "!leadingpunct",
    "average # of days", "jan2009sales", "jan 2009 sales", "long", "lat"
  )
  
  test_df["long"] <- -80
  test_df["lat"] <- 40
  
  test_df <- sf::st_as_sf(test_df, coords = c("long", "lat"))
  names(test_df)[21] <- "geometry"
  sf::st_geometry(test_df) <- "geometry"
  
  expect_equal(
    names(clean_names(test_df, "small_camel")),
    c(
      "spAce", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2",
      "cant", "hiThere", "leadingSpaces", "x_3", "acao", "faroe", "aBCDEF", "testCamelCase", "leadingpunct", "averageNumberOfDays", "jan2009Sales", "jan2009Sales_2", "geometry"
    )
  )
  expect_equal(
    names(clean_names(test_df, "big_camel")),
    c(
      "SpAce", "Repeated", "A", "Percent", "X", "X_2", "D9", "Repeated_2",
      "Cant", "HiThere", "LeadingSpaces", "X_3", "Acao", "Faroe", "ABCDEF", "TestCamelCase", "Leadingpunct", "AverageNumberOfDays", "Jan2009Sales", "Jan2009Sales_2", "geometry"
    )
  )
  expect_equal(
    names(clean_names(test_df, "all_caps")),
    c(
      "SP_ACE", "REPEATED", "A", "PERCENT", "X", "X_2", "D_9", "REPEATED_2",
      "CANT", "HI_THERE", "LEADING_SPACES", "X_3", "ACAO", "FAROE", "A_B_C_D_E_F", "TEST_CAMEL_CASE", "LEADINGPUNCT", "AVERAGE_NUMBER_OF_DAYS", "JAN2009SALES", "JAN_2009_SALES", "geometry"
    )
  )
  expect_equal(
    names(clean_names(test_df, "lower_upper")),
    c(
      "spACE", "repeated", "a", "percent", "x", "x_2", "d9", "repeated_2",
      "cant", "hiTHERE", "leadingSPACES", "x_3", "acao", "faroe", "aBcDeF", "testCAMELcase", "leadingpunct", "averageNUMBERofDAYS", "jan2009SALES", "jan2009SALES_2", "geometry"
    )
  )
  expect_equal(
    names(clean_names(test_df, "upper_lower")),
    c(
      "SPace", "REPEATED", "A", "PERCENT", "X", "X_2", "D9", "REPEATED_2",
      "CANT", "HIthere", "LEADINGspaces", "X_3", "ACAO", "FAROE", "AbCdEf", "TESTcamelCASE", "LEADINGPUNCT", "AVERAGEnumberOFdays", "JAN2009sales", "JAN2009sales_2", "geometry"
    )
  )
  expect_equal(
    names(clean_names(test_df, "none")),
    c(
      "sp_ace", "repeated", "a", "percent", "X", "X_2", "d_9", "REPEATED",
      "cant", "hi_there", "leading_spaces", "X_3", "acao", "Faroe", "a_b_c_d_e_f", 
      "testCamelCase", "leadingpunct", "average_number_of_days", 
      "jan2009sales", "jan_2009_sales", "geometry"
    )
  )
  expect_equal(
    names(clean_names(test_df, "old_janitor")),
    c(
      "sp_ace", "repeated", "a", "percent", "x", "x_2", "d_9", "repeated_2",
      "cant", "hi_there", "leading_spaces", "x_3", "ação", "farœ",
      "a_b_c_d_e_f", "testcamelcase", "x_leadingpunct", "average_of_days", "jan2009sales", "jan_2009_sales", "geometry"
    )
  )
})

#------------------------------------------------------------------------------# 
#------------------------ Tests for tbl_graph method --------------------------#####
#------------------------------------------------------------------------------#

context("clean_names.tbl_graph")

test_that("tbl_graph/tidygraph", {
  skip_if_not_installed("tidygraph")
  # create test graph to test clean_names
  test_graph <-
    tidygraph::play_erdos_renyi(10, 0.5) %>% 
    # create nodes wi
    tidygraph::bind_nodes(test_df) %>% 
    tidygraph::mutate_all(tidyr::replace_na, 1)

  # create a graph with clean names
  clean_graph <- clean_names(test_graph, case = "snake")

  # get clean names
  clean <- names(tibble::as_tibble(clean_graph))
  expect_is(
    clean_graph, "tbl_graph",
    info="Returns a tbl_graph object"
  )

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

test_that("Work around incomplete stringi transliterators (Fix #365)", {
  options(janitor_warn_transliterators=NULL)
  expect_warning(
    available_transliterators("foo"),
    regexp="Some transliterators to convert characters in names are not available"
  )
  # The warning only occurs once per session
  expect_silent(
    available_transliterators("foo")
  )
  expect_equal(
    available_transliterators("foo"),
    ""
  )
})

test_that("groupings are preserved, #260", {
  df_grouped <- iris %>% dplyr::group_by(Sepal.Length, Sepal.Width) # nonsense for analysis but doesn't matter
  df_grouped_renamed <- df_grouped %>% clean_names(case = "lower_camel")
  expect_equal(group_vars(df_grouped_renamed), c("sepalLength", "sepalWidth")) # group got renamed
  expect_equal(names(df_grouped_renamed), c("sepalLength", "sepalWidth", "petalLength", "petalWidth", "species"))
  
  ## Test that it works on this data.frame with terrible names from the janitor vignette:
  ## Because right off the bat switching to dplyr::rename_all, it doesn't
  test_df <- as.data.frame(matrix(ncol = 6))
  names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                      "REPEAT VALUE", "REPEAT VALUE", "")
  expect_equal(names(clean_names(test_df)), c("first_name", "abc", "percent_successful_2009", "repeat_value", 
                                 "repeat_value_2", "x"))
})

test_that("groupings are preserved, #260", {
  df_grouped <- iris %>% dplyr::group_by(Sepal.Length, Sepal.Width) # nonsense for analysis but doesn't matter
  df_grouped_renamed <- df_grouped %>% clean_names(case = "lower_camel")
  expect_equal(group_vars(df_grouped_renamed), c("sepalLength", "sepalWidth")) # group got renamed
  expect_equal(names(df_grouped_renamed), c("sepalLength", "sepalWidth", "petalLength", "petalWidth", "species"))
  
  ## Test that it works on this data.frame with terrible names from the janitor vignette:
  ## Because right off the bat switching to dplyr::rename_all, it doesn't
  test_df <- as.data.frame(matrix(ncol = 6))
  names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                      "REPEAT VALUE", "REPEAT VALUE", "")
  expect_equal(names(clean_names(test_df)), c("first_name", "abc", "percent_successful_2009", "repeat_value", 
                                 "repeat_value_2", "x"))
})
