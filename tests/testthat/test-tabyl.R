# Tests for data.frame renaming function

cyl_tbl <- tabyl(mtcars$cyl)

test_that("counts are accurate", {
  expect_equal(cyl_tbl$`mtcars$cyl`, c(4, 6, 8))
  expect_equal(cyl_tbl$n, c(11, 7, 14))
})

test_that("percentages are accurate", {
  expect_equal(cyl_tbl$percent, c(11 / 32, 7 / 32, 14 / 32))
})

# Character input, with and without NA
test_df <- data.frame(grp = c("a", "b", "b", "c"), stringsAsFactors = FALSE)
test_df_na <- data.frame(grp = c("a", "b", "b", "c", NA), stringsAsFactors = FALSE)
test_res <- tabyl(test_df$grp)
test_res_na <- tabyl(test_df_na$grp)

test_that("names are right", {
  expect_equal(names(cyl_tbl), c("mtcars$cyl", "n", "percent"))
  expect_equal(names(test_res_na), c("test_df_na$grp", "n", "percent", "valid_percent"))
})

test_that("named vectors are handled properly", { # issue 144
  x <- c(a = "x", b = "y", c = "z")
  expect_equal(names(tabyl(x))[1], "x")
})

test_that("NAs handled correctly", {
  expect_equal(test_res_na$percent, c(0.2, 0.4, 0.2, 0.2))
  expect_equal(test_res_na$valid_percent, c(0.25, 0.5, 0.25, NA))
})

test_that("show_NA = FALSE parameter works, incl. with piped input", {
  resss <- test_res
  names(resss)[1] <- "test_df_na$grp"
  names(attr(resss, "core"))[1] <- "test_df_na$grp"
  expect_equal(
    resss,
    tabyl(test_df_na$grp, show_na = FALSE)
  )
  names(attr(resss, "core"))[1] <- "grp"
  names(resss)[1] <- "grp" # for this next instance, col name changes
  expect_equal(
    resss,
    test_df_na %>% tabyl(grp, show_na = FALSE)
  )
})

test_that("ordering of result by factor levels is preserved for factors", {
  expect_equal(tabyl(factor(c("x", "y", "z"), levels = c("y", "z", "x")))[[1]], factor(c("y", "z", "x"), levels = c("y", "z", "x")))
})

# missing factor levels shown, with and without NA
fac <- iris[["Species"]][70:80] # to get versicolor, not the first alphabetically
fac_na <- fac
fac_na[1:2] <- NA


test_that("missing factor levels are displayed without NA values", {
  expect_equal(tabyl(fac)[[1]], factor(c("setosa", "versicolor", "virginica"), levels = c("setosa", "versicolor", "virginica")))
  expect_equal(tabyl(fac)[[2]], c(0, 11, 0))
  expect_equal(tabyl(fac)[[3]], c(0, 1, 0))
})

test_that("missing factor levels are displayed with NA values", {
  expect_equal(tabyl(fac_na)[[1]], factor(c("setosa", "versicolor", "virginica", NA), levels = c("setosa", "versicolor", "virginica")))
  expect_equal(tabyl(fac_na)[[2]], c(0, 9, 0, 2))
  expect_equal(tabyl(fac_na)[[3]], c(0, 9 / 11, 0, 2 / 11))
  expect_equal(tabyl(fac_na)[[4]], c(0, 1, 0, NA))
})

# piping
test_that("piping in a data.frame works", {
  x <- tabyl(mtcars$cyl)
  names(x)[1] <- "cyl"
  names(attr(x, "core"))[1] <- "cyl"
  expect_equal(
    x,
    mtcars %>% tabyl(cyl)
  )
})


test_that("column1 stays its original data type per #168, in both resulting tabyl and core", {
  # test character, logical, numeric, factor X both values for show_missing_levels; confirm class in core and in main result
  # do those 8 tests in a loop?
  loop_df <- data.frame(
    a = c(TRUE, FALSE, TRUE),
    b = c("x", "y", "y"),
    c = c(1, 1, 2), stringsAsFactors = FALSE
  )
  for (i in c("logical", "numeric", "character")) {
    for (j in c(TRUE, FALSE)) {
      loop_df_temp <- loop_df
      class(loop_df_temp$a) <- i
      loop_tab <- loop_df_temp %>% tabyl(a, b, c, show_missing_levels = j)
      expect_equal(class(loop_tab[[1]]$a), class(loop_df_temp$a))
      expect_equal(class(attr(loop_tab[[1]], "core")$a), class(loop_df_temp$a)) # check core class
    }
  }
  loop_df$a <- factor(c("hi", "lo", "hi"))
  for (j in c(TRUE, FALSE)) {
    loop_df_temp <- loop_df
    loop_tab <- loop_df_temp %>% tabyl(a, b, c, show_missing_levels = j)
    expect_equal(class(loop_tab[[1]]$a), class(loop_df_temp$a))
    expect_equal(levels(loop_tab[[1]]$a), levels(loop_df_temp$a))
    expect_equal(class(attr(loop_tab[[1]], "core")$a), class(loop_df_temp$a)) # check core class and levels
    expect_equal(levels(attr(loop_tab[[1]], "core")$a), levels(loop_df_temp$a))
  }
})

# bad inputs

test_that("failure occurs when passed unsupported types", {
  expect_error(tabyl(matrix(1:10, nrow = 5)), "input must be a vector of type logical, numeric, character, list, or factor")
  expect_error(tabyl(complex(10)), "input must be a vector of type logical, numeric, character, list, or factor")
})

test_that("bad input variable name is preserved", {
  expect_equal(
    mtcars %>% dplyr::mutate(`bad name` = cyl) %>% tabyl(`bad name`) %>% names() %>% .[[1]],
    "bad name"
  )
  k <- mtcars %>% dplyr::mutate(`bad name` = cyl)
  expect_equal(
    tabyl(k$`bad name`) %>% names() %>% .[[1]],
    "k$`bad name`"
  )
})


test_that("input variable names 'percent' and 'n' are handled", {
  a <- mtcars %>% tabyl(mpg)
  expect_equal(
    a %>% tabyl(percent),
    as_tabyl(
      data.frame(
        percent = c(1 / 32, 2 / 32),
        n = c(18, 7),
        percent_percent = c(18 / 25, 7 / 25)
      ),
      1
    )
  )
  expect_equal(
    a %>% tabyl(n),
    as_tabyl(
      data.frame(
        n = 1:2,
        n_n = c(18, 7),
        percent = c(18 / 25, 7 / 25)
      ),
      1
    )
  )
})

test_that("bizarre combination of %>%, quotes, and spaces in names is handled", {
  dat <- data.frame(
    `The candidate(s) applied directly to my school` = c("a", "b", "a", "b"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_equal(
    tabyl(dat$`The candidate(s) applied directly to my school` %>% gsub("hi", "there", .)) %>%
      names() %>%
      .[1],
    "dat$`The candidate(s) applied directly to my school` %>% gsub(\"hi\",     \"there\", .)"
  )
})

test_that("grouped data.frame inputs are handled (#125)", {
  expect_equal(
    mtcars %>% dplyr::group_by(cyl) %>% tabyl(carb, gear),
    mtcars %>% tabyl(carb, gear)
  )
})


test_that("if called on non-existent vector, returns useful error message", {
  expect_error(tabyl(mtcars$moose), "object mtcars\\$moose not found")
  expect_error(tabyl(moose), "object 'moose' not found")
  expect_error(mtcars %>% tabyl(moose))
})

test_that("if called on data.frame with no or irregular columns specified, returns informative error message", {
  expect_error(tabyl(mtcars), "if calling on a data.frame, specify unquoted column names(s) to tabulate.  Did you mean to call tabyl() on a vector?",
    fixed = TRUE
  )
  expect_error(tabyl(mtcars, var2 = am),
    "please specify var1 OR var1 & var2 OR var1 & var2 & var3",
    fixed = TRUE
  )
})

test_that("fails if called on a non-data.frame list", { # it's not meant to do this and result will likely be garbage, so fail
  L <- list(a = 1, b = "rstats")
  expect_error(tabyl(L),
    "tabyl() is meant to be called on vectors and data.frames; convert non-data.frame lists to one of these types",
    fixed = TRUE
  )
})

# showing missing factor levels

test_that("show_missing_levels parameter works", {
  z <- structure(list(
    a = structure(1, .Label = c("hi", "lo"), class = "factor"),
    b = structure(2, .Label = c("big", "small"), class = "factor"),
    new = structure(1, .Label = c("lvl1", "lvl2"), class = "factor")
  ),
  row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"),
  .Names = c("a", "b", "new")
  )

  expect_equal(
    z %>% tabyl(a, b, new, show_missing_levels = TRUE),
    list(lvl1 = data.frame(
      a = c("hi", "lo"),
      big = c(0, 0),
      small = c(1, 0),
      stringsAsFactors = TRUE
    ) %>% as_tabyl(2, "a", "b"))
  )
  expect_equal(
    z %>% tabyl(a, b, new, show_missing_levels = FALSE) %>% .[[1]],
    data.frame(
      a = factor("hi", levels = c("hi", "lo")),
      small = c(1)
    ) %>% as_tabyl(2, "a", "b")
  )

  # Works with numerics
  expect_equal(
    mtcars %>% tabyl(cyl, am),
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3, 4, 12),
      `1` = c(8, 3, 2),
      check.names = FALSE
    ) %>% as_tabyl(2, "cyl", "am")
  )
})

# NA handling - position and removal
# Putting this outside the following test block for later re-use
x <- data.frame(
  a = c(1, 2, 2, 2, 1, 1, 1, NA, NA, 1),
  b = c(rep("up", 4), rep("down", 4), NA, NA),
  c = 10,
  d = c(NA, 10:2),
  stringsAsFactors = FALSE
)

test_that("NA levels get moved to the last column in the data.frame, are suppressed properly", {
  y <- tabyl(x, a, b) %>%
    untabyl()
  expect_equal(
    y,
    data.frame(
      a = c(1, 2, NA),
      down = c(3, 0, 1),
      up = c(1, 3, 0),
      NA_ = c(1, 0, 1)
    )
  )

  expect_equal(
    tabyl(x, a, b, show_na = FALSE) %>%
      untabyl(),
    data.frame(
      a = c(1, 2),
      down = c(3, 0),
      up = c(1, 3)
    )
  )

  # one-way suppression
  expect_equal(
    tabyl(x$a, show_na = FALSE) %>%
      untabyl(),
    data.frame(
      `x$a` = 1:2,
      n = c(5, 3),
      percent = c(0.625, 0.375),
      check.names = FALSE
    )
  )

  # NA level is shown in 3 way split
  y <- x %>% tabyl(c, a, b, show_missing_levels = FALSE)
  expect_equal(length(y), 3)
  expect_equal(names(y), c("down", "up", "NA_"))
  expect_equal(
    y[["NA_"]], # column c remains numeric
    x %>% dplyr::filter(is.na(b)) %>% tabyl(c, a)
  )

  y_with_missing <- x %>% tabyl(c, a, b, show_missing_levels = TRUE)
  expect_equal(length(y_with_missing), 3)
  expect_equal(names(y_with_missing), c("down", "up", "NA_"))
  expect_equal(
    y_with_missing[["NA_"]] %>% untabyl(), # column c remains numeric
    data.frame(c = 10, `1` = 1, `2` = 0, NA_ = 1, check.names = FALSE)
  )

  # If no NA in 3rd variable, it doesn't appear in split list
  expect_equal(length(dplyr::starwars %>%
    dplyr::filter(species == "Human") %>%
    tabyl(eye_color, skin_color, gender, show_missing_levels = TRUE)), 2)

  # The starwars data set changed in dplyr v 1.0.0 so have two blocks of tests:
  if(packageVersion("dplyr") > package_version("0.8.5")){
    # If there is NA, it does appear in split list
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_missing_levels = TRUE)), 3)
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE)), 3)

    # NA level in the list gets suppressed if show_na = FALSE.  Should have one less level if NA is suppressed.
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_na = TRUE)), 3)
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_na = FALSE)), 2)
  } else {
    # If there is NA, it does appear in split list
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_missing_levels = TRUE)), 5)
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE)), 5)

    # NA level in the list gets suppressed if show_na = FALSE.  Should have one less level if NA is suppressed.
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_na = TRUE)), 5)
    expect_equal(length(dplyr::starwars %>%
                          tabyl(eye_color, skin_color, gender, show_na = FALSE)), 4)
  }
})

test_that("zero-row and fully-NA inputs are handled", {
  zero_vec <- character(0)
  expect_equal(nrow(tabyl(zero_vec)), 0)
  expect_equal(names(tabyl(zero_vec)), c("zero_vec", "n", "percent"))

  zero_df <- data.frame(a = character(0), b = character(0))
  expect_message(
    expect_equal(nrow(tabyl(zero_df, a, b)), 0)
  )
  expect_message(
    expect_equal(names(tabyl(zero_df, a, b)), "a"),
    "No records to count so returning a zero-row tabyl"
  )

  all_na_df <- data.frame(a = c(NA, NA), b = c(NA_character_, NA_character_))
  expect_message(
    expect_equal(tabyl(all_na_df, a, b, show_na = FALSE) %>% nrow(), 0)
  )
  expect_message(
    expect_equal(tabyl(all_na_df, a, b, show_na = FALSE) %>% names(), "a"),
    "No records to count so returning a zero-row tabyl"
  )
})

test_that("print.tabyl prints without row numbers", {
  expect_equal(
    mtcars %>% tabyl(am, cyl) %>% capture.output(),
    c(" am 4 6  8", "  0 3 4 12", "  1 8 3  2")
  )
})

test_that("the dplyr warning suggesting forcats::fct_explicit_na that is generated by a tabyl of a factor with NA values is caught ", {
  # leaving this in as I'd want to know if it ever gets loud again, but the warning seems to be gone in
  # dplyr 1.0.0 and I have removed the withCallingHandlers({}) code in tabyl() that this was testing
  expect_silent(
    tabyl(factor(c("a", "b", NA)))
  )
  xx <- data.frame(a = factor(c("a", "b", NA)),
                  b = 1:3)
  expect_silent(xx %>%
    tabyl(a, b)
  )
})

test_that("3-way tabyl with 3rd var factor is listed in right order, #250", {
  z <- mtcars
  z$cyl <- factor(z$cyl, levels = c(4, 8, 6))
  expect_equal(names(tabyl(z, am, gear, cyl)), c("4", "8", "6"))
  z$cyl[32] <- NA
  expect_equal(names(tabyl(z, am, gear, cyl)), c("4", "8", "6", "NA_"))
  expect_equal(names(tabyl(z, am, gear, cyl, show_na = FALSE)), c("4", "8", "6"))
  z <- z %>% dplyr::filter(! cyl %in% "4")
  expect_equal(names(tabyl(z, am, gear, cyl)), c("8", "6", "NA_"))
})

test_that("tabyl works with ordered 1st variable, #386", {
  mt_ordered <- mtcars
  mt_ordered$cyl <- ordered(mt_ordered$cyl, levels = c("4", "8", "6"))

  ordered_3way <- mt_ordered %>%
    tabyl(cyl, gear, am)
  expect_equal(class(ordered_3way[[1]]$cyl), c("ordered", "factor")) # 1st col in resulting tabyl
  expect_equal(class(attr(ordered_3way[[1]], "core")$cyl), c("ordered", "factor")) # 1st col in tabyl core
})

test_that("factor ordering of columns is correct in 2-way tabyl", {
  two_factors <- data.frame(
    x = factor(c("big", "small", "medium", "small"),
               levels = c("small", "medium", "big")),
    y = factor(c("hi", "hi", "hi", "lo"),
               levels = c("lo", "hi"))
  )
  expect_equal(
    two_factors %>%
      tabyl(x, y) %>%
      names(),
    c("x", "lo", "hi")
  )
})

test_that("empty strings converted to _emptystring", {
  mt_empty <- mtcars
  mt_empty$cyl[1:2] <- c("", NA_character_)
  expect_equal(
    mt_empty %>%
      tabyl(am, cyl) %>%
      names,
    c("am", "4", "6", "8", "emptystring_", "NA_")
  )
})

test_that("3way tabyls with factors in cols 1-2 are arranged correctly, #379", {

  dat_3wayfactors <- data.frame(
    gender = c("f", "m", "m", "f", "m"),
    age_group = factor(
      c("18-35", "46-55", "46-55", "36-45", ">55"),
      levels = c("18-35", "36-45", "46-55", ">55")),
    bmi_group = factor(
      c("18.5 - 25", "25 - 30", "18.5 - 25", ">30", "<18.5"),
      levels = c("<18.5", "18.5 - 25", "25 - 30", ">30")),
    stringsAsFactors = TRUE
    )

  tabyl_3wf <- dat_3wayfactors %>%
    tabyl(bmi_group, age_group, gender, show_missing_levels = FALSE)

  expect_equal(names(tabyl_3wf$m), c("bmi_group", "46-55", ">55"))
  expect_equal(tabyl_3wf$f[[1]],
               factor(
                 c("18.5 - 25", ">30"),
                 levels = c("<18.5", "18.5 - 25", "25 - 30", ">30")
               )
  )
})

test_that("tabyl errors informatively called like tabyl(mtcars$cyl, mtcars$gear), #377", {
  expect_error(
    tabyl(mtcars$cyl, mtcars$am),
    regexp = "Did you try to call tabyl on two vectors"
  )
  has_logicals <- data.frame(x = 1:2, y = c(TRUE, FALSE))
  expect_error(
    tabyl(has_logicals$x, has_logicals$y),
    regexp = "Did you try to call tabyl on two vectors"
  )
  expect_type(
    has_logicals %>%
      tabyl(x, y),
    "list"
  )
})

test_that("2-way tabyl with numeric column names is sorted numerically", {
  df <- data.frame(var1 = c(1:11), var2 = c(NA, 10:1))
  expect_equal(colnames(df %>% tabyl(var1, var2)), c("var1", 1:10, "NA_"))
})

test_that("3-way tabyl with numeric names is sorted numerically", {
  expect_equal(names(mtcars %>% tabyl(gear, cyl, hp)),
               as.character(sort(unique(mtcars$hp))))

  # Check putting NA last - data.frame "x" is created way above
  expect_equal(
    names(x %>% tabyl(a, c, d)),
    c(2:10, "NA_"))
})
