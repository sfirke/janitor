source_an <- tibble::tibble(
  x = c(rep("a", 500), "b", "b", "c", "d"),
  y = rep(c(0, 0, 0, 0, 0, 1), 84)
) %>%
  tabyl(x, y)


test_that("spacing is correct", {
  expect_equal(
    source_an %>%
      adorn_totals() %>%
      adorn_percentages("all") %>%
      adorn_pct_formatting() %>%
      adorn_ns() %>%
      untabyl(),
    data.frame(
      x = c(letters[1:4], "Total"),
      `0` = c("82.7% (417)", "0.4%   (2)", "0.2%   (1)", "0.0%   (0)", "83.3% (420)"),
      `1` = c("16.5% (83)", "0.0%  (0)", "0.0%  (0)", "0.2%  (1)", "16.7% (84)"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

test_that("front parameter works", {
  expect_equal(
    source_an %>%
      adorn_totals() %>%
      adorn_percentages("all") %>%
      adorn_pct_formatting() %>%
      adorn_ns("front") %>%
      untabyl(),
    data.frame(
      x = c(letters[1:4], "Total"),
      `0` = c("417 (82.7%)", "2  (0.4%)", "1  (0.2%)", "0  (0.0%)", "420 (83.3%)"),
      `1` = c("83 (16.5%)", "0  (0.0%)", "0  (0.0%)", "1  (0.2%)", "84 (16.7%)"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

test_that("bad inputs are caught", {
  expect_error(mtcars %>% adorn_ns(),
    "argument \"ns\" cannot be null; if not calling adorn_ns() on a data.frame of class \"tabyl\", pass your own value for ns",
    fixed = TRUE
  )
  expect_error(mtcars %>% tabyl(am, cyl) %>% adorn_ns("huh"),
    "\"position\" must be one of \"front\" or \"rear\"",
    fixed = TRUE
  )
  expect_error(
    mtcars %>% tabyl(am, cyl) %>% adorn_ns(ns = mtcars$mpg),
    "if supplying a value to the ns argument, it must be of class data.frame"
  )
  reg_df <- mtcars %>% tabyl(am, cyl)
  wide_df <- mtcars %>% tabyl(am, cyl)
  wide_df$extra <- c(10, 20)
  expect_error(
    adorn_ns(reg_df, ns = wide_df),
    "if supplying your own data.frame of Ns to append, its dimensions must match those of the data.frame in the \"dat\" argument"
  )
  expect_warning(mtcars %>% tabyl(cyl) %>% adorn_ns(),
    "adorn_ns() is meant to be called on a two_way tabyl; consider combining columns of a one_way tabyl with tidyr::unite()",
    fixed = TRUE
  )
})

test_that("attributes make it through unaltered", {
  expect_equal(
    attributes(
      source_an %>%
        adorn_totals() %>%
        adorn_percentages("all") %>%
        adorn_pct_formatting() %>%
        adorn_ns("front") # with adorn_ns
    ),
    attributes(
      source_an %>%
        adorn_totals() %>%
        adorn_percentages("all") %>%
        adorn_pct_formatting() # without adorn_ns
    )
  )
})

test_that("works on smallest tabyls", {
  expect_equal(
    mtcars %>%
      dplyr::slice(1) %>%
      tabyl(am, cyl) %>%
      dplyr::rename(new_var_name = `6`) %>%
      adorn_percentages() %>%
      adorn_pct_formatting() %>%
      adorn_ns() %>%
      untabyl(),
    data.frame(
      am = 1,
      new_var_name = "100.0% (1)", stringsAsFactors = FALSE
    )
  )
})


test_that("users can supply own Ns", {
  # make tabyl with thousands, convert to Ks to append
  big_tabyl <- data.frame(
    a = rep(c("x", rep("y", 9)), 999),
    b = rep(c("big", "big", "big", "small", "small"), 1998),
    stringsAsFactors = FALSE
  ) %>%
    tabyl(a, b)

  custom_Ns <- big_tabyl %>%
    dplyr::mutate(
      big = paste0(round(big / 1000, 1), "k"),
      small = paste0(round(small / 1000, 1), "k")
    )

  expect_equal(
    big_tabyl %>%
      adorn_percentages("col") %>%
      adorn_pct_formatting() %>%
      adorn_ns(ns = custom_Ns) %>%
      untabyl(),
    data.frame(
      a = c("x", "y"),
      big = c("16.7% (1k)", "83.3% (5k)"),
      small = c("0.0% (0k)", "100.0% (4k)"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear) %>%
    adorn_percentages() %>%
    adorn_pct_formatting()
  expect_equal(
    adorn_ns(three), # vanilla call
    purrr::map(three, adorn_ns)
  )

  # with arguments passing through
  expect_equal(
    adorn_ns(three, "front"),
    purrr::map(three, adorn_ns, "front")
  )
})

test_that("non-data.frame inputs are handled", {
  expect_error(adorn_ns(1:5), "adorn_ns() must be called on a data.frame or list of data.frames", fixed = TRUE)
})

test_that("multiple character columns in a tabyl are left untouched",{
  small_with_char <- data.frame(
    x = letters[1:2],
    a = 1:2,
    b = 3:4,
    text = "text",
    stringsAsFactors = FALSE
  )
 expect_equal(
   small_with_char %>%
     adorn_percentages() %>%
     dplyr::pull(text),
   c("text", "text")
  )
})

test_that("works with tidyselect", {
  simple_percs <- source_an %>% adorn_percentages()
  one_adorned <- simple_percs %>% adorn_ns(,,,`1`)
  expect_equal(
    simple_percs[, 1:2],
    one_adorned[, 1:2]
  )
  expect_equal(
    one_adorned[[3]],
    c("0.166 (83)", "0.000  (0)", "0.000  (0)", "1.000  (1)")
  )
})

test_that("no message thrown on grouped df input", {
  expect_silent(source_an %>%
                  adorn_percentages() %>%
                  adorn_ns())
})

test_that("adorn_ns works on single column data.frame with custom Ns if tidyselect is used, #456", {
  adorned_single <- mtcars %>%
    tabyl(am, cyl) %>%
    adorn_percentages()
  adorned_single <-
    adorned_single %>%
    dplyr::select(a = `4`) %>%
    adorn_ns(ns = dplyr::select(attr(adorned_single, "core"), a = `4`),,,, a)
  expect_equal(stringr::str_sub(adorned_single$a, -4, -1), c(" (3)", " (8)"))
})

# This tests the display of the decimal.mark by forcing a decimal into a tabyl
# Can't happen with a natural table, but maybe someone will use adorn_ns on a homespun data.frame
test_that("formatting function works, #444", {
  set.seed(1)
  bigger_dat <- data.frame(sex = rep(c("m", "f"), 3000),
                           age = round(runif(3000, 1, 102), 0))
  bigger_dat$age_group = cut(bigger_dat$age, quantile(bigger_dat$age, c(0, 1/3, 2/3, 1)))
  
  bigger_tab <- bigger_dat %>%
    tabyl(age_group, sex, show_missing_levels = F)
  
  standard_output <- bigger_tab %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 1) %>% 
    adorn_ns(position = "front")
  
  # test commas in thousands place by default
  expect_equal(standard_output$f,
               c("1,018 (33.9%)", "990 (33.0%)", "980 (32.7%)", "12  (0.4%)")
               )
  
  # Test decimal mark
  bigger_tab$f[1] <- 1018.5 # makes no sense in a tabyl but need for testing decimal mark display
  
  bigger_result <- bigger_tab %>%
    untabyl() %>% # to get the decimal into the core
    as_tabyl() %>% 
    adorn_totals(c("row", "col")) %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 1) %>% 
    adorn_ns(position = "rear", format_func = function(x) format(x, big.mark = ".", decimal.mark = ","))
  
  expect_equal(
    bigger_result$f,
    c("33.9% (1.018,5)", "33.0%   (990,0)", "32.7%   (980,0)", "0.4%    (12,0)", 
      "100.0% (3.000,5)")
  )
})