source1 <- mtcars %>%
  tabyl(cyl, am) %>%
  adorn_percentages()

test_that("calculations are accurate", {
  expect_equal(
    untabyl(adorn_pct_formatting(source1)), # default parameter is denom = "row"
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c("27.3%", "57.1%", "85.7%"),
      `1` = c("72.7%", "42.9%", "14.3%"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

test_that("data.frames with no numeric columns beyond the first cause failure", {
  expect_error(
    adorn_pct_formatting(data.frame(a = 1:2, b = c("hi", "lo"))),
    "at least one targeted column must be of class numeric",
    fixed = TRUE
  )
})

dat <- data.frame(Operation = c("Login", "Posted", "Deleted"), `Total Count` = c(5, 25, 40), check.names = FALSE)

test_that("works with a single numeric column per #89", {
  expect_equal(
    dat %>% adorn_percentages("col") %>% untabyl(),
    data.frame(
      Operation = c("Login", "Posted", "Deleted"),
      `Total Count` = c(5 / 70, 25 / 70, 40 / 70),
      check.names = FALSE
    )
  )
})

test_that("works with totals row", {
  expect_equal(
    dat %>% adorn_totals("row") %>% adorn_percentages("col") %>% untabyl(),
    data.frame(
      Operation = c("Login", "Posted", "Deleted", "Total"),
      `Total Count` = c(5 / 70, 25 / 70, 40 / 70, 1),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
})

test_that("works with one-way tabyl", {
  expect_equal(
    mtcars %>%
      tabyl(carb) %>%
      adorn_pct_formatting(digits = 0) %>%
      untabyl(),
    data.frame(
      carb = c(1:4, 6, 8),
      n = c(7, 10, 3, 10, 1, 1),
      percent = c("22%", "31%", "9%", "31%", "3%", "3%"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("NAs are replaced with dashes when percentage signs are affixed", {
  # NaNs from adorn_percentages, the more common case (still uncommon)
  has_nans <- mtcars %>%
    tabyl(carb, cyl) %>%
    .[5:6, ] %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting() %>%
    untabyl()
  row.names(has_nans) <- NULL
  expect_equal(
    has_nans,
    data.frame(
      carb = c(6, 8),
      `4` = c("-", "-"),
      `6` = c("100.0%", "0.0%"),
      `8` = c("0.0%", "100.0%"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )

  # NAs convert to -
  has_nas <- data.frame(a = c("big", "little"), x = c(0.1, 0.123), y = c(0.98, NA), stringsAsFactors = FALSE)
  expect_equal(
    adorn_pct_formatting(has_nas),
    data.frame(a = c("big", "little"), x = c("10.0%", "12.3%"), y = c("98.0%", "-"), stringsAsFactors = FALSE)
  )
})

test_that("NAs are replaced with dashes - no percentage signs affixed", {
  # NaNs from adorn_percentages, the more common case (still uncommon)
  has_nans <- mtcars %>%
    tabyl(carb, cyl) %>%
    .[5:6, ] %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(affix_sign = FALSE) %>%
    untabyl()
  row.names(has_nans) <- NULL
  expect_equal(
    has_nans,
    data.frame(
      carb = c(6, 8),
      `4` = c("-", "-"),
      `6` = c("100.0", "0.0"),
      `8` = c("0.0", "100.0"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )

  # NAs convert to - symbol
  has_nas <- data.frame(a = c("big", "little"), x = c(0.1, 0.123), y = c(0.98, NA), stringsAsFactors = FALSE)
  expect_equal(
    adorn_pct_formatting(has_nas, affix_sign = FALSE),
    data.frame(a = c("big", "little"), x = c("10.0", "12.3"), y = c("98.0", "-"), stringsAsFactors = FALSE)
  )
})


test_that("bad rounding argument caught", {
  expect_error(
    dat %>%
      adorn_percentages() %>%
      adorn_pct_formatting(rounding = "blargh"),
    "'rounding' must be one of 'half to even' or 'half up'",
    fixed = TRUE
  )
})

test_that("automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear)
  expect_equal(
    adorn_pct_formatting(three), # vanilla call
    purrr::map(three, adorn_pct_formatting)
  )

  # with arguments passing through
  expect_equal(
    adorn_pct_formatting(three, 2, "half up", affix_sign = FALSE),
    purrr::map(three, adorn_pct_formatting, 2, "half up", FALSE)
  )
})

test_that("non-data.frame inputs are handled", {
  expect_error(adorn_pct_formatting(1:5), "adorn_pct_formatting() must be called on a data.frame or list of data.frames", fixed = TRUE)
})

test_that("tidyselecting works", {
  target <- data.frame(
    color = c("green", "blue", "red"),
    first_wave = c(1:3),
    second_wave = c(4:6),
    third_wave = c(3, 3, 3),
    size = c("small", "medium", "large"),
    stringsAsFactors = FALSE
  )  %>%
    adorn_percentages()

  two_cols <- target %>%
    adorn_pct_formatting(,,,first_wave:second_wave)
  expect_equal(two_cols$first_wave, c("12.5%", "20.0%", "25.0%"))
  expect_equal(two_cols$third_wave, c(3/8, 3/10, 3/12))

  expect_message(
    target %>%
      adorn_pct_formatting(,,,third_wave:size),
    "At least one non-numeric column was specified and will not be modified."
  )
  # correct behavior occurs when text columns are skipped
  expect_message(
    text_skipped <- target %>%
      adorn_pct_formatting(.,,,,c(first_wave, size)),
    "At least one non-numeric column was specified and will not be modified."
  )

  expect_equal(text_skipped$first_wave, c("12.5%", "20.0%", "25.0%"))
  expect_equal(
    text_skipped %>% dplyr::select(-first_wave),
    target %>% dplyr::select(-first_wave),
    ignore_attr = TRUE
  )
})

test_that("decimal.mark works", {
  locale_decimal_sep <- getOption("OutDec") # not sure if it's necessary to save and restore this,
  # but seems safe for locale-independent testing processes 
  options(OutDec= ",") 
  expect_true(
    all(grepl(",", unlist(adorn_pct_formatting(source1)[2])))
  )
  options(OutDec= locale_decimal_sep)
})
