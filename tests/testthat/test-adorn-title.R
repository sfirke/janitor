source1 <- mtcars %>%
  tabyl(gear, cyl)

test_that("placement is correct", {
  # Top
  expect_equal(
    source1 %>%
      adorn_title() %>%
      names(),
    c("", "cyl", rep("", 2))
  )
  expect_equal(
    source1 %>%
      adorn_title() %>%
      .[1, ] %>%
      unlist() %>%
      unname(),
    c("gear", "4", "6", "8")
  )
  # Combined
  expect_equal(
    source1 %>%
      adorn_title("combined") %>%
      names(),
    c("gear/cyl", "4", "6", "8")
  )
})

test_that("name overrides work", {
  expect_equal(
    source1 %>%
      adorn_title(row_name = "R", col_name = "C") %>%
      names(),
    c("", "C", rep("", 2))
  )
})

test_that("non-tabyls are treated correctly", {
  non_tab <- mtcars %>% dplyr::count(gear, cyl) %>% tidyr::spread(gear, n)
  expect_error(adorn_title(non_tab), "When input is not a data.frame of class tabyl, a value must be specified for the col_name argument")

  expect_equal(
    non_tab %>% adorn_title(col_name = "col") %>% names(),
    c("", "col", rep("", 2))
  )

  expect_equal(
    non_tab %>% adorn_title(placement = "combined", col_name = "col") %>% names(),
    c("cyl/col", 3, 4, 5)
  )

  expect_equal(
    non_tab %>% adorn_title(placement = "combined", row_name = "row!", col_name = "col") %>% names(),
    c("row!/col", 3, 4, 5)
  )
})
test_that("bad inputs are caught", {
  expect_error(adorn_title(1:2),
    "\"dat\" must be a data.frame",
    fixed = TRUE
  )
  expect_error(adorn_title(source1,
    placement = "blargh"
  ),
  "\"placement\" must be one of \"top\" or \"combined\"",
  fixed = TRUE
  )
  expect_error(
    adorn_title(source1,
      row_name = 1:4
    ),
    "row_name must be a string"
  )
  expect_error(
    adorn_title(source1,
      col_name = mtcars
    ),
    "col_name must be a string"
  )

  # Doesn't make sense with a one-way tabyl
  expect_warning(
    mtcars %>% tabyl(cyl) %>% adorn_title(),
    "adorn_title is meant for two-way tabyls, calling it on a one-way tabyl may not yield a meaningful result"
  )
})

test_that("works with non-count inputs", {
  source2_base <- data.frame(sector = c("North", "South"), units = 1:2, group = c("a", "b"))
  source2_tibble <- dplyr::as_tibble(source2_base)
  expect_equal(
    adorn_title(source2_base, col_name = "Characteristics") %>% names(),
    c("", "Characteristics", "")
  )
  expect_equal(
    adorn_title(source2_base, col_name = "Characteristics"),
    adorn_title(source2_tibble, col_name = "Characteristics")
  )

})

test_that("for printing purposes: tabyl class stays tabyl, data.frame stays data.frame, tibble is downgraded to data.frame", {
  # right output classes with tabyl inputs
  expect_equal(class(mtcars %>% tabyl(cyl, am) %>% adorn_title()), c("tabyl", "data.frame"))
  expect_equal(class(mtcars %>% tabyl(gear, carb) %>% adorn_title(., "combined")), c("tabyl", "data.frame"))

  # Create tibble input:
  mpg_by_cyl_and_am <-
    mtcars %>%
    dplyr::group_by(cyl, am) %>%
    dplyr::summarise(mean_mpg = mean(mpg)) %>%
    tidyr::spread(am, mean_mpg)

  # handles tibble input
  expect_s3_class(
    mpg_by_cyl_and_am %>% adorn_title("top", "Cylinders", "Automatic?"),
    "data.frame"
  )

  # Convert columns 2:n to strings
  expect_s3_class(
    mpg_by_cyl_and_am %>% adorn_pct_formatting() %>% # nonsense command here, just want to convert cols 2:n into character
      adorn_title("top", "Cylinders", "Automatic?"),
    "data.frame"
  )

  # handles data.frame non-tabyl input
  expect_s3_class(
    mtcars %>% adorn_title("top", col_name = "hey look ma I'm a title"),
    "data.frame"
  )
})

test_that("works with factors in input", {
  facts <- data.frame(a = "high", large = "1", stringsAsFactors = TRUE)
  # first with "top" then "combined"
  expect_equal(
    facts %>% adorn_title(col_name = "col"),
    data.frame(a = c("a", "high"), col = c("large", "1"), stringsAsFactors = FALSE) %>%
      setNames(., c("", "col"))
  )
  # with combined the original column types are preserved
  expect_equal(
    facts %>% adorn_title("combined", col_name = "col"),
    data.frame(`a/col` = "high", large = "1", stringsAsFactors = TRUE, check.names = FALSE)
  )
})

test_that("automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear) %>%
    adorn_percentages() %>%
    adorn_pct_formatting()
  expect_equal(
    adorn_title(three), # vanilla call
    purrr::map(three, adorn_title)
  )

  # with arguments passing through, incl. custom row and col names
  expect_equal(
    adorn_title(three, "combined", "cyl", "am"),
    purrr::map(three, adorn_title, "combined", "cyl", "am")
  )
})
