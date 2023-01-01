x <- data.frame(
  a = c(rep("x", 55), rep("y", 45)),
  b = c(rep("x", 50), rep("y", 50)),
  stringsAsFactors = FALSE
)

# Crosstab with decimal values ending in .5
y <- x %>%
  tabyl(a, b) %>%
  adorn_percentages("all")

test_that("rounding parameter works", {
  expect_equal(
    y %>%
      adorn_rounding(digits = 1, rounding = "half up") %>%
      untabyl(),
    data.frame(
      a = c("x", "y"),
      x = c(0.5, 0.0),
      y = c(0.1, 0.5),
      stringsAsFactors = FALSE
    )
  )
  # Test failing on CRAN and only there
  skip_on_cran()
  expect_equal(
    y %>%
      adorn_rounding(digits = 1) %>% # default rounding: "half to even"
      untabyl(),
    data.frame(
      a = c("x", "y"),
      x = c(0.5, 0.0),
      y = c(0.0, 0.4),
      stringsAsFactors = FALSE
    )
  )
})

test_that("digit control succeeds", {
  expect_equal(
    y %>%
      adorn_rounding(digits = 0, rounding = "half up") %>%
      untabyl(),
    data.frame(
      a = c("x", "y"),
      x = c(1, 0),
      y = c(0, 0),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    y %>%
      adorn_rounding(digits = 2, rounding = "half up"), # shouldn't do anything given the input only having 2 decimal places
    y
  )
})

test_that("bad rounding argument caught", {
  expect_error(
    y %>%
      adorn_rounding(rounding = "blargh"),
    "'rounding' must be one of 'half to even' or 'half up'",
    fixed = TRUE
  )
})

test_that("works when called on a 3-way tabyl", {
  triple <- mtcars %>%
    tabyl(am, cyl, vs) %>%
    adorn_percentages("row")

  triple_rounded_manual <- triple
  triple_rounded_manual[[1]] <- adorn_rounding(triple[[1]])
  triple_rounded_manual[[2]] <- adorn_rounding(triple[[2]])

  expect_equal(
    triple %>%
      adorn_rounding(),
    triple_rounded_manual
  )
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
    adorn_rounding(,"half up",first_wave:second_wave)
  expect_equal(two_cols$first_wave, c(.1, .2, .3))
  expect_equal(two_cols$third_wave, c(3/8, 3/10, 3/12))

  expect_message(
    target %>%
      adorn_rounding(,,third_wave:size),
    "At least one non-numeric column was specified and will not be modified."
  )
  expect_message(
    text_skipped <- target %>%
      adorn_rounding(,,c(first_wave, size)),
    "At least one non-numeric column was specified and will not be modified."
  )
  expect_equal(text_skipped$first_wave, c(.1, .2, .2))
  expect_equal(
    text_skipped %>% dplyr::select(-first_wave),
    target %>% dplyr::select(-first_wave),
    ignore_attr = TRUE
  )
})

test_that("non-data.frame inputs are handled", {
  expect_error(adorn_rounding(1:5), "adorn_rounding() must be called on a data.frame or list of data.frames", fixed = TRUE)
})
