source1 <- mtcars %>%
  tabyl(cyl, am)

test_that("bad input to denominator arg is caught", {
  expect_error(mtcars %>%
                 adorn_percentages("blargh"),
               paste0("'denominator' must be one of 'row', 'col', or 'all'"),
               fixed = TRUE
  )
})

test_that("calculations are accurate", {
  expect_equal(
    untabyl(adorn_percentages(source1)), # default parameter is denominator = "row"
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3 / 11, 4 / 7, 12 / 14),
      `1` = c(8 / 11, 3 / 7, 2 / 14),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    untabyl(adorn_percentages(source1, denominator = "col")),
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3 / 19, 4 / 19, 12 / 19),
      `1` = c(8 / 13, 3 / 13, 2 / 13),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    untabyl(adorn_percentages(source1, denominator = "all")),
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3 / 32, 4 / 32, 12 / 32),
      `1` = c(8 / 32, 3 / 32, 2 / 32),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

source2 <- source1 %>%
  adorn_totals(c("row", "col"))
test_that("calculations are correct when totals row/col doesn't match axis of computation", {
  expect_equal(
    untabyl(adorn_percentages(source2, denominator = "row")),
    data.frame(
      cyl = c(4, 6, 8, "Total"),
      `0` = c(3 / 11, 4 / 7, 12 / 14, 19 / 32),
      `1` = c(8 / 11, 3 / 7, 2 / 14, 13 / 32),
      Total = c(1, 1, 1, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

test_that("works with totals row/col when denominator = col or all, #357", {
  col_percs <- source1 %>%
    adorn_totals(where = c("col", "row")) %>%
    adorn_percentages(denominator = "col")
  expect_equal(col_percs$Total, c(11, 7, 14, 32)/32)
  expect_equal(unname(unlist(col_percs[4, ])), c("Total", rep(1, 3)))

  # Same but for denominator = all
  all_percs <- source1 %>%
    adorn_totals(where = c("col", "row")) %>%
    adorn_percentages(denominator = "all")
  expect_equal(all_percs$Total, c(11, 7, 14, 32)/32)
  expect_equal(unname(unlist(all_percs[4, ])), unname(c("Total", colSums(source1)[2:3]/32, 32/32)))

  # Now with no totals row, same two tests as preceding
  col_percs_no_row <- source1 %>%
    adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "col")
  expect_equal(col_percs_no_row$Total, c(11, 7, 14)/32)

  # Same but for denominator = all
  all_percs_no_row <- source1 %>%
    adorn_totals(where = c("col")) %>%
    adorn_percentages(denominator = "all")
  expect_equal(all_percs_no_row$Total, c(11, 7, 14)/32)

  # And try one where we exempt the totals col
  expect_message(
    col_percs_exempted <- source1 %>%
      adorn_totals(where = c("col", "row")) %>%
      adorn_percentages(denominator = "col",,-Total),
    regexp = "At least one non-numeric column was specified.  All non-numeric columns will be removed from percentage calculations."
  )
  expect_equal(col_percs_exempted$Total, c(11, 7, 14, 32))
  expect_equal(unname(unlist(col_percs_exempted[4, ])), c("Total", 1, 1, 32))

  expect_message(
    all_percs_exempted <- source1 %>%
      adorn_totals(where = c("col", "row")) %>%
      adorn_percentages(denominator = "all",,-Total),
    regexp = "At least one non-numeric column was specified.  All non-numeric columns will be removed from percentage calculations."
  )
  expect_equal(all_percs_exempted$Total, c(11, 7, 14, 32))
  expect_equal(unname(unlist(all_percs_exempted[4, ])), unname(c("Total", colSums(source1)[2:3]/32, 32)))
})

source2 <- source1
source2[2, 2] <- NA

test_that("NAs handled correctly with na.rm = TRUE", {
  expect_equal(
    untabyl(adorn_percentages(source2)), # row
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3 / 11, NA, 12 / 14),
      `1` = c(8 / 11, 1, 2 / 14),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    untabyl(adorn_percentages(source2, denominator = "col")),
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3 / 15, NA, 12 / 15),
      `1` = c(8 / 13, 3 / 13, 2 / 13),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

test_that("NAs handled correctly with na.rm = FALSE", {
  expect_equal(
    untabyl(adorn_percentages(source2, na.rm = FALSE)), # row
    data.frame(
      cyl = c(4, 6, 8),
      `0` = c(3 / 11, NA, 12 / 14),
      `1` = c(8 / 11, NA, 2 / 14),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    untabyl(adorn_percentages(source2, denominator = "col", na.rm = FALSE)),
    data.frame(
      cyl = c(4, 6, 8),
      `0` = as.numeric(c(NA, NA, NA)),
      `1` = c(8 / 13, 3 / 13, 2 / 13),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

test_that("data.frames with no numeric columns beyond the first cause failure", {
  expect_error(
    adorn_percentages(data.frame(a = 1:2, b = c("hi", "lo"))),
    "at least one one of columns 2:n must be of class numeric"
  )
})

test_that("works with a single numeric column per #89", {
  dat <- data.frame(Operation = c("Login", "Posted", "Deleted"), `Total Count` = c(5, 25, 40), check.names = FALSE)
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
  dat <- data.frame(Operation = c("Login", "Posted", "Deleted"), `Total Count` = c(5, 25, 40), check.names = FALSE)
  expect_equal(
    dat %>% adorn_totals("row") %>% adorn_percentages("col") %>% untabyl(),
    data.frame(
      Operation = c("Login", "Posted", "Deleted", "Total"),
      `Total Count` = c(5 / 70, 25 / 70, 40 / 70, 1),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
})

test_that("automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear)
  expect_equal(
    adorn_percentages(three), # vanilla call
    purrr::map(three, adorn_percentages)
  )

  # with arguments passing through
  expect_equal(
    adorn_percentages(three, "col", na.rm = FALSE),
    purrr::map(three, adorn_percentages, "col", FALSE)
  )
})

test_that("non-data.frame inputs are handled", {
  expect_error(adorn_percentages(1:5), "adorn_percentages() must be called on a data.frame or list of data.frames", fixed = TRUE)
})

test_that("tidyselecting works", {
  target <- data.frame(
    color = c("green", "blue", "red"),
    first_wave = c(1:3),
    second_wave = c(4:6),
    third_wave = c(3, 3, 3),
    size = c("small", "medium", "large"),
    stringsAsFactors = FALSE
  )
  two_cols <- target %>%
    adorn_percentages(,,,first_wave:second_wave)
  expect_equal(two_cols$first_wave, c(1/5, 2/7, 3/9))
  expect_equal(two_cols$third_wave, rep(3, 3))

  expect_message(
    target %>%
    adorn_percentages(., "col",,c(first_wave, size)),
    "At least one non-numeric column was specified.  All non-numeric columns will be removed from percentage calculations."
  )
  expect_message(
    text_skipped <- target %>%
      adorn_percentages(., "col",,c(first_wave, size)),
    regexp = "At least one non-numeric column was specified.  All non-numeric columns will be removed from percentage calculations."
  )
  expect_equal(text_skipped$first_wave, target$first_wave/sum(target$first_wave))
  expect_equal(
    text_skipped %>% dplyr::select(-first_wave),
    target %>% dplyr::select(-first_wave),
    ignore_attr = TRUE
  )

  # Check combination of totals and tidyselecting does not modify totals col
  totaled <- target %>%
    adorn_totals("col",,,,second_wave:third_wave) %>%
    adorn_percentages(,,,second_wave:third_wave)
  expect_equal(totaled$Total, 7:9)
})
