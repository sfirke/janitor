# Tests for removing fully-NA rows or columns

library(janitor)
context("remove empty rows or columns")

dat <- data.frame(a = c(NA, NA, 1),
                  b = c(NA, 1, NA),
                  c = c(NA, NA, NA))

test_that("empty rows are removed", {
  expect_equal(remove_empty(dat, "rows"), dat[2:3, ])
})

test_that("empty cols are removed", {
  expect_equal(remove_empty(dat, "cols"), dat[, 1:2])
})

test_that("missing or bad argument to which throws error", {
  expect_error(mtcars %>%
                 remove_empty("blargh"),
               paste0('"which" must be one of "rows", "cols", or c("rows", "cols")'),
               fixed = TRUE)
  expect_error(mtcars %>%
                 remove_empty(),
               paste0('"which" must be one of "rows", "cols", or c("rows", "cols")'),
               fixed = TRUE)
}
)

# Kind of superficial given that remove_empty_* have been refactored to call remove_empty() themselves, but might as well keep until deprecated functions are removed
test_that("deprecated functions remove_empty_cols and remove_empty_rows function as expected", {
  expect_equal(
    dat %>%
      remove_empty("rows"),
    suppressWarnings(dat %>%
                       remove_empty_rows())
  )
  expect_equal(
    dat %>%
      remove_empty("cols"),
    suppressWarnings(dat %>%
                       remove_empty_cols())
  )
})
