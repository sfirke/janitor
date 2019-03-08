# Tests for removing fully-NA rows or columns

library(janitor)
context("remove empty rows or columns")

dat <- data.frame(
  a = c(NA, NA, 1),
  b = c(NA, 1, NA),
  c = c(NA, NA, NA)
)

test_that("empty rows are removed", {
  expect_equal(remove_empty(dat, "rows"), dat[2:3, ])
})

test_that("empty cols are removed", {
  expect_equal(remove_empty(dat, "cols"), dat[, 1:2])
})

test_that("bad argument to which throws error", {
  expect_error(mtcars %>%
    remove_empty("blargh"),
  paste0('"which" must be one of "rows", "cols", or c("rows", "cols")'),
  fixed = TRUE
  )
})

test_that("missing argument to which defaults to both, printing a message", {
  expect_message(dat %>%
    remove_empty(),
    "value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")",
  fixed = TRUE
  )
  expect_equal(dat %>% remove_empty,
               dat %>% remove_empty(c("rows", "cols")))
})

test_that("missing data.frame input throws its error before messages about 'which' arg", {
  expect_error(remove_empty(),
               "argument \"dat\" is missing, with no default",
               fixed = TRUE)
})

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

test_that("remove_empty leaves matrices as matrices", {
  mat <- matrix(c(NA, NA, NA, rep(0, 3)), ncol = 2, byrow = TRUE)
  expect_equal(remove_empty(mat), matrix(c(NA, rep(0, 3)), ncol=2),
               info="remove_empty with a matrix returns a matrix")
})

test_that("remove_empty leaves single-column results as the original class", {
  mat <- matrix(c(NA, NA, NA, 0), ncol = 2, byrow = FALSE)
  expect_equal(remove_empty(mat),
               matrix(0, ncol=1),
               info="remove_empty with a matrix that should return a single row and column still returns a matrix")
  df <- data.frame(A=NA, B=c(NA, 0))
  expect_equal(remove_empty(df),
               data.frame(B=0, row.names=2L),
               info="remove_empty with a data.frame that should return a single row and column still returns a data.frame")
})

test_that("remove_empty single-column input results as the original class", {
  mat <- matrix(c(NA, NA, NA, 0), ncol = 1, byrow = FALSE)
  expect_equal(remove_empty(mat),
               matrix(0, ncol=1),
               info="remove_empty with a matrix that should return a single row and column still returns a matrix")
  df <- data.frame(B=c(NA, 0))
  expect_equal(remove_empty(df),
               data.frame(B=0, row.names=2L),
               info="remove_empty with a data.frame that should return a single row and column still returns a data.frame")
})

test_that("remove_constant", {
  expect_equal(
    remove_constant(data.frame(A=1:2, B=1:2)),
    data.frame(A=1:2, B=1:2),
    info="Everything kept."
  )
  expect_equal(
    remove_constant(data.frame(A=c(1, 1), B=c(2, 2))),
    data.frame(A=1:2)[,-1],
    info="All rows are kept, all columns are removed."
  )
  expect_equal(
    remove_constant(data.frame(A=c(1, 1), B=c(2, 3))),
    data.frame(B=2:3),
    info="One column kept (not accidentally turned into a vector)"
  )
  expect_equal(
    remove_constant(data.frame(A=NA, B=1:2)),
    data.frame(B=1:2),
    info="NA is dropped"
  )
  expect_equal(
    remove_constant(data.frame(A=NA, B=c(NA, 1), C=c(1, NA), D=c(1, 1))),
    data.frame(B=c(NA, 1), C=c(1, NA)),
    info="NA with other values is kept"
  )
  expect_equal(
    remove_constant(data.frame(A=NA, B=c(NA, 1, 2), C=c(1, 2, NA), D=c(1, 1, 1), E=c(1, NA, NA), F=c(NA, 1, 1), G=c(1, NA, 1)), na.rm=FALSE),
    data.frame(B=c(NA, 1, 2), C=c(1, 2, NA), E=c(1, NA, NA), F=c(NA, 1, 1), G=c(1, NA, 1)),
    info="NA with other values is kept without na.rm"
  )
  expect_equal(
    remove_constant(data.frame(A=NA, B=c(NA, 1, 2), C=c(1, 2, NA), D=c(1, 1, 1), E=c(1, NA, NA), F=c(NA, 1, 1), G=c(1, NA, 1)), na.rm=TRUE),
    data.frame(B=c(NA, 1, 2), C=c(1, 2, NA)),
    info="NA with other values is kept with na.rm"
  )
})
