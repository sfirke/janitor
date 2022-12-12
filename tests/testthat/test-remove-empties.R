# Tests for removing fully-NA rows or columns

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
  expect_message(
    result <-
      dat %>%
      remove_empty(),
    "value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")",
    fixed = TRUE
  )
  expect_equal(
    result,
    dat %>% remove_empty(c("rows", "cols"))
  )
})

test_that("missing data.frame input throws its error before messages about 'which' arg", {
  expect_error(remove_empty(),
               "argument \"dat\" is missing, with no default",
               fixed = TRUE)
})

test_that("remove_empty leaves matrices as matrices", {
  mat <- matrix(c(NA, NA, NA, rep(0, 3)), ncol = 2, byrow = TRUE)
  expect_message(
    expect_equal(
      remove_empty(mat), matrix(c(NA, rep(0, 3)), ncol=2),
      info="remove_empty with a matrix returns a matrix"
    ),
    regexp = 'value for "which" not specified, defaulting to c("rows", "cols")',
    fixed = TRUE
  )
})

test_that("remove_empty leaves single-column results as the original class", {
  mat <- matrix(c(NA, NA, NA, 0), ncol = 2, byrow = FALSE)
  expect_equal(
    remove_empty(mat, which = c("rows", "cols")),
    matrix(0, ncol=1),
    info="remove_empty with a matrix that should return a single row and column still returns a matrix"
  )
  df <- data.frame(A=NA, B=c(NA, 0))
  expect_equal(
    remove_empty(df, which = c("rows", "cols")),
    data.frame(B=0, row.names=2L),
    info="remove_empty with a data.frame that should return a single row and column still returns a data.frame"
  )
})

test_that("remove_empty single-column input results as the original class", {
  mat <- matrix(c(NA, NA, NA, 0), ncol = 1, byrow = FALSE)
  expect_equal(
    remove_empty(mat, which = c("rows", "cols")),
    matrix(0, ncol=1),
    info="remove_empty with a matrix that should return a single row and column still returns a matrix"
  )
  df <- data.frame(B=c(NA, 0))
  expect_equal(
    remove_empty(df, which = c("rows", "cols")),
    data.frame(B=0, row.names=2L),
    info="remove_empty with a data.frame that should return a single row and column still returns a data.frame"
  )
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
  expect_equal(
    remove_constant(tibble::tibble(A=NA, B=c(NA, 1, 2), C=1)),
    tibble::tibble(B=c(NA, 1, 2)),
    info="tibbles are correctly handled"
  )
})

test_that("Messages are accurate with remove_empty and remove_constant", {
  expect_message(
    remove_empty(data.frame(A=NA, B=1), which="cols", quiet=FALSE),
    regexp="Removing 1 empty columns of 2 columns total (Removed: A).",
    fixed=TRUE
  )
  expect_message(
    remove_empty(data.frame(A=NA, B=1, C=NA), which="cols", quiet=FALSE),
    regexp="Removing 2 empty columns of 3 columns total (Removed: A, C).",
    fixed=TRUE
  )
  expect_message(
    remove_empty(data.frame(A=NA, B=c(1, NA)), which="rows", quiet=FALSE),
    regexp="Removing 1 empty rows of 2 rows total (50%).",
    fixed=TRUE
  )
  expect_message(
    remove_empty(matrix(c(NA, NA, 1, NA), nrow=2), which="cols", quiet=FALSE),
    regexp="Removing 1 empty columns of 2 columns total (50%).",
    fixed=TRUE
  )
  expect_message(
    remove_constant(matrix(c(NA, NA, 1, NA), nrow=2), quiet=FALSE),
    regexp="Removing 1 constant columns of 2 columns total (50%).",
    fixed=TRUE,
    info="Unnamed, constant columns"
  )
  expect_silent(
    remove_empty(data.frame(A=NA, B=1), which="cols", quiet=TRUE)
  )
  expect_silent(
    remove_empty(data.frame(A=NA, B=c(1, NA)), which="rows", quiet=TRUE)
  )
  expect_message(
   remove_constant(mtcars, quiet = FALSE),
   regexp="No constant columns to remove.",
   fixed=TRUE,
   info="No constant columns to remove"
  )
  expect_message(expect_message(
    remove_empty(mtcars, quiet = FALSE, which = c("rows", "cols")),
    regexp="No empty columns to remove."),
    regexp = "No empty rows to remove."
  )
})

test_that("remove_empty cutoff tests", {
  dat <-
    data.frame(
      A=rep(NA, 10),
      B=c(1, 1, rep(NA, 8)),
      C=c(rep(1, 8), NA, NA),
      D=c(rep(1, 9), NA),
      E=1
    )
  # Implicit cutoff is 1
  expect_equal(
    remove_empty(dat, which = c("rows", "cols")),
    remove_empty(dat, cutoff=1, which = c("rows", "cols"))
  )
  expect_equal(
    remove_empty(dat, cutoff=1, which="rows"),
    dat
  )
  expect_equal(
    remove_empty(dat, cutoff=0.8, which="rows"),
    dat[c(), ]
  )
  expect_equal(
    remove_empty(dat, cutoff=0.79, which="rows"),
    dat[1:2, ]
  )
  expect_equal(
    remove_empty(dat, cutoff=0.2, which="rows"),
    dat[1:9, ]
  )
  expect_equal(
    remove_empty(dat, cutoff=1, which="cols"),
    dat[, c("B", "C", "D", "E")]
  )
  expect_equal(
    remove_empty(dat, cutoff=0.9, which="cols"),
    dat[, "E", drop=FALSE]
  )
  expect_equal(
    remove_empty(dat, cutoff=0.2, which="cols"),
    dat[, c("C", "D", "E"), drop=FALSE]
  )
})

test_that("remove_empty cutoff errors", {
  expect_error(
    remove_empty(cutoff=c(0.1, 0.2)),
    regexp="cutoff must be a single value"
  )
  expect_error(
    remove_empty(cutoff="A"),
    regexp="cutoff must be numeric"
  )
  expect_error(
    remove_empty(cutoff=0),
    regexp="cutoff must be >0 and <= 1"
  )
  expect_error(
    remove_empty(cutoff=1.1),
    regexp="cutoff must be >0 and <= 1"
  )
  # Implicit `which` argument
  expect_error(
    remove_empty(cutoff=0.9),
    regexp="cutoff must be used with only one of which = 'rows' or 'cols', not both"
  )
  # Explicit `which` argument
  expect_error(
    remove_empty(cutoff=0.9, which=c("rows", "cols")),
    regexp="cutoff must be used with only one of which = 'rows' or 'cols', not both"
  )
})
