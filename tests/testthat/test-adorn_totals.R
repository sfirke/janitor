dat <- data.frame(
  a = c(rep(c("big", "small", "big"), 3)),
  b = c(1:3, 1:3, 1, 1, 1),
  stringsAsFactors = TRUE
)
ct <- dat %>%
  tabyl(a, b)

mixed <- data.frame(
  a = 1:3,
  b = c("x", "y", "z"),
  c = 5:7,
  d = c("big", "med", "small"),
  stringsAsFactors = FALSE
)

test_that("long vectors are trimmed", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "row",
        name = c("total", "row_total"),
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "total"),
      b = c("x", "y", "z", "-"),
      c = c(5:7, 18),
      d = c("big", "med", "small", "-"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("row and column names are taken correctly from a vector", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "both",
        name = c("column_totals", "row_totals"),
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "column_totals"),
      b = c("x", "y", "z", "-"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "-"),
      row_totals = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})

test_that("row and column names are taken correctly from a single name", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "both",
        name = "totals",
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "totals"),
      b = c("x", "y", "z", "-"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "-"),
      totals = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})
