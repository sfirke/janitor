# Tests for two-way statistical tests

# Duplicate mtcars rows to avoid chis.test warnings
mtcars3 <- rbind(mtcars, mtcars, mtcars)
tab <- table(mtcars3$am, mtcars3$cyl)
ttab <- tabyl(mtcars3, am, cyl)
ow_tab <- tabyl(mtcars3, am)

test_that("one-way tabyl is rejected by chisq.test and fisher.test", {
  expect_error(chisq.test(ow_tab))
  expect_error(fisher.test(ow_tab))
})

test_that("janitor::chisq.test on a table is correct", {
  res <- stats::chisq.test(tab)
  jres <- janitor::chisq.test(tab)
  expect_equal(jres, res)
})

test_that("janitor::chisq.test on a matrix is correct", {
  mat <- matrix(c(151, 434, 345, 221, 145, 167), ncol=3)
  res <- stats::chisq.test(mat)
  jres <- janitor::chisq.test(mat)
  expect_equal(jres, res)
})

test_that("janitor::chisq.test on two factors is correct", {
  res <- stats::chisq.test(mtcars3$am, mtcars3$cyl)
  jres <- janitor::chisq.test(mtcars3$am, mtcars3$cyl)
  expect_equal(jres, res)
})

test_that("janitor::chisq.test with a numeric vector and p is correct", {
  v1 <- round(runif(10, 200, 1000))
  v2 <- round(runif(10, 200, 1000))
  res <- stats::chisq.test(v1, p = v2/sum(v2))
  jres <- janitor::chisq.test(v1, p = v2/sum(v2))
  expect_equal(jres, res)
})

test_that("janitor::fisher.test on a table is correct", {
  res <- stats::fisher.test(tab)
  jres <- janitor::fisher.test(tab)
  expect_equal(jres, res)
})

test_that("janitor::fisher.test on a matrix is correct", {
  mat <- matrix(c(151, 434, 345, 221, 145, 167), ncol=3)
  res <- stats::fisher.test(mat)
  jres <- janitor::fisher.test(mat)
  expect_equal(jres, res)
})

test_that("janitor::fisher.test on two vectors is correct", {
  res <- stats::fisher.test(mtcars3$am, mtcars3$cyl)
  jres <- janitor::fisher.test(mtcars3$am, mtcars3$cyl)
  expect_equal(jres, res)
})

test_that("janitor::chisq.test on a two-way tabyl is identical to stats::chisq.test", {
  tab <- tabyl(mtcars3, am, cyl)
  tres <- chisq.test(tab, tabyl_results = FALSE)
  tab <- table(mtcars3$am, mtcars3$cyl)
  res <- chisq.test(tab)
  expect_equal(tres, res)
})

test_that("janitor::fisher.test on a two-way tabyl is identical to stats::fisher.test", {
  tab <- tabyl(mtcars3, am, cyl)
  tres <- fisher.test(tab)
  tab <- table(mtcars3$am, mtcars3$cyl)
  res <- fisher.test(tab)
  expect_equal(tres, res)
})

test_that("janitor::chisq.test returns tabyl tables", {
  tres <- chisq.test(ttab, tabyl_results = TRUE)
  expect_s3_class(tres$observed, "tabyl")
  expect_s3_class(tres$expected, "tabyl")
  expect_s3_class(tres$residuals, "tabyl")
  expect_s3_class(tres$stdres, "tabyl")
})

test_that("returned tabyls have correct names and attributes", {
  tres <- chisq.test(ttab, tabyl_results = TRUE)
  expect_named(tres$observed, c("am", "4", "6", "8"))
  expect_named(tres$expected, c("am", "4", "6", "8"))
  expect_named(tres$residuals, c("am", "4", "6", "8"))
  expect_named(tres$stdres, c("am", "4", "6", "8"))
  expect_equal(tres$observed[[1]], c("0", "1"))
  expect_equal(tres$expected[[1]], c("0", "1"))
  expect_equal(tres$residuals[[1]], c("0", "1"))
  expect_equal(tres$stdres[[1]], c("0", "1"))
  expect_equal(attr(tres$observed, "var_names"), list(row = "am", col = "cyl"))
  expect_equal(attr(tres$expected, "var_names"), list(row = "am", col = "cyl"))
  expect_equal(attr(tres$residuals, "var_names"), list(row = "am", col = "cyl"))
  expect_equal(attr(tres$stdres, "var_names"), list(row = "am", col = "cyl"))
})

test_that("totals are excluded from the statistical tests, #385", {
  # Chi-Square
  cx <- chisq.test(ttab)
  cx_totals <- suppressWarnings(chisq.test(adorn_totals(ttab, "both")))
  cx_totals$data.name <- "ttab" # otherwise the test shows a mismatch, as the inputs had different names
  expect_equal(
    cx,
    cx_totals
  )
  expect_warning(chisq.test(ttab %>% adorn_totals()),
                 "detected a totals row")

  # Fisher
  fisher <- fisher.test(ttab)
  fisher_totals <- suppressWarnings(fisher.test(adorn_totals(ttab, "both")))
  fisher_totals$data.name <- "ttab" # otherwise the test shows a mismatch, as the inputs had different names
  expect_equal(
    fisher,
    fisher_totals
  )
  expect_warning(fisher.test(ttab %>% adorn_totals()),
                 "detected a totals row")
})
