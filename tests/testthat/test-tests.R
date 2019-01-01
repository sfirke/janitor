# Tests for two-way statistical tests

library(janitor)
context("tests")

# Duplicate mtcars rows to avoid chis.test warnings
mtcars3 <- rbind(mtcars, mtcars, mtcars)
tab <- table(mtcars3$am, mtcars3$cyl)
ttab <- tabyl(mtcars3, am, cyl)
ow_tab <- tabyl(mtcars3, am)

test_that("one-way tabyl is rejected", {
  expect_error(chisq.test(ow_tab))
})

test_that("janitor::chisq.test on a table is correct", {
  tab <- table(mtcars3$am, mtcars3$cyl)
  res <- stats::chisq.test(tab)
  jres <- janitor::chisq.test(tab)
  expect_equal(jres, res)
})

test_that("janitor::chisq.test on a two-way tabyl is identical to stats::chisq.test", {
  tab <- tabyl(mtcars3, am, cyl)
  tres <- chisq.test(tab, tabyl_results = FALSE)
  tab <- table(mtcars3$am, mtcars3$cyl)
  res <- chisq.test(tab)
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
