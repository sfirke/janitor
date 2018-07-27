# Tests for Excel date cleaning function

library(janitor)
context("Excel date cleaning")

test_that("Serial number dates convert correctly", {
  expect_equal(excel_numeric_to_date(42370), as.Date("2016-01-01"))
  expect_equal(excel_numeric_to_date(42370, "modern"), as.Date("2016-01-01"))
  expect_equal(excel_numeric_to_date(40908, "mac pre-2011"), as.Date("2016-01-01"))
})

test_that("Bad inputs handled appropriately", {
  expect_error(excel_numeric_to_date("hello"), "argument `date_num` must be of class numeric")
  expect_error(excel_numeric_to_date(40908, "bad string"), "argument 'created' must be one of 'mac pre-2011' or 'modern'")
  expect_error(excel_numeric_to_date(40908, 4), "argument 'created' must be one of 'mac pre-2011' or 'modern'")
})

test_that("time handling works correctly", {
  expect_equal(excel_numeric_to_date(42370, include_time=TRUE),
               as.POSIXlt("2016-01-01"),
               info="Time inclusion works with an integer date")
  expect_equal(excel_numeric_to_date(42370.521, include_time=TRUE),
               as.POSIXlt("2016-01-01 12:30:14"),
               info="Time inclusion works with a fractional date/time and seconds rounded")
  expect_equal(excel_numeric_to_date(42370.521, include_time=TRUE, round_seconds=FALSE),
               as.POSIXlt("2016-01-01 12:30:14.4"),
               info="Time inclusion works with a fractional date/time and seconds not rounded")
  expect_equal(excel_numeric_to_date(42370.521, include_time=FALSE),
               as.Date("2016-01-01"),
               info="Time inclusion works with a fractional date/time and only the date kept.")
})

test_that("time handling at the edge of a minute works correctly", {
  expect_equal(excel_numeric_to_date(42001.1, include_time = TRUE),
               as.POSIXlt("2014-12-28 02:24:00"),
               info = "60 seconds gets converted to 0 seconds, +1 minute")
})

test_that("time handling at the edge of the next date works correctly", {
  expect_warning(excel_numeric_to_date(42002 - 0.0005/86400, include_time=TRUE, round_seconds=FALSE),
                 regexp="1 date_num values are within 0.001 sec of a later date and were rounded up to the next day.")
  expect_equal(excel_numeric_to_date(42002 - 0.0005/86400, include_time=TRUE, round_seconds=FALSE),
               as.POSIXlt("2014-12-29"))
  expect_equal(excel_numeric_to_date(42002 - 0.0005/86400, include_time=TRUE, round_seconds=TRUE),
               as.POSIXlt("2014-12-29"))
  expect_equal(excel_numeric_to_date(42002 - 0.0011/86400, include_time=TRUE, round_seconds=FALSE),
               as.POSIXlt("2014-12-28 23:59:59.998"))
  expect_equal(excel_numeric_to_date(42002 - 0.0011/86400, include_time=TRUE, round_seconds=TRUE),
               as.POSIXlt("2014-12-29"))
})

test_that("excel_numeric_to_date handles NA", {
  expect_equal(excel_numeric_to_date(NA),
               as.Date(NA_character_),
               info="Return NA output of the correct class (Date) for NA input.")
  expect_equal(excel_numeric_to_date(NA, include_time=TRUE),
               as.POSIXlt(NA_character_),
               info="Return NA output of the correct class (POSIXlt) for NA input.")
  expect_equal(excel_numeric_to_date(c(43088, NA)),
               as.Date(floor(c(43088, NA)), origin = "1899-12-30"),
               info="Return NA output as part of a vector of inputs correctly")
  expect_equal(excel_numeric_to_date(c(43088, NA), include_time=TRUE),
               structure(as.POSIXlt(as.Date(floor(c(43088, NA)), origin = "1899-12-30")),
                         tzone=NULL),
               info="Return NA output as part of a vector of inputs correctly")
})
