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
  expect_error(excel_numeric_to_date(40908, "bad string"), "argument 'date_system' must be one of 'mac pre-2011' or 'modern'")
  expect_error(excel_numeric_to_date(40908, 4), "argument 'date_system' must be one of 'mac pre-2011' or 'modern'")
})

test_that("time handling works correctly", {
  expect_equal(excel_numeric_to_date(42370, include_time=TRUE),
               as.POSIXct("2016-01-01"),
               info="Time inclusion works with an integer date")
  expect_equal(excel_numeric_to_date(42370.521, include_time=TRUE),
               as.POSIXct("2016-01-01 12:30:14"),
               info="Time inclusion works with a fractional date/time and seconds rounded")
  expect_equal(excel_numeric_to_date(42370.521, include_time=TRUE, round_seconds=FALSE),
               as.POSIXct("2016-01-01 12:30:14.4"),
               info="Time inclusion works with a fractional date/time and seconds not rounded")
  expect_equal(excel_numeric_to_date(42370.521, include_time=FALSE),
               as.Date("2016-01-01"),
               info="Time inclusion works with a fractional date/time and only the date kept.")
})

test_that("time handling at the edge of a minute works correctly", {
  expect_equal(excel_numeric_to_date(42001.1, include_time = TRUE),
               as.POSIXct("2014-12-28 02:24:00"),
               info = "60 seconds gets converted to 0 seconds, +1 minute")
})

test_that("time handling at the edge of the next date works correctly", {
  expect_warning(excel_numeric_to_date(42002 - 0.0005/86400, include_time=TRUE, round_seconds=FALSE),
                 regexp="1 date_num values are within 0.001 sec of a later date and were rounded up to the next day.")
  # suppress warning on next two tests, that aspect is identical to preceding call where warning is checked
  expect_equal(
    suppressWarnings(
      excel_numeric_to_date(42002 - 0.0005/86400, include_time=TRUE, round_seconds=FALSE)
    ),
    as.POSIXct("2014-12-29")
  )
  expect_equal(
    suppressWarnings(
      excel_numeric_to_date(42002 - 0.0005/86400, include_time=TRUE, round_seconds=TRUE)
    ),
    as.POSIXct("2014-12-29")
  )
  expect_equal(excel_numeric_to_date(42002 - 0.0011/86400, include_time=TRUE, round_seconds=FALSE),
               as.POSIXct("2014-12-28 23:59:59.998"))
  expect_equal(excel_numeric_to_date(42002 - 0.0011/86400, include_time=TRUE, round_seconds=TRUE),
               as.POSIXct("2014-12-29"))
})

test_that("excel_numeric_to_date handles NA", {
  expect_equal(
    excel_numeric_to_date(NA),
    as.Date(NA_character_),
    info="Return NA output of the correct class (Date) for NA input."
  )
  expect_equal(
    excel_numeric_to_date(NA, include_time=TRUE),
    as.POSIXct(NA_character_, tz=Sys.timezone()),
    info="Return NA output of the correct class (POSIXct) for NA input."
  )
  expect_equal(
    excel_numeric_to_date(c(43088, NA)),
    as.Date(floor(c(43088, NA)), origin = "1899-12-30"),
    info="Return NA output as part of a vector of inputs correctly"
  )
  expect_equal(
    excel_numeric_to_date(c(43088, NA), include_time=TRUE),
    structure(
      as.POSIXlt(as.Date(floor(c(43088, NA)), origin = "1899-12-30")),
      tzone=NULL
    ),
    info="Return NA output as part of a vector of inputs correctly"
  )
})

test_that("excel_numeric_to_date returns a POSIXct object when include_time is requested", {
  expect_equal(class(excel_numeric_to_date(c(43088, NA), include_time=TRUE)),
               c("POSIXct", "POSIXt"))
})

test_that("time zone setting works", {
  expect_equal(
    attr(excel_numeric_to_date(43001.11, include_time = TRUE), "tzone"),
    Sys.timezone(),
    info="Defaults to the local timezone"
  )
  expect_equal(
    attr(excel_numeric_to_date(43001.11, include_time = TRUE, tz = "America/New_York"), "tzone"),
    "America/New_York"
  )
  expect_equal(
    attr(excel_numeric_to_date(43001.11, include_time = TRUE, tz = "Europe/Zurich"), "tzone"),
    "Europe/Zurich"
  )
  expect_error(
    excel_numeric_to_date(43001.11, include_time = TRUE, tz = "nonsense"),
    info="Invalid timezone gives an error"
  )
})

test_that("integer Excel dates do not overflow (ref issue #241)", {
  expect_equal(excel_numeric_to_date(42370L),
               excel_numeric_to_date(42370))
})

test_that("daylight savings time handling (issue #420)", {
  expect_equal(
    expect_warning(
      excel_numeric_to_date(43170.09, include_time=TRUE, tz="America/New_York"),
      regexp="NAs introduced by coercion, possible daylight savings time issue with input.  Consider `tz='UTC'`.",
      fixed=TRUE
    ),
    as.POSIXct(NA_real_, tz="America/New_York", origin="1900-01-01")
  )
  expect_equal(
    excel_numeric_to_date(43170.09, include_time=TRUE, tz="UTC"),
    as.POSIXct("2018-03-11 02:09:36", tz="UTC")
  )
})

test_that("Nonexistent 29 Feb 1900 exists in Excel but not in accurate calendars", {
  expect_equal(
    expect_warning(
      excel_numeric_to_date(59:61),
      regexp="NAs introduced by coercion, Excel leap day bug detected in `date_num`.  29 February 1900 does not exist.",
      fixed=TRUE
    ),
    as.Date(c("1900-02-28", NA, "1900-03-01"))
  )
})

test_that("Negative dates are invalid in Excel (issue #423)", {
  expect_equal(
    expect_warning(
      excel_numeric_to_date(-1:1),
      regexp="Only `date_num` >= 1 are valid in Excel, creating an earlier date than Excel supports.",
      fixed=TRUE
    ),
    as.Date(c("1899-12-30", "1899-12-31", "1900-01-01"))
  )
})
