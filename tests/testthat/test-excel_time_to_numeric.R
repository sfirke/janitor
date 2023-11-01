context("Excel time cleaning")

test_that("excel_time_to_numeric numbers function correctly", {
  expect_equal(excel_time_to_numeric(0.1), 8640)
  expect_equal(excel_time_to_numeric(0.1000000001), 8640)
  expect_equal(excel_time_to_numeric(0), 0)
  expect_equal(excel_time_to_numeric(NA_real_), NA_real_)
})

test_that("excel_time_to_numeric POSIX objects extract the correct part of the time", {
  expect_equal(excel_time_to_numeric(as.POSIXct("1899-12-31 08:00")), 8 * 3600)
  expect_equal(excel_time_to_numeric(as.POSIXct("1899-12-31 13:00")), 13 * 3600)
  expect_equal(excel_time_to_numeric(as.POSIXct("1899-12-31 13:05:10")), 13 * 3600 + 5 * 60 + 10)
})

test_that("excel_time_to_numeric POSIX objects ignore the time zone", {
  expect_equal(excel_time_to_numeric(as.POSIXct("1899-12-31 13:00", tz = "EST")), 13 * 3600)
  expect_equal(excel_time_to_numeric(as.POSIXct("1899-12-31 13:00", tz = "UTC")), 13 * 3600)
  expect_equal(
    excel_time_to_numeric(
      as.POSIXct(c("1899-12-31 13:00", "1899-12-31 13:00"), tz = "EST")
    ),
    rep(13 * 3600, 2)
  )
})

test_that("excel_time_to_numeric POSIXlt works like POSIXct", {
  expect_equal(
    excel_time_to_numeric(as.POSIXct("1899-12-31 13:00", tz = "EST")),
    excel_time_to_numeric(as.POSIXlt("1899-12-31 13:00", tz = "EST"))
  )
})

test_that("excel_time_to_numeric numbers errors when out of range", {
  expect_error(excel_time_to_numeric(1))
  expect_error(excel_time_to_numeric(-0.1))
})

test_that("excel_time_to_numeric logical values return as expected", {
  expect_equal(excel_time_to_numeric(NA), NA_real_)
  expect_error(excel_time_to_numeric(c(NA, TRUE)))
  expect_error(excel_time_to_numeric(TRUE))
})

test_that("excel_time_to_numeric, character strings of numbers work as expected", {
  expect_equal(excel_time_to_numeric("0.5"), 12 * 3600)
  expect_equal(excel_time_to_numeric("0"), 0)
  expect_equal(excel_time_to_numeric("0."), 0)
  expect_equal(excel_time_to_numeric("0.000000"), 0)
  expect_equal(excel_time_to_numeric("0.00001"), 1)
  expect_equal(
    excel_time_to_numeric("0.00001", round_seconds = FALSE),
    0.00001 * 86400
  )
  # Confirm scientific notation values
  expect_equal(
    excel_time_to_numeric("2.9166666666666664E-2", round_seconds = TRUE),
    2520
  )
})

test_that("excel_time_to_numeric, am/pm times work", {
  expect_equal(excel_time_to_numeric("8:00am"), 8 * 3600)
  expect_equal(excel_time_to_numeric("8:00pm"), 20 * 3600)
  expect_equal(excel_time_to_numeric("8:10am"), 8 * 3600 + 10 * 60)
  expect_equal(excel_time_to_numeric("8:10:05am"), 8 * 3600 + 10 * 60 + 5)
  expect_equal(
    excel_time_to_numeric("12:10:05am"), 10 * 60 + 5,
    info = "After midnight is treated as 0 not 12."
  )
  expect_equal(
    excel_time_to_numeric("12:10:05pm"), 12 * 3600 + 10 * 60 + 5,
    info = "After noon is treated as 12."
  )
  # Test mixed AM/PM and 24-hour clock values
  expect_equal(
    excel_time_to_numeric(c("8:00pm", "8:00", "9:00")),
    c(20, 8, 9)*3600
  )
})

test_that("excel_time_to_numeric, am/pm times work case insensitively and with spaces", {
  expect_equal(
    excel_time_to_numeric("8:00am"),
    excel_time_to_numeric("8:00AM")
  )
  expect_equal(
    excel_time_to_numeric("8:00am"),
    excel_time_to_numeric("8:00Am")
  )
  expect_equal(
    excel_time_to_numeric("8:00am"),
    excel_time_to_numeric("8:00aM")
  )
  expect_equal(
    excel_time_to_numeric("8:00am"),
    excel_time_to_numeric("8:00 AM")
  )
})

test_that("excel_time_to_numeric, 24-hour times work (zero-padded hours or not)", {
  expect_equal(excel_time_to_numeric("8:00"), 8 * 3600)
  expect_equal(excel_time_to_numeric("08:00"), 8 * 3600)
  expect_equal(excel_time_to_numeric("08:10"), 8 * 3600 + 10 * 60)
  expect_equal(excel_time_to_numeric("8:10:05"), 8 * 3600 + 10 * 60 + 5)
  expect_equal(excel_time_to_numeric("21:05"), 21 * 3600 + 5 * 60)
  expect_equal(excel_time_to_numeric("21:05:20"), 21 * 3600 + 5 * 60 + 20)
})

test_that("excel_time_to_numeric, 24-hour times work (zero-padded hours or not)", {
  expect_equal(excel_time_to_numeric("8:00"), 8 * 3600)
  expect_equal(excel_time_to_numeric("08:00"), 8 * 3600)
  expect_equal(excel_time_to_numeric("08:10"), 8 * 3600 + 10 * 60)
  expect_equal(excel_time_to_numeric("8:10:05"), 8 * 3600 + 10 * 60 + 5)
  expect_equal(excel_time_to_numeric("21:05"), 21 * 3600 + 5 * 60)
  expect_equal(excel_time_to_numeric("0:05"), 5 * 60)
  expect_equal(excel_time_to_numeric("00:05"), 5 * 60)
  expect_equal(excel_time_to_numeric("21:05:20"), 21 * 3600 + 5 * 60 + 20)
})

test_that("excel_time_to_numeric, POSIX times on 1899-12-31 work", {
  expect_equal(excel_time_to_numeric("1899-12-31 8:00"), 8 * 3600)
  expect_equal(excel_time_to_numeric("1899-12-31 08:00"), 8 * 3600)
  expect_equal(excel_time_to_numeric("1899-12-31 08:10"), 8 * 3600 + 10 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 8:10:05"), 8 * 3600 + 10 * 60 + 5)
  expect_equal(excel_time_to_numeric("1899-12-31 21:05"), 21 * 3600 + 5 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 0:05"), 5 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 00:05"), 5 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 21:05:20"), 21 * 3600 + 5 * 60 + 20)
})

test_that("excel_time_to_numeric, POSIX times ignore extra text (which is hopefully a time zone)", {
  expect_equal(excel_time_to_numeric("1899-12-31 8:00 foo"), 8 * 3600)
  expect_equal(excel_time_to_numeric("1899-12-31 08:00 foo"), 8 * 3600)
  expect_equal(excel_time_to_numeric("1899-12-31 08:10 foo"), 8 * 3600 + 10 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 8:10:05 foo"), 8 * 3600 + 10 * 60 + 5)
  expect_equal(excel_time_to_numeric("1899-12-31 21:05 foo"), 21 * 3600 + 5 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 0:05 foo"), 5 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 00:05 foo"), 5 * 60)
  expect_equal(excel_time_to_numeric("1899-12-31 21:05:20 foo"), 21 * 3600 + 5 * 60 + 20)
})

test_that("excel_time_to_numeric, POSIX times treat no time as midnight but only if there is a space indicating a mostly-well-formed date-time object.", {
  # the just-a-space requirement is there because some time formatting puts the
  # date then a space then the time zone.
  expect_equal(excel_time_to_numeric("1899-12-31 foo"), 0)
  expect_error(excel_time_to_numeric("1899-12-31foo"))
})

test_that("excel_time_to_numeric, invalid character times trigger an error", {
  expect_error(excel_time_to_numeric("1"))
  expect_error(excel_time_to_numeric("-0.1"))
  expect_error(excel_time_to_numeric("0:05:20am"))
  expect_error(excel_time_to_numeric("1:60:20am"))
  expect_error(excel_time_to_numeric("1:00:70am"))
  expect_error(excel_time_to_numeric("13:05:20am"))
  expect_error(excel_time_to_numeric("13:05:20am"))
  expect_error(excel_time_to_numeric("25:05:20"))
  expect_error(excel_time_to_numeric("23:65:20"))
  expect_error(excel_time_to_numeric("23:05:90"))
  expect_error(excel_time_to_numeric("1899-12-30 21:05:20"))
})
