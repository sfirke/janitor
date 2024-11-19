test_that("convert_date works", {
  expect_equal(
    convert_to_date("2009-07-06"),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(40000),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(40000.1),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date("40000"),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date("40000.1"),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(factor("40000.1")),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(as.Date("2009-07-06")),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(as.POSIXct("2009-07-06", tz = "UTC")),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(as.POSIXlt("2009-07-06")),
    as.Date("2009-07-06")
  )
  expect_equal(
    convert_to_date(c("2009-07-06", "40000.1", "40000", NA)),
    c(rep(as.Date("2009-07-06"), 3), NA),
    info="Mixed input works, including NA."
  )
})

test_that("convert_datetime works", {
  expect_equal(
    convert_to_datetime("2009-07-06 12:13:14"),
    as.POSIXct("2009-07-06 12:13:14", tz="UTC")
  )
  expect_equal(
    convert_to_datetime("2009-07-06 12:13:14", tz = "Etc/GMT-5"),
    as.POSIXct("2009-07-06 12:13:14", tz = "Etc/GMT-5"),
    info = "The tz argument is respected"
  )
  expect_equal(
    convert_to_datetime(40000),
    as.POSIXct("2009-07-06", tz="UTC")
  )
  expect_equal(
    convert_to_datetime(40000.1),
    as.POSIXct("2009-07-06 02:24", tz="UTC")
  )
  expect_equal(
    convert_to_datetime(40000.1, tz = "Etc/GMT-5"),
    as.POSIXct("2009-07-06 02:24", tz = "Etc/GMT-5")
  )
  expect_equal(
    convert_to_datetime("40000"),
    as.POSIXct("2009-07-06", tz="UTC")
  )
  expect_equal(
    convert_to_datetime("40000.1"),
    as.POSIXct("2009-07-06 02:24", tz="UTC")
  )
  expect_equal(
    convert_to_datetime("40000.1", tz = "Etc/GMT-5"),
    as.POSIXct("2009-07-06 02:24", tz = "Etc/GMT-5")
  )
  expect_equal(
    convert_to_datetime(factor("40000.1")),
    as.POSIXct("2009-07-06 02:24", tz="UTC")
  )
  expect_equal(
    convert_to_datetime(as.Date("2009-07-06")),
    as.POSIXct("2009-07-06", tz="UTC")
  )
  expect_equal(
    convert_to_datetime(as.POSIXct("2009-07-06", tz="UTC")),
    as.POSIXct("2009-07-06", tz="UTC")
  )
  expect_equal(
    convert_to_datetime(as.POSIXlt("2009-07-06", tz="UTC")),
    as.POSIXct("2009-07-06", tz="UTC")
  )
  expect_equal(
    convert_to_datetime(c("2009-07-06", "40000.1", "40000", NA), character_fun=lubridate::ymd_h, truncated=1, tz="UTC"),
    as.POSIXct(c("2009-07-06 00:00", "2009-07-06 02:24", "2009-07-06 00:00", NA), tz="UTC"),
    info="Mixed input works, including NA."
  )
})

test_that("convert_date warnings and errors work", {
  expect_warning(
    expect_error(
      convert_to_date("A"),
      regexp="Not all character strings converted to class Date."
    ),
    regexp="All formats failed to parse." # lubridate warning
  )
  expect_warning(
    expect_error(
      convert_to_date(LETTERS),
      regexp="Not all character strings converted to class Date.*17 other values",
      info="Confirm the 'other values' when there are many values not converted."
    ),
    regexp="All formats failed to parse." # lubridate warning
  )
  expect_warning(
    expect_error(
      convert_to_date(LETTERS),
      regexp="Not all character strings converted to class Date."
    ),
    regexp="All formats failed to parse." # lubridate warning
  )
  expect_warning(
    expect_warning(
      expect_equal(
        convert_to_date("A", string_conversion_failure="warning"),
        as.Date(NA)
      ),
      regexp = "All formats failed to parse. No formats found."
    ),
    regexp="Not all character strings converted to class Date."
  )
  expect_error(
    convert_to_date("A", character_fun=function(x) 1),
    regexp="must return class Date"
  )
  expect_warning(
    convert_to_date("40000", include_time=TRUE),
    regexp="`include_time` is ignored in favor of `out_class`"
  )
})
