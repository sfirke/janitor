test_that("sas_numeric_to_date", {
  expect_equal(
    sas_numeric_to_date(date_num=15639),
    as.Date("2002-10-26")
  )
  expect_equal(
    sas_numeric_to_date(datetime_num=1217083532, tz="UTC"),
    as.POSIXct("1998-07-26 14:45:32", tz="UTC")
  )
  expect_equal(
    sas_numeric_to_date(date_num=15639, time_num=3600, tz="UTC"),
    as.POSIXct("2002-10-26 01:00:00", tz="UTC")
  )
  expect_equal(
    sas_numeric_to_date(time_num=3600),
    hms::hms(3600)
  )
  # NA management
  expect_equal(
    sas_numeric_to_date(date_num = c(NA, 1), time_num = c(NA, 1), tz = "UTC"),
    as.POSIXct(c(NA, "1960-01-02 00:00:01"), tz = "UTC")
  )
  expect_equal(
    sas_numeric_to_date(date_num = NA, time_num = NA, tz = "UTC"),
    as.POSIXct(NA, tz = "UTC")
  )
  # Timezone warning (#583)
  expect_warning(
    sas_numeric_to_date(date_num = 1, time_num = 1, tz = "America/New_York"),
    regexp = "SAS may not properly store timezones other than UTC. Consider confirming the accuracy of the resulting data.",
    fixed = TRUE
  )
})

test_that("sas_numeric_to_date expected errors", {
  expect_error(
    sas_numeric_to_date(date_num=15639, datetime_num=1),
    regexp="Must not give both `date_num` and `datetime_num`"
  )
  expect_error(
    sas_numeric_to_date(datetime_num=1, time_num=1),
    regexp="Must not give both `time_num` and `datetime_num`"
  )
  expect_error(
    sas_numeric_to_date(time_num=-1),
    regexp="`time_num` must be non-negative"
  )
  expect_error(
    sas_numeric_to_date(time_num=86401),
    regexp="`time_num` must be within the number of seconds in a day (<= 86400)",
    fixed=TRUE
  )
  expect_error(
    sas_numeric_to_date(),
    regexp="Must give one of `date_num`, `datetime_num`, `time_num`, or `date_num` and `time_num`"
  )
  expect_error(
    sas_numeric_to_date(date_num=c(NA, 1), time_num=c(1, NA)),
    regexp="The same values are not NA for both `date_num` and `time_num`"
  )
})
