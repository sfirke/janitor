test_that("single_value", {
  expect_equal(single_value(1), 1)
  expect_equal(single_value(c(1, NA)), 1)
  expect_equal(single_value(c(1, NA, 1)), 1)
  expect_equal(single_value(NA_real_), NA)
  expect_equal(single_value(NA, missing=NA_real_), NA_real_)
  expect_error(
    single_value(1:2),
    regexp="More than one (2) value found (1, 2)",
    fixed=TRUE
  )
  expect_error(
    single_value(1:2, info="multiple"),
    regexp="More than one (2) value found (1, 2): multiple",
    fixed=TRUE
  )
})
