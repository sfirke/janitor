test_that("signif_half_up results are as expected" , {
  # scalars
  expect_equal(
    signif_half_up(x = 12.5, digits = 2),
    13
  )
  expect_equal(
    signif_half_up(x = 0),
    0
  )
  expect_equal(
    signif_half_up(x = -2.5, digits = 1),
    -3
  )
  expect_equal(
    signif_half_up(x = 123.45, digits = 4),
    123.5
  )
  expect_equal(
    signif_half_up(x = -123.45, digits = 4),
    -123.5
  )
  # vectors
  expect_equal(
    signif_half_up(x = c(12.5, 0, -2.5, 123.45, -123.45), digits = 2),
    c(13, 0, -2.5, 120, -120)
  )
  expect_equal(
    signif_half_up(x = c(1, 1.5, 1.49, NA, NaN, -Inf, Inf), digits = 2),
    c(1, 1.5, 1.5, NA, NaN, -Inf, Inf)
  )
})
