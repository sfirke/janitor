context("signif_half_up")

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
})