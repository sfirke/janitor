test_that("round_half_up works", {
  expect_equal(round_half_up(-0.5, 0), -1)
  expect_equal(round_half_up(0.5, 0), 1)
  expect_equal(round_half_up(1.125, 2), 1.13)
  expect_equal(round_half_up(1.135, 2), 1.14)
  expect_equal(round_half_up(2436.845, 2), 2436.85)
})
