test_that("round_to_fraction input requirements (confirm errors)", {
  expect_error(
    round_to_fraction(x="A", denominator=1),
    info="x must be numberic"
  )
  expect_error(
    round_to_fraction(x=1, denominator="A"),
    info="denominator must be numberic"
  )
  expect_error(
    round_to_fraction(x=1, denominator=c(1, 2)),
    info="length(denominator) must equal length(x) or be 1"
  )
  expect_error(
    round_to_fraction(x=1, denominator=-1),
    info="denominator must be positive"
  )
  expect_error(
    round_to_fraction(x=1, denominator=0),
    info="denominator must be positive, not zero"
  )
  expect_error(
    round_to_fraction(x=1, denominator=2, digits="A"),
    info="digits must be numeric or 'auto'"
  )
  expect_error(
    round_to_fraction(x=1, denominator=2, digits=c(1, 2)),
    info="digits must be numeric or 'auto'"
  )
  expect_error(
    round_to_fraction(x=c(1, 2), denominator=2, digits=c(1, 2, 3)),
    info="digits must be numeric the same length as x (or scalar)"
  )
})

test_that("round_to_fraction results are as expected", {
  # scalars
  expect_equal(
    round_to_fraction(x=1.1, denominator=7),
    8/7
  )
  expect_equal(
    round_to_fraction(x=1.1, denominator=7, digits=Inf),
    round_to_fraction(x=1.1, denominator=7)
  )
  expect_equal(
    round_to_fraction(x=1.1, denominator=7, digits=5),
    round(8/7, digits=5)
  )
  expect_equal(
    round_to_fraction(x=1.1, denominator=7, digits="auto"),
    round(8/7, digits=2)
  )

  # vectors
  expect_equal(
    round_to_fraction(x=c(1.1, 2.05), denominator=7, digits=Inf),
    c(8/7, 2)
  )
  expect_equal(
    round_to_fraction(x=c(1.1, 2.05), denominator=c(7, 25), digits=Inf),
    c(8/7, 51/25)
  )
  expect_equal(
    round_to_fraction(x=c(1.1, 2.05), denominator=c(7, 27), digits=Inf),
    c(8/7, 55/27)
  )
  expect_equal(
    round_to_fraction(x=c(1.1, 2.05), denominator=c(7, 27), digits=3),
    round(c(8/7, 55/27), digits=3)
  )
  expect_equal(
    round_to_fraction(x=c(1.1, 2.05), denominator=c(7, 27), digits=c(3, 4)),
    round(c(8/7, 55/27), digits=c(3, 4))
  )
  expect_equal(
    round_to_fraction(x=c(1.1, 2.05), denominator=c(7, 27), digits="auto"),
    round(c(8/7, 55/27), digits=c(2, 3))
  )
})
