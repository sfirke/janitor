test_that("assert_count", {
  expect_equal(
    assert_count(TRUE, 1),
    TRUE
  )
  expect_equal(
    assert_count(rep(TRUE, 3), 3),
    rep(TRUE, 3)
  )
  my_vector <- c(rep(TRUE, 3), FALSE)
  expect_equal(
    assert_count(my_vector, 3),
    my_vector
  )
  expect_error(
    assert_count(NA),
    regexp = "NA has NA values"
  )
  # more informative errors
  my_vector <- c(NA, TRUE)
  expect_error(
    assert_count(my_vector),
    regexp = "my_vector has NA values"
  )
  my_vector <- c(FALSE, TRUE)
  expect_error(
    assert_count(my_vector, n = 2),
    regexp = "`my_vector` expected 2 `TRUE` values but 1 was found."
  )
  # Check grammar of error message
  my_vector <- c(TRUE, TRUE)
  expect_error(
    assert_count(my_vector, n = 1),
    regexp = "`my_vector` expected 1 `TRUE` value but 2 were found."
  )
})
