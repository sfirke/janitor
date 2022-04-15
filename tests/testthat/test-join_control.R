test_that("join_control works as expected", {
  empty_df <- data.frame(A=1)[-1, , drop=FALSE]
  one_row_df <- data.frame(A=1, B=2)
  two_row_df <- data.frame(A=1:2, C=3)
  
  expect_equal(
    join_control(
      empty_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control="any", y_control="any"
    ),
    dplyr::left_join(empty_df, one_row_df)
  )
  expect_equal(
    join_control(
      one_row_df, two_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control="unique"
    ),
    dplyr::left_join(one_row_df, two_row_df)
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control="unique"
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  expect_error(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("unique", "nomissing")
    ),
    regexp="Rows are missing in the new dataset"
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("all", "unique")
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("any", "nomissing")
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  expect_equal(
    join_control(
      two_row_df, one_row_df,
      join_fun=dplyr::left_join,
      x_control=c("all", "unique"),
      y_control=c("unique", "missing")
    ),
    dplyr::left_join(two_row_df, one_row_df)
  )
  
  # the `by` argument is respected
  x <- data.frame(A=1:4, B=rep(1:2, each=2))
  y <- data.frame(A=1:4, B=5:8)
  expect_error(
    join_one_to_one(x=x, y=y),
    regexp="All rows were are not in the new dataset"
  )
  expect_equal(
    join_one_to_one(x=x, y=y, by="A"),
    data.frame(A=1:4, B.x=rep(1:2, each=2), B.y=5:8)
  )
})

test_that("join_control row counts in output are maintained", {
  one_row_df <- data.frame(A=1, B=2)
  two_row_df <- data.frame(A=1:2, C=3)
  five_row_df <- data.frame(A=1:5, D=3)
  expect_equal(
    join_control(
      x=one_row_df, y=two_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      x_fraction=1, y_fraction=0.5
    ),
    data.frame(A=1, B=2, C=3)
  )
  expect_error(
    join_control(
      x=two_row_df, y=one_row_df,
      join_fun=dplyr::right_join,
      x_control="all", y_control="unique",
      x_fraction=0.75, y_fraction=1
    ),
    regexp="Not enough rows from `x` are in the returned value (2 expected and 1 found)",
    fixed=TRUE
  )
  expect_error(
    join_control(
      x=one_row_df, y=two_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      x_fraction=1, y_fraction=0.75
    ),
    regexp="Not enough rows from `y` are in the returned value (2 expected and 1 found)",
    fixed=TRUE
  )
  expect_equal(
    join_control(
      x=one_row_df, y=two_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      overlap_fraction=1
    ),
    data.frame(A=1, B=2, C=3)
  )
  expect_equal(
    join_control(
      x=five_row_df, y=one_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique"
    ),
    data.frame(A=1:5, D=3, B=c(2, rep(NA, 4)))
  )
  expect_equal(
    join_control(
      x=five_row_df, y=one_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      x_count=5
    ),
    data.frame(A=1:5, D=3, B=c(2, rep(NA, 4))),
    info="Test x_count"
  )
  expect_equal(
    join_control(
      x=five_row_df, y=one_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      y_count=1
    ),
    data.frame(A=1:5, D=3, B=c(2, rep(NA, 4))),
    info="Test y_count (success)"
  )
  expect_error(
    join_control(
      x=five_row_df, y=one_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      y_count=2
    ),
    regexp="For y: 'count' must be between 1 and nrow(data) (1), if not NA.",
    fixed=TRUE,
    info="Test y_count (failure)"
  )
  expect_equal(
    join_control(
      x=five_row_df, y=one_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      overlap_count=1
    ),
    data.frame(A=1:5, D=3, B=c(2, rep(NA, 4))),
    info="Test overlap_count (success)"
  )
  expect_error(
    join_control(
      x=five_row_df, y=one_row_df,
      join_fun=dplyr::left_join,
      x_control="all", y_control="unique",
      overlap_count=2
    ),
    regexp="Not enough overlapping rows `x` and `y` are in the returned value (2 expected and 1 found)",
    fixed=TRUE,
    info="Test overlap_count (failure)"
  )
})

test_that("join_control algorithm errors", {
  x <- data.frame()
  y <- data.frame()
  
  expect_error(
    join_control(x, y, join_fun=dplyr::left_join, x_control="foo", y_control="any"),
    regexp='should be one of'
  )
  expect_error(
    join_control(x, y, join_fun=dplyr::left_join, x_control="any", y_control="foo"),
    regexp='should be one of'
  )
})

test_that("join_control_check_count_fraction", {
  testdata <- data.frame(A=1:5)
  expect_equal(
    join_control_check_count_fraction(
      fraction=NA_real_, count=NA_integer_,
      data=testdata, value_name="x"
    ),
    0L
  )
  expect_equal(
    join_control_check_count_fraction(
      fraction=1, count=NA_integer_,
      data=testdata, value_name="x"
    ),
    5L
  )
  expect_equal(
    join_control_check_count_fraction(
      fraction=0.5, count=NA_integer_,
      data=testdata, value_name="x"
    ),
    3L,
    info="Rounding goes to up (ceiling())"
  )
  expect_equal(
    join_control_check_count_fraction(
      fraction=NA_real_, count=2,
      data=testdata, value_name="x"
    ),
    2L
  )
})
test_that("join_control_check_count_fraction expected errors", {
  testdata <- data.frame(A=1:5)
  expect_error(
    expect_warning(
      join_control_check_count_fraction(
        fraction="A", count=NA_integer_,
        data=testdata, value_name="x"
      ),
      regexp="NAs introduced by coercion"
    ),
    regexp="For x: `fraction` was not a valid double",
    fixed=TRUE
  )
  expect_error(
    expect_warning(
      join_control_check_count_fraction(
        fraction=NA_real_, count="A",
        data=testdata, value_name="x"
      ),
      regexp="NAs introduced by coercion"
    ),
    regexp="For x: `count` was not a valid integer",
    fixed=TRUE
  )
  expect_error(
    join_control_check_count_fraction(
      fraction=1, count=1,
      data=testdata, value_name="x"
    ),
    regexp="For x: Both `count` and `fraction` cannot be provided",
    fixed=TRUE
  )
  expect_error(
    join_control_check_count_fraction(
      fraction=NA_real_, count=0,
      data=testdata, value_name="x"
    ),
    regexp="For x: 'count' must be between 1 and nrow(data) (5), if not NA.",
    fixed=TRUE
  )
  expect_error(
    join_control_check_count_fraction(
      fraction=NA_real_, count=10,
      data=testdata, value_name="x"
    ),
    regexp="For x: 'count' must be between 1 and nrow(data) (5), if not NA.",
    fixed=TRUE
  )
  expect_error(
    join_control_check_count_fraction(
      fraction=-1, count=NA_integer_,
      data=testdata, value_name="x"
    ),
    regexp="For x: 'fraction' must be >0 and <=1",
    fixed=TRUE
  )
  expect_error(
    join_control_check_count_fraction(
      fraction=2, count=NA_integer_,
      data=testdata, value_name="x"
    ),
    regexp="For x: 'fraction' must be >0 and <=1",
    fixed=TRUE
  )
})

test_that("join_many_to_one and join_one_to_many work", {
  x <- data.frame(A=rep(1:2, 2), B=1:4)
  y_nomissing <- data.frame(A=1:2, C=1:2)
  y_missing <- data.frame(A=1, C=1)
  expect_equal(
    join_many_to_one(x, y_nomissing),
    data.frame(A=rep(1:2, 2), B=1:4, C=rep(1:2, 2))
  )
  expect_error(
    join_many_to_one(x, y_missing),
    regexp="Rows are missing in the new dataset"
  )

  expect_equal(
    join_one_to_many(y_nomissing, x),
    data.frame(A=rep(1:2, each=2), C=rep(1:2, each=2), B=c(1, 3, 2, 4))
  )
  expect_error(
    join_one_to_many(y_missing, x),
    regexp="All rows were are not in the new dataset"
  )
})

test_that("join_one_to_one and join_one_to_many work", {
  x <- data.frame(A=rep(1:2, 2), B=1:4)
  y <- data.frame(A=rep(1:2, 2), C=1:4)
  z <- data.frame(B=1:4, C=5:8)
  expect_equal(
    join_one_to_one(x, z),
    data.frame(A=rep(1:2, 2), B=1:4, C=5:8)
  )
  expect_error(
    join_one_to_one(x, y),
    regexp="Rows are not unique in the new dataset"
  )
})

test_that("join_control expected errors", {
  x <- data.frame(A=rep(1:2, 2), B=1:4)
  y <- data.frame(A=rep(1:2, 2), C=1:4)
  expect_error(
    join_control(x, y, x_control=c("missing", "nomissing")),
    regexp="Both 'missing' and 'nomissing' may not be provided at the same time for `x_control`",
    fixed=TRUE
  )
  expect_error(
    join_control(x, y, y_control=c("missing", "nomissing")),
    regexp="Both 'missing' and 'nomissing' may not be provided at the same time for `y_control`",
    fixed=TRUE
  )
  expect_error(
    join_control(x, y, y_control=c("missing"), join_fun=dplyr::left_join),
    regexp="No rows are missing in the new dataset"
  )
  z <- data.frame(A=1:10, C=1:10)
  expect_error(
    join_control(x, y=z, y_fraction=0.5, join_fun=dplyr::left_join),
    regexp="Not enough rows from `y` are in the returned value (5 expected and 4 found)",
    fixed=TRUE
  )
})
