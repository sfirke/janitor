test_df <- data.frame(a = c(1, 3, 3, 3, 5), b = c("a", "c", "c", "e", "c"), stringsAsFactors = FALSE)

test_that("Correct combinations of duplicates are found", {
  expect_equal(get_dupes(test_df, a), data.frame(a = test_df[[1]][2:4], dupe_count = rep(3L, 3), b = test_df[[2]][2:4], stringsAsFactors = FALSE))
  expect_equal(get_dupes(test_df, b), data.frame(b = test_df[[2]][c(2:3, 5)], dupe_count = rep(3L, 3), a = test_df[[1]][c(2:3, 5)], stringsAsFactors = FALSE))
})

test_that("calling with no specified variable names uses all variable names", {
  expect_message(
    expect_equal(get_dupes(test_df), get_dupes(test_df, a, b)),
    "No variable names specified - using all columns."
  )
  expect_message(expect_message(
    get_dupes(mtcars),
    "No variable names specified - using all columns."),
    "No duplicate combinations found of: mpg, cyl.*and 2 other variables"
  )
})

no_dupes <- data.frame(a = 1, stringsAsFactors = FALSE)

test_that("instances of no dupes throw correct messages, return empty df", {
  expect_message(no_dupes %>% get_dupes(a), "No duplicate combinations found of: a")
  expect_message(
    no_dup_a <- no_dupes %>% get_dupes(a),
    "No duplicate combinations found of: a"
  )
  expect_equal(
    no_dup_a,
    data.frame(a = double(0), dupe_count = integer(0))
  )
  expect_message(expect_message(
    mtcars %>% dplyr::select(-1) %>% get_dupes(),
    "No variable names specified - using all columns."),
    "No duplicate combinations found of: cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb"
  )
  expect_message(expect_message(
    mtcars %>% get_dupes(),
    "No variable names specified - using all columns."),
    "No duplicate combinations found of: mpg, cyl, disp, hp, drat, wt, qsec, vs, am, ... and 2 other variables"
  )
})

test_that("incorrect variable names are handled", {
  expect_error(get_dupes(mtcars, x))
})

test_that("works on variables with irregular names", {
  badname_df <- mtcars %>% dplyr::mutate(`bad name!` = mpg * 1000)
  expect_equal(
    badname_df %>% get_dupes(`bad name!`, cyl) %>% dim(),
    c(10, 13)
  ) # does it return the right-sized result?
  expect_message(expect_message(
    badname_df_dup <- badname_df %>% get_dupes(),
    "No variable names specified - using all columns"),
    "No duplicate combinations found of: mpg, cyl, disp, hp, drat, wt, qsec, vs, am, ... and 3 other variables"
  )
  expect_s3_class(badname_df_dup, "data.frame") # test for success, i.e., produces a data.frame (with 0 rows)
})

test_that("tidyselect specification matches exact specification", {
  expect_equal(mtcars %>% get_dupes(contains("cy"), mpg), mtcars %>% get_dupes(cyl, mpg))
  expect_equal(mtcars %>% get_dupes(mpg), mtcars %>% get_dupes(-c(cyl, disp, hp, drat, wt, qsec, vs, am ,gear, carb)))
  expect_equal(
    suppressMessages(mtcars %>% dplyr::select(cyl, wt) %>% get_dupes()),
    mtcars %>% dplyr::select(cyl, wt) %>% get_dupes(dplyr::everything())
  )
})

test_that("grouped and ungrouped data is handled correctly", {
  expect_equal(
    mtcars %>% dplyr::group_by(carb, cyl) %>% get_dupes(mpg, carb) %>% dplyr::group_vars(),
    mtcars %>% dplyr::group_by(carb, cyl) %>% dplyr::group_vars()
  )
  expect_equal(
    mtcars %>% dplyr::group_by(carb, cyl) %>% get_dupes(mpg, carb) %>% dplyr::ungroup(),
    mtcars %>% tibble::as_tibble() %>% get_dupes(mpg, carb)
  )
})

test_that("tibbles stay tibbles, non-tibble stay non-tibbles", {
  expect_equal(
    class(test_df %>% get_dupes(a)),
    class(test_df)
  )
  expect_equal(
    class(tibble::as_tibble(test_df) %>% get_dupes(a)),
    class(tibble::as_tibble(test_df))
  )
})

test_that("result is sorted by dupe_count descending, #493", {
  test_sort <- data.frame(
    a = c("x", "x", "y", "y", "y", "a", "a"),
    b = 1:7
  )
  res <- get_dupes(test_sort, a)
  expect_equal(unique(res$a), c("y", "a", "x")) # y has 3, then alphabetically a precedes x
})
