# Tests tabyl class functions

a <- mtcars %>%
  tabyl(cyl, carb)

b <- mtcars %>%
  dplyr::count(cyl, carb) %>%
  tidyr::pivot_wider(
    names_from = carb,
    values_from = n,
    values_fill = 0,
    names_sort = TRUE
  ) %>%
  as.data.frame() # for comparison purposes, remove the tbl_df aspect


test_that("as_tabyl works on result of a non-janitor count/pivot_wider", {
  expect_equal(
    as_tabyl(a),
    as_tabyl(b, 2, "cyl", "carb")
  )
})

test_that("as_tabyl sets attributes correctly", {
  d <- as_tabyl(a)
  expect_equal(class(d), class(a))
  expect_equal(attr(d, "core"), untabyl(a))
  expect_equal(attr(d, "tabyl_type"), "two_way")
})

test_that("untabyl puts back to original form", {
  expect_equal(mtcars, untabyl(as_tabyl(mtcars)))
})

test_that("untabyl warns if called on non-tabyl", {
  expect_warning(
    untabyl(mtcars),
    "untabyl\\(\\) called on a non-tabyl"
  )
})

test_that("untabyl automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear)
  expect_equal(
    untabyl(three), # vanilla call
    purrr::map(three, untabyl)
  )
})

test_that("as_tabyl is okay with non-numeric columns", {
  e <- b %>%
    dplyr::mutate(extra = "val")
  expect_equal(attr(as_tabyl(e), "core"), e) # implied success of as_tabyl
})

test_that("as_tabyl fails if no numeric columns in 2:n", {
  bad <- data.frame(
    a = 1:2,
    b = c("x", "y")
  )
  expect_error(as_tabyl(bad), "at least one one of columns 2:n must be of class numeric")
})

test_that("bad inputs are caught", {
  expect_error(as_tabyl(mtcars, 3),
    "axes must be either 1 or 2",
    fixed = TRUE
  )

  expect_error(as_tabyl(1:10),
    "input must be a data.frame",
    fixed = TRUE
  )

  # don't pass names to a 1-way tabyl
  expect_error(
    as_tabyl(mtcars, axes = 1, row_var_name = "foo"),
    "variable names are only meaningful for two-way tabyls"
  )
})

test_that("adorn_totals and adorn_percentages reset the tabyl's core to reflect sorting, #407", {
  unsorted <- mtcars %>% tabyl(am, cyl)
  sorted <- dplyr::arrange(unsorted, desc(`4`))
  expect_equal(
    sorted %>%
      adorn_totals() %>%
      attr(., "core"),
    sorted %>%
      untabyl()
  )
  expect_equal(
    sorted %>%
      adorn_percentages() %>%
      attr(., "core"),
    sorted %>%
      untabyl()
  )
  # both:
  expect_equal(
    sorted %>%
      adorn_totals() %>%
      adorn_percentages() %>%
      attr(., "core"),
    sorted %>%
      untabyl()
  )
  # Ns with "Total" row sorted to top - the Total N should be up there too:
  expect_equal(
    sorted %>%
      adorn_totals() %>%
      adorn_percentages("col") %>%
      dplyr::arrange(desc(`4`)) %>%
      adorn_ns() %>%
      dplyr::pull(`4`) %>%
      dplyr::first(),
    "1.0000000 (11)"
  )
})

test_that("as_tabyl rebuilds a missing core instead of erroring, #527", {
  # dplyr verbs that subset columns keep the tabyl class but drop its attributes
  stripped <- dplyr::select(a, dplyr::everything())
  expect_s3_class(stripped, "tabyl")
  expect_null(attr(stripped, "core"))

  rebuilt <- as_tabyl(stripped)
  expect_equal(attr(rebuilt, "core"), untabyl(a))
  expect_equal(attr(rebuilt, "tabyl_type"), "two_way")
  # core is a plain data.frame without tabyl class or attributes
  expect_identical(class(attr(rebuilt, "core")), "data.frame")
  expect_null(attr(attr(rebuilt, "core"), "core"))
})

test_that("as_tabyl handles an object of class tabyl with no attributes at all", {
  bare <- structure(
    list(x = c("a", "b"), y = c(1, 2)),
    class = c("tabyl", "data.frame"),
    row.names = 1:2
  )
  res <- as_tabyl(bare)
  expect_equal(attr(res, "core"), data.frame(x = c("a", "b"), y = c(1, 2)))
  expect_identical(class(attr(res, "core")), "data.frame")
  expect_equal(attr(res, "tabyl_type"), "two_way")
  expect_equal(class(res), c("tabyl", "data.frame"))
})

test_that("leftover tabyl attributes are not copied into a rebuilt core", {
  partial <- adorn_totals(a)
  attr(partial, "core") <- NULL
  res <- as_tabyl(partial)
  expect_equal(attr(res, "totals"), "row")
  expect_null(attr(attr(res, "core"), "totals"))
  expect_null(attr(attr(res, "core"), "tabyl_type"))
  expect_null(attr(attr(res, "core"), "var_names"))
})

test_that("a one-way tabyl that lost its attributes stays one_way, #527", {
  one <- mtcars %>%
    tabyl(cyl) %>%
    dplyr::mutate(percent = round(percent * 100, 1))
  expect_null(attr(one, "tabyl_type"))
  expect_equal(attr(as_tabyl(one), "tabyl_type"), "one_way")

  # with valid_percent column
  one_na <- data.frame(x = c("a", "a", "b", NA)) %>%
    tabyl(x) %>%
    dplyr::select(dplyr::everything())
  expect_equal(attr(as_tabyl(one_na), "tabyl_type"), "one_way")

  # an explicit axes value is respected
  expect_equal(attr(as_tabyl(one, axes = 2), "tabyl_type"), "two_way")

  # a plain data.frame with one-way column names is not inferred
  plain <- data.frame(x = c("a", "b"), n = 1:2, percent = c(1 / 3, 2 / 3))
  expect_equal(attr(as_tabyl(plain), "tabyl_type"), "two_way")
})
