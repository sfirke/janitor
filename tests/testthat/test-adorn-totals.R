dat <- data.frame(
  a = factor(c(rep(c("big", "small", "big"), 3)), levels = c("small", "big")),
  b = c(1:3, 1:3, 1, 1, 1)
)
ct <- dat %>%
  tabyl(a, b)

mixed <- data.frame(
  a = 1:3,
  b = c("x", "y", "z"),
  c = 5:7,
  d = c("big", "med", "small"),
  stringsAsFactors = FALSE
)

test_that("totals row is correct", {
  expect_equal(
    untabyl(adorn_totals(ct, "row")),
    data.frame(
      a = factor(c("small", "big", "Total"), levels = c("small", "big", "Total")),
      `1` = c(1, 4, 5),
      `2` = c(2, 0, 2),
      `3` = c(0, 2, 2),
      check.names = FALSE
    )
  )
})

test_that("totals col is correct", {
  expect_equal(
    untabyl(adorn_totals(ct, "col")),
    data.frame(
      a = factor(c("small", "big"), levels = c("small", "big")),
      `1` = c(1, 4),
      `2` = c(2, 0),
      `3` = c(0, 2),
      Total = c(3, 6),
      check.names = FALSE
    )
  )
})

test_that("totals row and col produce correct results when called together", {
  expect_equal(
    ct %>%
      adorn_totals(c("row", "col")) %>%
      untabyl(),
    data.frame(
      a = factor(c("small", "big", "Total"), levels = c("small", "big", "Total")),
      `1` = c(1, 4, 5),
      `2` = c(2, 0, 2),
      `3` = c(0, 2, 2),
      Total = c(3, 6, 9),
      check.names = FALSE
    )
  )
})

test_that("totals where='both' produce equivalent results to c('row','col')", {
  expect_equal(
    ct %>%
      adorn_totals("both") %>%
      untabyl(),
    ct %>%
      adorn_totals(c("row", "col")) %>%
      untabyl()
  )
})

test_that("order doesn't matter when row and col are called together", {
  expect_equal(
    ct %>%
      adorn_totals(c("row", "col")) %>%
      untabyl(),
    ct %>%
      adorn_totals(c("col", "row")) %>%
      untabyl()
  )
})

test_that("both functions work with a single column", {
  single_col <- tibble::tibble(
    a = c(as.Date("2016-01-01"), as.Date("2016-02-03")),
    b = c(1, 2)
  )
  expect_error(single_col %>% adorn_totals("row"), NA) # this method of testing passage is from http://stackoverflow.com/a/30068233
  expect_error(single_col %>% adorn_totals("col"), NA)
  expect_error(single_col %>% adorn_totals(c("col", "row")), NA)
})

dat <- data.frame(
  a = c("hi", "lo"),
  b = c(1, 2),
  c = c(5, 10),
  d = c("big", "small"),
  e = c(20, NA),
  stringsAsFactors = FALSE
)

test_that("numeric first column is ignored", {
  expect_equal(
    mtcars %>%
      tabyl(cyl, gear) %>%
      adorn_totals("col") %>%
      untabyl(),
    data.frame(
      cyl = c(4, 6, 8),
      `3` = c(1, 2, 12),
      `4` = c(8, 4, 0),
      `5` = c(2, 1, 2),
      Total = c(11, 7, 14),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
})

# create input tables for subsequent testing
ct_2 <-
  mtcars %>%
  dplyr::group_by(cyl, gear) %>%
  dplyr::tally() %>%
  tidyr::spread(gear, n)
df1 <- data.frame(x = c(1, 2), y = c(NA, 4))

test_that("grouped_df gets ungrouped and succeeds", {
  ct_2 <-
    mtcars %>%
    dplyr::group_by(cyl, gear) %>%
    dplyr::tally() %>%
    tidyr::spread(gear, n)
  expect_equal(
    ct_2 %>% adorn_totals(),
    ct_2 %>% dplyr::ungroup() %>% adorn_totals()
  )
})

test_that("na.rm value works correctly", {
  expect_equal(
    df1 %>% adorn_totals(c("row", "col"), na.rm = FALSE) %>% untabyl(),
    data.frame(
      x = c("1", "2", "Total"),
      y = c(NA, 4, NA),
      Total = c(NA, 4, NA),
      stringsAsFactors = FALSE
    )
  )
})

test_that("add_totals respects if input was data.frame", {
  expect_equal(
    class(df1),
    class(df1 %>% adorn_totals() %>% untabyl())
  )
})

test_that("add_totals respects if input was tibble", {
  expect_equal(
    class(df1 %>% tibble::as_tibble()),
    class(df1 %>% tibble::as_tibble() %>% adorn_totals() %>% untabyl())
  )
})

test_that("error thrown if no columns past first are numeric", {
  df2 <- data.frame(
    x = c("big", "small"),
    y = c("hi", "lo")
  )
  expect_error(
    adorn_totals(df2, "col"),
    "at least one targeted column must be of class numeric.  Control target variables with the ... argument. adorn_totals should be called before other adorn_ functions."
  )
  expect_error(
    mixed %>%
      adorn_totals("row","-",TRUE,"Totals",d),
    "at least one targeted column must be of class numeric.  Control target variables with the ... argument. adorn_totals should be called before other adorn_ functions."
  )

  # Add a test where only the first column is numeric
  df3 <- data.frame(
    x = 1:2,
    y = c("hi", "lo")
  )
  expect_error(
    adorn_totals(df3),
    "at least one targeted column must be of class numeric.  Control target variables with the ... argument. adorn_totals should be called before other adorn_ functions."
  )
})

test_that("bad input to where arg is caught", {
  expect_error(mtcars %>%
    adorn_totals("blargh"),
  paste0('"where" must be one of "row", "col", or c("row", "col")'),
  fixed = TRUE
  )
})


test_that("works with non-numeric columns mixed in; fill character specification", {
  expect_equal(
    mixed %>% adorn_totals(where = c("row", "col"), fill = "*") %>% untabyl(),
    data.frame(
      a = c("1", "2", "3", "Total"),
      b = c("x", "y", "z", "*"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "*"),
      Total = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})

test_that("fill works with multiple factor and date columns", {
  has_facs <- data.frame(
    a = c("hi", "low"),
    b = c("big", "small"),
    c = c(as.Date("2000-01-01"), as.Date("2000-01-02")),
    d = 1:2
  )
  expect_equal(
    adorn_totals(has_facs, "row") %>% untabyl,
    data.frame(a = c("hi", "low", "Total"),
               b = c("big", "small", "-"),
               c = c("2000-01-01","2000-01-02", "-"),
               d = 1:3,
               stringsAsFactors = FALSE
    ))
})

test_that("totals attributes are assigned correctly", {
  post <- adorn_totals(ct, c("row", "col"))
  expect_equal(attr(post, "totals"), c("row", "col"))
  expect_equal(class(post), c("tabyl", "data.frame"))
  expect_equal(attr(post, "tabyl_type"), "two_way")
  expect_equal(attr(post, "core"), untabyl(ct))

  post_col <- adorn_totals(ct, "col")
  expect_equal(attr(post_col, "totals"), "col")
  expect_equal(class(post_col), c("tabyl", "data.frame"))
  expect_equal(attr(post_col, "tabyl_type"), "two_way")
  expect_equal(attr(post_col, "core"), untabyl(ct))

  post_sequential_both <- adorn_totals(ct, "col") %>%
    adorn_totals("row")
  expect_equal(post_sequential_both, post, ignore_attr = TRUE)
  expect_equal(
    sort(attr(post, "totals")),
    sort(attr(post_sequential_both, "totals"))
  )
})

test_that("trying to re-adorn a dimension fails", {
  expect_error(
    ct %>% adorn_totals("col") %>% adorn_totals("col"),
    "trying to re-add a totals dimension that is already been added"
  )
  expect_error(
    ct %>% adorn_totals() %>% adorn_totals(),
    "trying to re-add a totals dimension that is already been added"
  )
})

test_that("automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear)
  expect_equal(
    adorn_totals(three), # vanilla call
    purrr::map(three, adorn_totals)
  )

  # with arguments passing through
  expect_equal(
    adorn_totals(three, c("row", "col"), fill = "---", na.rm = FALSE, name = "dummy_name"),
    purrr::map(three, adorn_totals, c("row", "col"), fill = "---", FALSE, name = "dummy_name")
  )
})

test_that("non-data.frame inputs are handled", {
  expect_error(adorn_totals(1:5), "adorn_totals() must be called on a data.frame or list of data.frames", fixed = TRUE)
})

test_that("row total name is changed", {
  expect_equal(
    as.character(adorn_totals(ct, name = "NewTitle")[nrow(ct) + 1, 1]),
    "NewTitle"
  )
})

test_that("column total name is changed", {
  expect_equal(
    colnames(adorn_totals(ct, where = "col", name = "NewTitle"))[(ncol(ct) + 1)],
    "NewTitle"
  )
})

test_that("tidyselecting works", {
  cyl_gear <- mtcars %>%
    adorn_totals(c("row", "col"), "-", TRUE, "cylgear", c(cyl, gear))
  expect_equal(cyl_gear$cylgear, c(mtcars$cyl + mtcars$gear, (sum(mtcars$cyl) + sum(mtcars$gear))))
  expect_equal(
    unname(unlist(cyl_gear[33, ])),
    c("cylgear", "198", rep("-", 7), "118", "-", "316")
  )

  # Can override the first column not being included
  # adorn_totals() still fails if ONLY the first column is numeric, that's fine - it's a nonsensical operation
  simple <- data.frame(
    x = 1:2,
    y = 3:4,
    z = c("hi", "lo")
  )

  expect_message(
    simple %>%
      adorn_totals(c("row", "col"), "-", TRUE, "Total", x),
    "Because the first column was specified to be totaled, it does not contain the label 'Total' (or user-specified name) in the totals row",
    fixed = TRUE
  )

  expect_message(
    simple_total <- simple %>%
      adorn_totals(c("row", "col"), "-", TRUE, "Total", x),
    regexp = "Because the first column was specified to be totaled, it does not contain the label 'Total' (or user-specified name) in the totals row",
    fixed = TRUE
  )

  expect_equal(unname(unlist(simple_total[3, ])), c("3", "-", "-", "3"))
  expect_equal(simple_total$Total, 1:3)

  # test that leaving out a numeric column of a tibble succeeds, #388
  expect_equal(
    simple %>%
      adorn_totals(,,,,y) %>%
      as.data.frame(),
    simple %>%
      tibble::tibble() %>%
      adorn_totals %>%
      as.data.frame()
  )
})

test_that("supplying NA to fill preserves column types", {
  test_df <- data.frame(
    a = c("hi", "low", "med"),
    b = factor(c("big", "small", "regular")),
    c = c(as.Date("2000-01-01"), as.Date("2000-01-02"), as.Date("2000-01-03")),
    d = c(as.POSIXct("2000-01-01", tz = "UTC"), as.POSIXct("2000-01-02"), as.POSIXct("2000-01-03")),
    e = 1:3,
    f = 4:6,
    g = c(TRUE, FALSE, TRUE),
    h = c(7.2, 8.2, 9.2),
    stringsAsFactors = FALSE
  )

  out <- adorn_totals(test_df, fill = NA)

  # expect types to be preserved
  expect_type(out[["a"]], "character")
  expect_s3_class(out[["b"]], "factor")
  expect_s3_class(out[["c"]], "Date")
  expect_s3_class(out[["d"]], "POSIXct")
  expect_type(out[["g"]], "logical")
  # expect factor levels to be preserved
  expect_equal(levels(out[["b"]]), levels(test_df[["b"]]))
  # expect NAs in total rows for non-numerics
  expect_true(is.na(out[4, "b"]))
  expect_true(is.na(out[4, "c"]))
  expect_true(is.na(out[4, "d"]))
  expect_true(is.na(out[4, "g"]))
  # test values of totals
  expect_equal(out[4, "a"], "Total")
  expect_equal(out[4, "e"], 6)
  expect_equal(out[4, "f"], 15)
  expect_equal(out[4, "h"], 24.6)
  # expect original df intact
  expect_equal(test_df, out[1:3,], ignore_attr = TRUE)
})

test_that("supplying NA as fill still works with non-character first col and numeric non-totaled cols", {
  test_df <- data.frame(
    a = factor(c("hi", "low", "med"), levels = c("low", "med", "hi")),
    b = factor(c("big", "small", "regular")),
    c = c(as.Date("2000-01-01"), as.Date("2000-01-02"), as.Date("2000-01-03")),
    d = 1:3,
    e = 4:6,
    f = c(TRUE, FALSE, TRUE),
    g = c(7.2, 8.2, 9.2),
    stringsAsFactors = FALSE
  )

  out <- adorn_totals(test_df,
                      where = "row",
                      fill = NA,
                      na.rm = TRUE,
                      name = "Total",
                      d, e)

  expect_equal(out[["a"]], factor(c("hi", "low", "med", "Total"), levels = c("low", "med", "hi", "Total")))
  expect_equal(out[["g"]], c(7.2, 8.2, 9.2, NA_real_))
  expect_equal(out[4,"d"], 6)
  expect_equal(out[4,"e"], 15)
  expect_equal(test_df[1:3, 2:7], out[1:3,2:7], ignore_attr = TRUE)
})

test_that("one_way tabyl inputs retain that class", {
  expect_equal(
    attr(mtcars %>% tabyl(am) %>% adorn_totals("both"), "tabyl_type"),
    "one_way"
  )
})


# Tests from #413, different values for row and col names
test_that("long vectors are trimmed", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "row",
        name = c("total", "something_else"),
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "total"),
      b = c("x", "y", "z", "-"),
      c = c(5:7, 18),
      d = c("big", "med", "small", "-"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("row and column names are taken correctly from a vector", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "both",
        name = c("row_name", "col_name"),
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "row_name"),
      b = c("x", "y", "z", "-"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "-"),
      col_name = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})

test_that("row and column names are taken correctly from a single name", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "both",
        name = "totals",
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "totals"),
      b = c("x", "y", "z", "-"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "-"),
      totals = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})

dat <- data.frame(
  a = c(rep(c("big", "small", "big"), 3)),
  b = c(1:3, 1:3, 1, 1, 1),
  stringsAsFactors = TRUE
)
ct <- dat %>%
  tabyl(a, b)

mixed <- data.frame(
  a = 1:3,
  b = c("x", "y", "z"),
  c = 5:7,
  d = c("big", "med", "small"),
  stringsAsFactors = FALSE
)

test_that("long vectors are trimmed", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "row",
        name = c("total", "row_total"),
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "total"),
      b = c("x", "y", "z", "-"),
      c = c(5:7, 18),
      d = c("big", "med", "small", "-"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("row and column names are taken correctly from a vector", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "both",
        name = c("column_totals", "row_totals"),
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "column_totals"),
      b = c("x", "y", "z", "-"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "-"),
      row_totals = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})

test_that("row and column names are taken correctly from a single name", {
  expect_equal(
    mixed %>%
      adorn_totals(
        where = "both",
        name = "totals",
        fill = "-") %>%
      untabyl(),
    data.frame(
      a = c(as.character(1:3), "totals"),
      b = c("x", "y", "z", "-"),
      c = c(5, 6, 7, 18),
      d = c("big", "med", "small", "-"),
      totals = c(5, 6, 7, 18),
      stringsAsFactors = FALSE
    )
  )
})

test_that("order is maintained when first column is a factor, #494", {
  o <- data.frame(a = 1:5,
                  fac = factor(c("orange", "blue", "orange", "orange", "blue")),
                  ord = ordered(
                    c("huge", "medium", "small", "medium", "medium"),
                    levels = c("small", "medium", "huge")))
  
  o_tabyl_totaled <- o %>%
    tabyl(ord, a) %>%
    adorn_totals("both")
  
  expect_equal(
    attr(o_tabyl_totaled$ord, "levels"),
    c("small", "medium", "huge", "Total")
  )
  expect_equal(
    class(o_tabyl_totaled$ord),
    c("ordered", "factor")
  )
  f_tabyl_totaled <- o %>%
    tabyl(fac, a) %>%
    adorn_totals("both")
  
  expect_equal(
    attr(f_tabyl_totaled$fac, "levels"),
    c("blue", "orange", "Total")
  )
  expect_equal(
    class(f_tabyl_totaled$fac),
    "factor"
  )
})
