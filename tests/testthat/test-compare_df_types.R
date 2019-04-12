context("compare_df_types")


test_that("data.frame comparison works", {
  # Names are intentionally not pretty to make it easier to see the source.
  # These data.frames are not typically used, and if the input name is needed,
  # it can be a named argument.
  expect_equal(
    compare_df_types(data.frame(A=1), data.frame(B=2)),
    setNames(
      data.frame(
        column_name=c("A", "B"),
        A=c("numeric", NA),
        B=c(NA, "numeric"),
        stringsAsFactors=FALSE
      ),
      c("column_name",
        "data.frame(A = 1)",
        "data.frame(B = 2)")
    ),
    info="Names are detected from unnamed input"
  )
  expect_equal(
    compare_df_types(foo=data.frame(A=1), bar=data.frame(B=2)),
    data.frame(
      column_name=c("A", "B"),
      foo=c("numeric", NA),
      bar=c(NA, "numeric"),
      stringsAsFactors=FALSE
    ),
    info="Names can be used from the input"
  )
  expect_equal(
    compare_df_types(foo=data.frame(A=1), data.frame(B=2)),
    setNames(
      data.frame(
        column_name=c("A", "B"),
        A=c("numeric", NA),
        B=c(NA, "numeric"),
        stringsAsFactors=FALSE
      ),
      c("column_name",
        "foo",
        "data.frame(B = 2)")
    ),
    info="Names are detected from unnamed input and can be mixed with named arguments"
  )
  expect_equal(
    compare_df_types(foo=data.frame(A=1, B=1), bar=data.frame(B=2)),
    data.frame(
      column_name=c("A", "B"),
      foo="numeric",
      bar=c(NA, "numeric"),
      stringsAsFactors=FALSE
    ),
    info="all output comes through when requested"
  )
  expect_equal(
    compare_df_types(foo=data.frame(A=1, B=1), bar=data.frame(B=2), return="match"),
    data.frame(
      column_name="B",
      foo="numeric",
      bar="numeric",
      stringsAsFactors=FALSE
    ),
    info="only matching output comes through when requested"
  )
  expect_equal(
    compare_df_types(foo=data.frame(A=1, B=1), bar=data.frame(B=2), return="mismatch"),
    data.frame(
      column_name="A",
      foo="numeric",
      bar=NA_character_,
      stringsAsFactors=FALSE
    ),
    info="only mismatching output comes through when requested"
  )
  expect_warning(
    expect_equal(
      compare_df_types(foo=data.frame(A=1, B=1), return="mismatch"),
      data.frame(
        column_name=c("A", "B"),
        foo="numeric",
        stringsAsFactors=FALSE
      ),
      info="A single data.frame gives all results"
    ),
    info="A single data.frame isn't very meaningful, so the user is warned that they probably didn't do what they meant to do."
  )
  expect_equal(
    compare_df_types(
      foo=data.frame(A=1, B=1, C=factor("A"), D=factor("B")),
      bar=data.frame(B=2, C=factor("A"), D=factor(c("A", "B"))),
      return="mismatch"
    ),
    data.frame(
      column_name=c("A", "D"),
      foo=c("numeric", 'factor(levels=c("B"))'),
      bar=c(NA_character_, 'factor(levels=c("A", "B"))'),
      stringsAsFactors=FALSE
    ),
    info="only mismatching output comes through when requested (and it works with something a bit more complex than numeric)"
  )
  expect_equal(
    compare_df_types(
      foo=data.frame(A=1, B=1, C=factor("A"), D=factor("B")),
      bar=data.frame(B=2, C=factor("A"), D=factor(c("A", "B"))),
      return="mismatch",
      bind_check="bind_rows"
    ),
    data.frame(
      column_name="D",
      foo='factor(levels=c("B"))',
      bar='factor(levels=c("A", "B"))',
      stringsAsFactors=FALSE
    ),
    info="bind_rows output skips NA"
  )
})

test_that("class detection works", {
  expect_equal(compare_df_types_class_detect(5), "numeric")
  expect_equal(compare_df_types_class_detect("A"), "character")
  expect_equal(
    compare_df_types_class_detect(as.POSIXct("2019-01-02")), "POSIXct, POSIXt",
    info="multiple classes work"
  )
  expect_equal(
    compare_df_types_class_detect(factor("A")),
    'factor(levels=c("A"))'
  )
  expect_equal(
    compare_df_types_class_detect(factor("A", ordered=TRUE)),
    'ordered, factor(levels=c("A"))'
  )
  expect_equal(
    compare_df_types_class_detect(factor(c("A", "B"), ordered=TRUE)),
    'ordered, factor(levels=c("A", "B"))'
  )
})

test_that("boolean df comparison works", {
  expect_true(compare_df_types_success(data.frame(A=1), data.frame(A=2)))
  expect_output(expect_false(compare_df_types_success(data.frame(A=1), data.frame(B=2))))
  expect_silent(expect_false(compare_df_types_success(data.frame(A=1), data.frame(B=2), verbose=FALSE)))
  expect_true(compare_df_types_success(data.frame(A=1), data.frame(B=2), bind_check="bind_rows"))
})
