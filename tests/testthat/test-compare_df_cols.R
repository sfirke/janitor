test_that("data.frame comparison works", {
  # Names are intentionally not pretty to make it easier to see the source.
  # These data.frames are not typically used, and if the input name is needed,
  # it can be a named argument.
  expect_equal(
    compare_df_cols(data.frame(A=1), data.frame(B=2)),
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
    compare_df_cols(foo=data.frame(A=1), bar=data.frame(B=2)),
    data.frame(
      column_name=c("A", "B"),
      foo=c("numeric", NA),
      bar=c(NA, "numeric"),
      stringsAsFactors=FALSE
    ),
    info="Names can be used from the input"
  )
  expect_equal(
    compare_df_cols(foo=data.frame(A=1), data.frame(B=2)),
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
    compare_df_cols(foo=data.frame(A=1, B=1), bar=data.frame(B=2)),
    data.frame(
      column_name=c("A", "B"),
      foo="numeric",
      bar=c(NA, "numeric"),
      stringsAsFactors=FALSE
    ),
    info="all output comes through when requested"
  )
  expect_equal(
    compare_df_cols(foo=data.frame(A=1, B=1), bar=data.frame(B=2), return="match", bind_method = "rbind"),
    data.frame(
      column_name="B",
      foo="numeric",
      bar="numeric",
      stringsAsFactors=FALSE
    ),
    info="only matching output comes through when requested"
  )
  expect_equal(
    compare_df_cols(foo=data.frame(A=1, B=1), bar=data.frame(B=2), return="mismatch", bind_method = "rbind"),
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
      compare_df_cols(foo=data.frame(A=1, B=1), return="mismatch"),
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
    compare_df_cols(
      foo=data.frame(A=1, B=1, C=factor("A"), D=factor("B")),
      bar=data.frame(B=2, C=factor("A"), D=factor(c("A", "B"))),
      return="mismatch",
      bind_method = "rbind"
    ),
    data.frame(
      column_name="A",
      foo="numeric",
      bar=NA_character_,
      stringsAsFactors=FALSE
    ),
    info="only mismatching output comes through when requested (and it works with something a bit more complex than numeric)"
  )
  expect_equal(
    compare_df_cols(
      foo=data.frame(A=1, B=1, C=factor("A"), D=factor("B")),
      bar=data.frame(B=2, C=factor("A"), D=factor(c("A", "B"))),
      return="mismatch",
      bind_method = "rbind",
      strict_description=TRUE
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
    compare_df_cols(
      foo=data.frame(A=1, B=1, C=factor("A"), D=factor("B")),
      bar=data.frame(B=2, C=factor("A"), D=factor(c("A", "B"))),
      return="mismatch",
      strict_description=FALSE
    ),
    data.frame(
      column_name="D",
      foo='factor',
      bar='factor',
      stringsAsFactors=FALSE
    )[-1,],
    info="bind_rows output skips NA"
  )
  expect_equal(
    compare_df_cols(
      foo=data.frame(A=1, B=1, C=factor("A"), D=factor("B")),
      bar=data.frame(B=2, C=factor("A"), D=factor(c("A", "B"))),
      return="mismatch",
      bind_method="bind_rows",
      strict_description=TRUE
    ),
    data.frame(
      column_name="D",
      foo='factor(levels=c("B"))',
      bar='factor(levels=c("A", "B"))',
      stringsAsFactors=FALSE
    ),
    info="bind_rows output skips NA"
  )

  expect_warning(
    expect_equal(
      compare_df_cols(data.frame()),
      data.frame(
        column_name=character(0),
        stringsAsFactors=FALSE
      )
    ),
    regexp="data.frame() has zero columns and will not appear in output.",
    fixed=TRUE,
    info="empty data.frames by themselves work"
  )
  expect_warning(
    expect_equal(
      compare_df_cols(foo=data.frame(), bar=data.frame(A=1)),
      data.frame(
        column_name="A",
        bar="numeric",
        stringsAsFactors=FALSE
      )
    ),
    regexp="foo has zero columns and will not appear in output.",
    fixed=TRUE,
    info="empty data.frames with other inputs work"
  )
})

test_that("class detection works", {
  expect_equal(describe_class(5), "numeric")
  expect_equal(describe_class("A"), "character")
  expect_equal(
    describe_class(as.POSIXct("2019-01-02")), "POSIXct, POSIXt",
    info="multiple classes work"
  )
  expect_equal(
    describe_class(factor("A")),
    'factor(levels=c("A"))'
  )
  expect_equal(
    describe_class(factor("A", ordered=TRUE)),
    'ordered, factor(levels=c("A"))'
  )
  expect_equal(
    describe_class(factor(c("A", "B"), ordered=TRUE)),
    'ordered, factor(levels=c("A", "B"))'
  )
})

test_that("class description without strict description", {
  # No change with numeric
  expect_equal(describe_class(5, strict_description=FALSE), "numeric")
  # ordered factors don't show "ordered" or the levels
  expect_equal(describe_class(ordered(c("A", "B")), strict_description=FALSE), "factor")
})

test_that("boolean df comparison works", {
  expect_true(compare_df_cols_same(data.frame(A=1), data.frame(A=2)))
  expect_output(expect_false(compare_df_cols_same(data.frame(A=1), data.frame(A="string"))))
  expect_silent(expect_false(compare_df_cols_same(data.frame(A=1), data.frame(A="string"), verbose=FALSE)))
  expect_false(compare_df_cols_same(data.frame(A=1L), data.frame(A=1.5), bind_method="bind_rows", verbose = FALSE))
  expect_false(compare_df_cols_same(data.frame(A=1L), data.frame(A=1.5), bind_method="rbind", verbose = FALSE))
})

test_that("error messages are correct", {
  expect_error(
    compare_df_cols("A"),
    regexp="Input given with.*Argument number 1 is not."
  )
  expect_error(
    compare_df_cols("A", data.frame(A=1), 3),
    regexp="Input given with.*Argument numbers 1, 3 are not."
  )
})

test_that("list inputs to compare_df_cols give appropriate errors", {
  expect_error(
    compare_df_cols(list("A")),
    regexp="List inputs must be lists of data.frames.  List input number 1 is not a list of data.frames.",
    fixed=TRUE
  )
  expect_error(
    compare_df_cols(data.frame(), list("A")),
    regexp="List inputs must be lists of data.frames.  List input number 2 is not a list of data.frames.",
    fixed=TRUE
  )
  expect_error(
    compare_df_cols(list("A"), list("A")),
    regexp="List inputs must be lists of data.frames.  List input numbers 1, 2 are not lists of data.frames.",
    fixed=TRUE
  )
  expect_error(
    compare_df_cols(list("A"), list("A"), list("A"), list("A"), list("A"), list("A")),
    regexp="List inputs must be lists of data.frames.  List input numbers 1, 2, 3, 4, 5, ... are not lists of data.frames.",
    fixed=TRUE
  )
  expect_error(
    compare_df_cols(list(column_name=data.frame())),
    regexp="None of the input ... argument names or list names may be `column_name`.",
    fixed=TRUE
  )
})

test_that("list inputs to compare_df_cols work as expected", {
  expect_warning(
    expect_equal(
      compare_df_cols(
        list(foo=data.frame(), bar=data.frame(A=1, B=2)),
        baz=data.frame(A=2, C=3)
      ),
      data.frame(
        column_name=c("A", "B", "C"),
        bar=c("numeric", "numeric", NA_character_),
        baz=c("numeric", NA_character_, "numeric"),
        stringsAsFactors=FALSE
      )
    ),
    regexp="foo has zero columns and will not appear in output.",
    info="empty data.frame with other data.frames"
  )
  expect_equal(
    compare_df_cols(
      list(foo=data.frame(A=1), bar=data.frame(A=1, B=2)),
      baz=data.frame(A=2, C=3)
    ),
    data.frame(
      column_name=c("A", "B", "C"),
      foo=c("numeric", NA_character_, NA_character_),
      bar=c("numeric", "numeric", NA_character_),
      baz=c("numeric", NA_character_, "numeric"),
      stringsAsFactors=FALSE
    )
  )
  # Naming complexity
  expect_equal(
    compare_df_cols(
      list(data.frame(A=1), bar=data.frame(A=1, B=2)),
      baz=data.frame(A=2, C=3)
    ),
    setNames(
      data.frame(
        column_name=c("A", "B", "C"),
        foo=c("numeric", NA_character_, NA_character_),
        bar=c("numeric", "numeric", NA_character_),
        baz=c("numeric", NA_character_, "numeric"),
        stringsAsFactors=FALSE
      ),
      c("column_name", "list(data.frame(A = 1), bar = data.frame(A = 1, B = 2))_1", "bar", "baz")
    )
  )
  expect_equal(
    compare_df_cols(
      foo=list(data.frame(A=1), bar=data.frame(A=1, B=2)),
      baz=data.frame(A=2, C=3)
    ),
    setNames(
      data.frame(
        column_name=c("A", "B", "C"),
        foo=c("numeric", NA_character_, NA_character_),
        bar=c("numeric", "numeric", NA_character_),
        baz=c("numeric", NA_character_, "numeric"),
        stringsAsFactors=FALSE
      ),
      c("column_name", "foo_1", "bar", "baz")
    )
  )
  expect_equal(
    compare_df_cols(
      foo=list(data.frame(A=1), data.frame(A=1, B=2)),
      baz=data.frame(A=2, C=3)
    ),
    setNames(
      data.frame(
        column_name=c("A", "B", "C"),
        foo=c("numeric", NA_character_, NA_character_),
        bar=c("numeric", "numeric", NA_character_),
        baz=c("numeric", NA_character_, "numeric"),
        stringsAsFactors=FALSE
      ),
      c("column_name", "foo_1", "foo_2", "baz")
    )
  )
})

test_that("compare_df_cols_df_maker catches bad inputs", {
  expect_error(
    compare_df_cols_df_maker(x = mtcars, class_colname = "column_name"),
    regexp = '`class_colname` cannot be "column_name"',
    fixed = TRUE
  )
  expect_error(
    compare_df_cols_df_maker(list(x = mtcars, y = iris), class_colname = "class"),
    regexp = '`x` and `class_colname` must be the same length.',
    fixed = TRUE
  )
  expect_error(
    compare_df_cols_df_maker(list(x = mtcars), class_colname = c("column_name")),
    regexp = '`class_colname` cannot be "column_name"',
    fixed = TRUE
  )
})
