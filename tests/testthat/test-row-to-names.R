example_data_row_to_names <-
  list(
    non_factor_data.frame=
      data.frame(
        X__1=c(NA, "Title", 1:3),
        X__2=c(NA, "Title2", 4:6),
        stringsAsFactors=FALSE
      ),
    factor_data.frame=
      data.frame(
        X__1=c(NA, "Title", 1:3),
        X__2=c(NA, "Title2", 4:6),
        stringsAsFactors=TRUE
      )
  )

example_data_row_to_names[[3]] <- tibble::as_tibble(example_data_row_to_names[[1]])
names(example_data_row_to_names)[3] <- "tibble"

test_that("row_to_names invalid and semi-valid input checking", {
  expect_error(
    example_data_row_to_names[[1]] %>%
      row_to_names(row_number = 1:2),
    regexp="row_number must be a scalar"
  )

  expect_error(
    row_to_names(example_data_row_to_names[[1]], row_number=1, remove_row="A"),
    regexp="remove_row must be either TRUE or FALSE, not A",
    fixed=TRUE
  )
  expect_error(
    row_to_names(example_data_row_to_names[[1]], row_number=1, remove_rows_above="A"),
    regexp="remove_rows_above must be either TRUE or FALSE, not A",
    fixed=TRUE
  )

  for (nm in names(example_data_row_to_names)) {
    expect_warning(
      example_data_row_to_names[[nm]] %>%
        row_to_names(row_number=1),
      regexp="Row 1 does not provide unique names. Consider running clean_names() after row_to_names()",
      info=paste("Unique name warning,", nm),
      fixed=TRUE
    )
  }

  # This loop is a test of issue 452 silencing the warning
  for (nm in names(example_data_row_to_names)) {
    expect_silent(
      suppressWarnings(
        example_data_row_to_names[[nm]] %>%
          row_to_names(row_number=1),
        classes="janitor_warn_row_to_names_not_unique"
      )
    )
  }

  expect_error(
    row_to_names(example_data_row_to_names[[1]], row_number="foo"),
    regexp="row_number must be a numeric value or 'find_header'",
    fixed=TRUE
  )

  expect_error(
    row_to_names(
      example_data_row_to_names[[1]],
      row_number=1, remove_row=TRUE, remove_rows_above=TRUE,
      "foo"
    ),
    regexp="Extra arguments (...) may only be given if row_number = 'find_header'.",
    fixed=TRUE
  )
})

test_that("row_to_names works on factor columns", {
  expect_equal(
    example_data_row_to_names$factor_data.frame %>%
      row_to_names(row_number=2) %>%
      names(),
    c("Title", "Title2"),
    info="Works on factors")
})

test_that("row_to_names rows are accurately removed", {
  for (nm in names(example_data_row_to_names)) {
    expect_equal(
      example_data_row_to_names[[nm]] %>%
        row_to_names(row_number=2),
      example_data_row_to_names[[nm]][3:nrow(example_data_row_to_names[[nm]]),,drop=FALSE] %>%
        setNames(nm=c("Title", "Title2")),
      info=paste("All rows are dropped when requested,", nm))
    for (remove_row_flag in c(FALSE, TRUE)) {
      remove_row_drop <- 2[remove_row_flag]
      for (remove_row_above_flag in c(FALSE, TRUE)) {
        remove_row_above_drop <- 1[remove_row_above_flag]
        keep_rows <- setdiff(seq_len(nrow(example_data_row_to_names[[nm]])),
                             c(remove_row_drop, remove_row_above_drop))
        expect_equal(
          example_data_row_to_names[[nm]] %>%
            row_to_names(row_number=2,
                         remove_row=remove_row_flag,
                         remove_rows_above=remove_row_above_flag),
          example_data_row_to_names[[nm]][keep_rows,,drop=FALSE] %>%
            setNames(nm=c("Title", "Title2")),
          info=paste0("Appropriate rows are dropped when requested with explicit information about remove_row=",
                      remove_row_flag,
                      " and remove_rows_above=",
                      remove_row_above_flag, ",", nm))

        expect_equal(
          example_data_row_to_names[[nm]][,1,drop=FALSE] %>%
            row_to_names(row_number=2,
                         remove_row=remove_row_flag,
                         remove_rows_above=remove_row_above_flag),
          example_data_row_to_names[[nm]][keep_rows,1,drop=FALSE] %>%
            setNames(nm=c("Title")),
          info=paste0("With single-column data the result is single-column data.frame still (not a vector) and appropriate rows are dropped when requested with explicit information about remove_row=",
                      remove_row_flag,
                      " and remove_rows_above=",
                      remove_row_above_flag, ",", nm))
      }
    }
  }
})

test_that("row_to_names works on matrices (Fix #320)", {
  expect_equal(
    row_to_names(matrix(LETTERS[1:4], nrow=2, ncol=2), row_number=1),
    matrix(c("B", "D"), nrow=1, dimnames=list(NULL, c("A", "C")))
  )
})

test_that("find_header works", {
  no_complete <-
    data.frame(
      A=NA_character_,
      stringsAsFactors=FALSE
    )
  expect_error(
    find_header(no_complete, "A", "B"),
    regexp="Either zero or one arguments other than 'dat' may be provided.",
    fixed=TRUE
  )
  expect_error(
    find_header(no_complete),
    regexp="No complete rows (rows with zero NA values) were found.",
    fixed=TRUE
  )
  all_partial <-
    data.frame(
      A=c(NA_character_, "A"),
      B=c("B", NA_character_),
      stringsAsFactors=FALSE
    )
  expect_error(
    find_header(all_partial),
    regexp="No complete rows (rows with zero NA values) were found.",
    fixed=TRUE
  )
  single_complete <-
    data.frame(
      A=c(NA_character_, "A"),
      B=c("B", "B"),
      stringsAsFactors=FALSE
    )
  expect_equal(find_header(single_complete), 2)
  expect_equal(find_header(single_complete, "A"), 2)
  expect_error(
    find_header(single_complete, "C"),
    regexp="The string 'C' was not found in column 1",
    fixed=TRUE
  )
  expect_warning(
    expect_equal(
      find_header(single_complete, "B"=2),
      1
    ),
    regexp="The string 'B' was found 2 times in column 2, using the first row where it was found"
  )
  multiple_complete <-
    data.frame(
      A=c("A", "A"),
      B=c("B", "B"),
      stringsAsFactors=FALSE
    )
  expect_equal(find_header(multiple_complete), 1)
  expect_equal(
    find_header(data.frame(A=c(NA, "B", "C", "D"), B=c("C", "D", "E", "F")), "E"=2),
    3,
    info="Use a nontrivial example of finding a row value"
  )
})

test_that("find_header works within row_to_names", {
  single_complete <-
    data.frame(
      A=c(NA_character_, "C"),
      B=c("D", "D"),
      stringsAsFactors=FALSE
    )
  expect_equal(
    row_to_names(dat=single_complete, row_number="find_header"),
    data.frame(C=NA_character_, D=NA_character_, stringsAsFactors=FALSE)[-1,]
  )

  find_correct <-
    data.frame(
      A=c(NA_character_, "C", "D", "E"),
      B=c("D", "D", "E", "F"),
      stringsAsFactors=FALSE
    )
  expect_equal(
    row_to_names(dat=find_correct, row_number="find_header"),
    setNames(find_correct[3:nrow(find_correct),], c("C", "D"))
  )
  expect_equal(
    row_to_names(dat=find_correct, row_number="find_header", "D"),
    setNames(find_correct[4:nrow(find_correct),], c("D", "E"))
  )
  expect_equal(
    row_to_names(dat=find_correct, row_number="find_header", "E"=2),
    setNames(find_correct[4:nrow(find_correct),], c("D", "E"))
  )
})
