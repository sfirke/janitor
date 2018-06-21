library(janitor)
context("row_to_names()")

library(dplyr)

example_data_row_to_names <-
  list(
    non_factor_data.frame=
      data.frame(X__1=c(NA, "Title", 1:3),
                 X__2=c(NA, "Title2", 4:6),
                 stringsAsFactors=FALSE),
    factor_data.frame=
      data.frame(X__1=c(NA, "Title", 1:3),
                 X__2=c(NA, "Title2", 4:6)),
    tibble=
      tibble(X__1=c(NA, "Title", 1:3),
             X__2=c(NA, "Title2", 4:6)))
    
test_that("row_to_names invalid and semi-valid input checking", {
  expect_error(
    example_data_row_to_names[[1]] %>%
      row_to_names(row_number=1:2),
    regexp="row_number must be a numeric of length 1")
  for (nm in names(example_data_row_to_names)) {
    expect_warning(
      example_data_row_to_names[[1]] %>%
        row_to_names(row_number=1),
      regexp="Row 1 does not provide unique names. Consider running clean_names() after row_to_names()",
      info=paste("Unique name warning,", nm),
      fixed=TRUE)
  }
})

test_that("row_to_names factors come through as characters", {
  expect_equal(
    example_data_row_to_names$factor_data.frame %>%
      row_to_names(row_number=2) %>%
      names(),
    c("Title", "Title2"),
    info="Factors become character strings")
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
          info=paste0("With single-column data the result is single-column still (not a vector) and appropriate rows are dropped when requested with explicit information about remove_row=",
                      remove_row_flag,
                      " and remove_rows_above=",
                      remove_row_above_flag, ",", nm))
      }
    }
  }
})
