test_that("paste_skip_na", {
  # handle no arguments the same as paste()
  expect_equal(paste_skip_na(), paste())
  expect_equal(paste_skip_na(NA), NA_character_)
  expect_equal(paste_skip_na(NA, NA), NA_character_)
  expect_equal(paste_skip_na(NA, NA, sep = ","), NA_character_)
  # It does not behave like paste(NA, NA, collapse = ",") nor does it behave like paste(c(), collapse = ",")
  expect_equal(paste_skip_na(NA, NA, collapse = ","), NA_character_)

  expect_equal(paste_skip_na("A", NA), "A")
  expect_equal(paste_skip_na("A", NA, collapse = ","), "A")
  expect_equal(paste_skip_na("A", NA, c(NA, "B"), collapse = ","), "A,A B")
  expect_equal(paste_skip_na("A", NA, c(NA, "B"), sep = ","), c("A", "A,B"))
})
