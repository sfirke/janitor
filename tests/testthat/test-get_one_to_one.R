test_that("get_one_to_one", {
  foo <- data.frame(
    Lab_Test_Long=c("Cholesterol, LDL", "Cholesterol, LDL", "Glucose"),
    Lab_Test_Short=c("CLDL", "CLDL", "GLUC"),
    LOINC=c(12345, 12345, 54321),
    Person=c("Sam", "Bill", "Sam"),
    stringsAsFactors=FALSE
  )
  expect_equal(
    get_one_to_one(foo),
    list(
      c("Lab_Test_Long", "Lab_Test_Short", "LOINC")
    )
  )
  # NA is respected as though it were any other value
  foo <- data.frame(
    Lab_Test_Long=c(NA, NA, "Glucose"),
    Lab_Test_Short=c("CLDL", "CLDL", "GLUC"),
    LOINC=c(12345, 12345, 54321),
    Person=c("Sam", "Bill", "Sam"),
    stringsAsFactors=FALSE
  )
  expect_equal(
    get_one_to_one(foo),
    list(
      c("Lab_Test_Long", "Lab_Test_Short", "LOINC")
    )
  )
})

test_that("get_one_to_one: columns are only described once", {
  expect_true(
    !any(duplicated(unlist(
      get_one_to_one(mtcars[1:3,])
    )))
  )
  expect_equal(
    get_one_to_one(mtcars[1:3,]),
    list(
      c("mpg", "cyl", "disp", "hp", "drat", "vs", "carb"),
      c("wt", "qsec"),
      c("am", "gear")
    )
  )
  # Ensure that single column outputs are dropped
  expect_equal(
    get_one_to_one(mtcars[1:5,]),
    list(
      c("mpg", "disp", "drat"),
      c("cyl", "hp"),
      c("am", "gear")
    )
  )
  expect_message(
    expect_equal(
      get_one_to_one(mtcars),
      list()
    ),
    regexp = "No columns in `mtcars` map to each other"
  )
})
