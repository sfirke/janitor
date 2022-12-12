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
      c("Lab_Test_Long", "Lab_Test_Short", "LOINC"),
      "Lab_Test_Short",
      "LOINC",
      "Person"
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
      c("Lab_Test_Long", "Lab_Test_Short", "LOINC"),
      "Lab_Test_Short",
      "LOINC",
      "Person"
    )
  )
})
