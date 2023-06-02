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

test_that("nearly duplicated dates (second decimal place differs) to not cause failure (#543)", {
  dates <- tibble::tibble(
    modification_time =
      structure(
        c(1684261364.85967, 1684274880.48328, 1684261364.85967, 1684418379.74664, 1685105253.21695, 1684418379.76668, 1684279133.50118, 1684161951.81434, 1684281651.93175, 1678483898.72893, 1685103626.03424),
        class = c("POSIXct", "POSIXt")
      ),
    access_time =
      structure(
        c(1685040222.34459, 1685041485.59089, 1685105067.68569, 1685040222.51569, 1685105253.21795, 1685105067.73877, 1685105253.66953, 1685106417.48391, 1685105253.66853, 1685041485.59089, 1685103652.82275),
        class = c("POSIXct", "POSIXt")
      ),
    change_time = structure(
      c(1684261364.85967, 1684274880.48328, 1684261364.85967, 1684418379.74664, 1685105253.21695, 1684418379.76668, 1684279133.50118, 1684161951.81434, 1684281651.93175, 1678483898.72893, 1685103626.03424),
      class = c("POSIXct", "POSIXt")
    )
  )
  expect_equal(
    janitor::get_one_to_one(dates),
    list(c("modification_time", "change_time"))
  )
})
