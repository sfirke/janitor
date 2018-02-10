#' @title Convert dates encoded as serial numbers to Date class.
#'
#' @description
#' Converts numbers like \code{42370} into date values like \code{2016-01-01}.
#'
#' Defaults to the modern Excel date encoding system. However, Excel for Mac 2008 and earlier Mac versions of Excel used a different date system. To determine what platform to specify: if the date 2016-01-01 is represented by the number 42370 in your spreadsheet, it's the modern system.  If it's 40908, it's the old Mac system.
#' More on date encoding systems at http://support.office.com/en-us/article/Date-calculations-in-Excel-e7fe7167-48a9-4b96-bb53-5612a800b487.
#'
#' @param date_num numeric vector of serial numbers to convert.
#' @param date_system the date system, either \code{"modern"} or \code{"mac pre-2011"}.
#' @return Returns a vector of class Date.
#' @export
#' @examples
#' excel_numeric_to_date(40000)

# Converts a numeric value like 42414 into a date "2016-02-14"

excel_numeric_to_date <- function(date_num, date_system = "modern") {
  if (!is.numeric(date_num)) {
    stop("argument `date_num` must be of class numeric")
  }

  if (date_system == "mac pre-2011") {
    as.Date(date_num, origin = "1904-01-01")
  } else if (date_system == "modern") {
    as.Date(date_num, origin = "1899-12-30")
  } else {
    stop("argument 'created' must be one of 'mac pre-2011' or 'modern'")
  }
}
