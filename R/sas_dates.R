#' Convert a SAS date, time or date/time to an R object
#' 
#' @inheritParams excel_numeric_to_date
#' @param datetime_num numeric vector of date/time numbers (seconds since
#'   midnight 1960-01-01) to convert
#' @param time_num numeric vector of time numbers (seconds since midnight on the
#'   current day) to convert
#' @return If a date and time or datetime are provided, a POSIXct object.  If a
#'   date is provided, a Date object.  If a time is provided, an hms::hms object
#' @references SAS Date, Time, and Datetime Values reference (retrieved on
#'   2022-03-08): https://v8doc.sas.com/sashtml/lrcon/zenid-63.htm
#' @examples
#' sas_numeric_to_date(date_num=15639) # 2002-10-26
#' sas_numeric_to_date(datetime_num=1217083532, tz="UTC") # 1998-07-26T14:45:32Z
#' sas_numeric_to_date(date_num=15639, time_num=3600, tz="UTC") # 2002-10-26T01:00:00Z
#' sas_numeric_to_date(time_num=3600) # 01:00:00
#' @family Date-time cleaning
#' @export
sas_numeric_to_date <- function(date_num, datetime_num, time_num, tz = "UTC") {
  # Confirm that a usable set of input arguments is given
  has_date <- !missing(date_num)
  has_datetime <- !missing(datetime_num)
  has_time <- !missing(time_num)
  stopifnot(is.character(tz))
  stopifnot(length(tz) == 1)
  if (tz != "UTC") {
    warning("SAS may not properly store timezones other than UTC. Consider confirming the accuracy of the resulting data.")
  }
  if (has_date & has_datetime) {
    stop("Must not give both `date_num` and `datetime_num`")
  } else if (has_time & has_datetime) {
    stop("Must not give both `time_num` and `datetime_num`")
  }
  if (has_time) {
    stopifnot("`time_num` must be non-negative"=all(is.na(time_num) | time_num >= 0))
    # Note the value of 86400 is allowed by the SAS standard listed in the
    # references section
    stopifnot("`time_num` must be within the number of seconds in a day (<= 86400)"=all(is.na(time_num) | time_num <= 86400))
  }
  if (has_date & has_time) {
    mask_na_match <- is.na(date_num) == is.na(time_num)
    if (!all(mask_na_match)) {
      stop("The same values are not NA for both `date_num` and `time_num`")
    }
    datetime_num <- 86400 * date_num + time_num
    has_datetime <- TRUE
  }
  if (has_datetime) {
    ret <- as.POSIXct(datetime_num, origin = "1960-01-01", tz = tz)
  } else if (has_date) {
    ret <- as.Date(date_num, origin="1960-01-01")
  } else if (has_time) {
    ret <- hms::hms(seconds=time_num)
  } else {
    stop("Must give one of `date_num`, `datetime_num`, `time_num`, or `date_num` and `time_num`")
  }
  ret
}
