#' Convert many date and datetime formats as may be received from Microsoft
#' Excel
#'
#' @details Character conversion checks if it matches something that looks like
#'   a Microsoft Excel numeric date, converts those to numeric, and then runs
#'   convert_to_datetime_helper() on those numbers.  Then, character to Date or
#'   POSIXct conversion occurs via `character_fun(x, ...)` or
#'   `character_fun(x, tz=tz, ...)`, respectively.
#'
#' @param x The object to convert
#' @param tz The timezone for POSIXct output, unless an object is POSIXt
#'   already.  Ignored for Date output.
#' @param ... Passed to further methods.  Eventually may be passed to
#'   `excel_numeric_to_date()`, `base::as.POSIXct()`, or `base::as.Date()`.
#' @param character_fun A function to convert non-numeric-looking, non-NA values
#'   in `x` to POSIXct objects.
#' @param string_conversion_failure If a character value fails to parse into the
#'   desired class and instead returns `NA`, should the function return the
#'   result with a warning or throw an error?
#' @return POSIXct objects for `convert_to_datetime()` or Date objects for
#'   `convert_to_date()`.
#' @examples
#' convert_to_date("2009-07-06")
#' convert_to_date(40000)
#' convert_to_date("40000.1")
#' # Mixed date source data can be provided.
#' convert_to_date(c("2020-02-29", "40000.1"))
#' @export
#' @family Date-time cleaning
#' @importFrom lubridate ymd
convert_to_date <- function(x, ..., character_fun=lubridate::ymd, string_conversion_failure=c("error", "warning")) {
  string_conversion_failure <- match.arg(string_conversion_failure)
  convert_to_datetime_helper(
    x, ...,
    character_fun=character_fun,
    string_conversion_failure=string_conversion_failure,
    out_class="Date"
  )
}

#' @describeIn convert_to_date Convert to a date-time (POSIXct)
#' @examples
#' convert_to_datetime(
#'   c("2009-07-06", "40000.1", "40000", NA),
#'   character_fun=lubridate::ymd_h, truncated=1, tz="UTC"
#' )
#' @export
#' @importFrom lubridate ymd_hms
convert_to_datetime <- function(x, ..., tz="UTC", character_fun=lubridate::ymd_hms, string_conversion_failure=c("error", "warning")) {
  string_conversion_failure <- match.arg(string_conversion_failure)
  convert_to_datetime_helper(
    x, ...,
    tz=tz,
    character_fun=character_fun,
    string_conversion_failure=string_conversion_failure,
    out_class="POSIXct"
  )
}

#' The general method to convert either to a datetime or a date.
#' @param x the object to convert
#' @param out_class The class expected for output.
#' @return An object of class `out_class`
#' @noRd
convert_to_datetime_helper <- function(x, ..., out_class=c("POSIXct", "Date"))
  UseMethod("convert_to_datetime_helper")

convert_to_datetime_helper.numeric <- function(x, ...,
                                               date_system="modern",
                                               include_time=NULL,
                                               round_seconds=TRUE,
                                               tz="UTC",
                                               out_class=c("POSIXct", "Date")) {
  if (!is.null(include_time)) {
    warning("`include_time` is ignored in favor of `out_class`.")
  }
  out_class <- match.arg(out_class)
  excel_numeric_to_date(
    date_num=x,
    date_system="modern",
    round_seconds=round_seconds,
    tz=tz,
    include_time=out_class %in% "POSIXct"
  )
}

convert_to_datetime_helper.factor <- function(x, ..., out_class=c("POSIXct", "Date")) {
  convert_to_datetime_helper.character(as.character(x), ..., out_class=out_class)
}

convert_to_datetime_helper.POSIXt <- function(x, ..., out_class=c("POSIXct", "Date")) {
  out_class <- match.arg(out_class)
  if (out_class %in% "POSIXct") {
    # Ensure that POSIXlt gets converted to POSIXct
    as.POSIXct(x, ...)
  } else {
    as.Date(x, ...)
  }
}

convert_to_datetime_helper.Date <- function(x, ..., tz="UTC", out_class=c("POSIXct", "Date")) {
  out_class <- match.arg(out_class)
  if (out_class %in% "POSIXct") {
    ret <- as.POSIXct(x, ...)
    # as.POSIXct.Date ignores the time zone, so manually apply it.
    attr(ret, "tzone") <- tz
  } else {
    ret <- x
  }
  ret
}

convert_to_datetime_helper.character <- function(x, ..., tz="UTC", character_fun=lubridate::ymd_hms, string_conversion_failure=c("error", "warning"), out_class=c("POSIXct", "Date")) {
  string_conversion_failure <- match.arg(string_conversion_failure)
  out_class <- match.arg(out_class)
  mask_na <- is.na(x)
  mask_excel_numeric <- !mask_na & grepl(pattern="^[0-9]{5}(?:\\.[0-9]*)?$", x=x)
  mask_character <- !(mask_na | mask_excel_numeric)
  if (out_class %in% "POSIXct") {
    ret <- as.POSIXct(x=rep(NA, length(x)), tz="UTC")
  } else {
    ret <- as.Date(x=rep(NA, length(x)))
  }
  if (any(mask_excel_numeric)) {
    ret[mask_excel_numeric] <- convert_to_datetime_helper(as.numeric(x[mask_excel_numeric]), ..., tz=tz)
  }
  if (any(mask_character)) {
    characters_converted <-
      if (out_class %in% "POSIXct") {
        character_fun(x[mask_character], tz=tz, ...)
      } else {
        character_fun(x[mask_character], ...)
      }
    if (!(out_class %in% class(characters_converted))) {
      stop(
        "`character_fun(x)` must return class ", out_class,
        "; the returned class was: ", paste(class(characters_converted), collapse=", ")
      )
    }
    ret[mask_character] <- characters_converted
    if (any(is.na(ret[mask_character]))) {
      not_converted_values <- unique(x[mask_character & is.na(ret)])
      # Don't provide too many error values
      if (length(not_converted_values) > 10) {
        not_converted_values <-
          paste(
            paste0('"', not_converted_values[1:9], '"', collapse=", "),
            "... and", length(not_converted_values) - 9, "other values."
          )
      } else {
        not_converted_values <-
          paste0('"', not_converted_values, '"', collapse=", ")
      }
      not_converted_message <-
        paste0(
          "Not all character strings converted to class ", out_class,
          ".  Values not converted were: ",
          not_converted_values
        )
      if (string_conversion_failure %in% "error") {
        stop(not_converted_message)
      } else {
        warning(not_converted_message)
      }
    }
  }
  if (out_class %in% "POSIXct") {
    attr(ret, "tzone") <- tz
  }
  ret
}
