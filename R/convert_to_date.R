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
#'   `excel_numeric_to_date()`, `base::as.POXIXct()`, or `base::as.Date()`.
#' @param character_fun A function to convert non-numeric-looking, non-NA values
#'   in `x` to POSIXct objects.
#' @param allow_na_conversion Can `character_fun(x)` for the
#'   non-numeric-looking, non-NA values return NA?
#' @return POSIXct objects for `convert_to_datetime()` or Date objects for
#'   `convert_to_date()`.
#' @examples
#' convert_to_date("2009-07-06")
#' convert_to_date(40000)
#' convert_to_date("40000.1")
#' @export
#' @family Date conversion
#' @importFrom lubridate ymd
convert_to_date <- function(x, ..., character_fun=lubridate::ymd, allow_na_conversion=FALSE) {
  convert_to_datetime_helper(
    x, ...,
    character_fun=character_fun,
    allow_na_conversion=allow_na_conversion,
    out_class="Date"
  )
}

#' @describeIn convert_to_date Convert to a date-time (POSIXct)
#' @examples
#' convert_to_datetime(c("2009-07-06", "40000.1", "40000", NA), character_fun=lubridate::ymd_h, truncated=1, tz="UTC")
#' @export
#' @importFrom lubridate ymd_hms
convert_to_datetime <- function(x, ..., tz="UTC", character_fun=lubridate::ymd_hms, allow_na_conversion=FALSE) {
  convert_to_datetime_helper(
    x, ...,
    tz=tz,
    character_fun=character_fun,
    allow_na_conversion=allow_na_conversion,
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

convert_to_datetime_helper.character <- function(x, ..., tz="UTC", character_fun=lubridate::ymd_hms, allow_na_conversion=FALSE, out_class=c("POSIXct", "Date")) {
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
      not_converted_message <-
        paste0(
          "Not all character strings converted to class ", out_class,
          ".  Values not converted were: ",
          paste0('"', not_converted_values, '"', collapse=", ")
        )
      if (!allow_na_conversion) {
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
