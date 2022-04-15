#' Perform join where the outcome of the join is verifed to match an expected
#' pattern.
#' 
#' @details Options for \code{x_control} and \code{y_control} are below and may be
#'   combined:
#'
#' \itemize{
#' \item{\code{"any"}: Any outcome is acceptable; this overrides all other options.}
#' \item{\code{"all"}: Each row from the input must appear in the output at least one
#'   time.}
#' \item{\code{"unique"}: A row may appear in the output zero or one time.}
#' \item{\code{"missing"}: At least one row must not match in the new dataset (the values
#'   must be missing).  This option is rarely used.}
#' \item{\code{"nomissing"}: All rows must match in the new dataset (the values must not
#'   be missing).}
#' }
#'
#' The combination of \code{x_control=c("all", "unique", "nomissing")} (or
#' \code{y_control}) is a common need to confirm that all values are present
#' exactly one time and that there are no missing values.
#' 
#' @param x,y tbls to join
#' @param join_fun Any function that can combine x and y (called as
#'   \code{join_fun(x, y, ...)}).  Typically this will be one of
#'   \code{dplyr::left_join}, \code{dplyr::right_join}, etc.
#' @param x_control,y_control What outcome is expected from the \code{x}, and
#'   \code{y} tbls? Default is "any" (see details).
#' @param x_fraction,y_fraction,x_count,y_count What fraction or count of the
#'   rows of \code{x} and \code{y} must be in the final data?  Fractions are
#'   converted to row counts by rounding up to the nearest integer.
#' @param overlap_fraction,overlap_count What fraction or count of the rows of
#'   the return value must overlap (i.e. have rows from both) \code{x} and
#'   \code{y}?  Fractions are converted to row counts by rounding up to the
#'   nearest integer.
#' @param ... Passed to \code{join_fun()} for \code{join_control()} and passed
#'   to \code{join_control()} for the other functions.
#' @return A joined tbl
#' @keywords Internal
#' @family Join control
#' @export
join_control <- function(x, y, join_fun,
                         x_control="any", y_control="any",
                         x_fraction=NA_real_, y_fraction=NA_real_,
                         x_count=NA_integer_, y_count=NA_integer_,
                         overlap_fraction=NA_real_, overlap_count=NA_integer_,
                         ...) {
  control_choices <- c("any", "all", "unique", "missing", "nomissing")
  x_control <-
    match.arg(
      x_control,
      choices=control_choices,
      several.ok=TRUE
    )
  y_control <-
    match.arg(
      y_control,
      choices=control_choices,
      several.ok=TRUE
    )
  x_count <- join_control_check_count_fraction(fraction=x_fraction, count=x_count, data=x, value_name="x")
  y_count <- join_control_check_count_fraction(fraction=y_fraction, count=y_count, data=y, value_name="y")
  if (all(c("missing", "nomissing") %in% x_control)) {
    stop("Both 'missing' and 'nomissing' may not be provided at the same time for `x_control`.")
  } else if (all(c("missing", "nomissing") %in% y_control)) {
    stop("Both 'missing' and 'nomissing' may not be provided at the same time for `y_control`.")
  }
  max_name <- max(c(names(x), names(y)))
  col_x_detect <- paste0(max_name, "x")
  col_y_detect <- paste0(max_name, "y")
  x[[col_x_detect]] <- seq_len(nrow(x))
  y[[col_y_detect]] <- seq_len(nrow(y))
  by_cols <-
    if ("by" %in% names(list(...))) {
      list(...)$by
    } else {
      intersect(names(x), names(y))
    }
  ret <- join_fun(x, y, ...)
  overlap_count <-
    join_control_check_count_fraction(
      fraction=overlap_fraction, count=overlap_count, data=ret,
      value_name="Returned data"
    )
  x_rows_detected <- sum(!is.na(ret[[col_x_detect]]))
  y_rows_detected <- sum(!is.na(ret[[col_y_detect]]))
  overlap_rows_detected <- sum(!is.na(ret[[col_x_detect]]) & !is.na(ret[[col_y_detect]]))
  if (x_rows_detected < x_count) {
    stop(
      sprintf(
        "Not enough rows from `x` are in the returned value (%g expected and %g found)",
        x_count, x_rows_detected
      )
    )
  } else if (y_rows_detected < y_count) {
    stop(
      sprintf(
        "Not enough rows from `y` are in the returned value (%g expected and %g found)",
        y_count, y_rows_detected
      )
    )
  } else if (overlap_rows_detected < overlap_count) {
    stop(
      sprintf(
        "Not enough overlapping rows `x` and `y` are in the returned value (%g expected and %g found)",
        overlap_count, overlap_rows_detected
      )
    )
  }
  join_control_detect(
    x=ret,
    control=x_control,
    detect_column=x[, col_x_detect, drop=FALSE],
    msg_prefix="x",
    by_cols=by_cols
  )
  join_control_detect(
    x=ret,
    control=y_control,
    detect_column=y[, col_y_detect, drop=FALSE],
    msg_prefix="y",
    by_cols=by_cols
  )
  ret[, setdiff(names(ret), c(col_x_detect, col_y_detect)), drop=FALSE]
}

#' @importFrom stats na.omit
join_control_detect <- function(x, control, detect_column, msg_prefix, by_cols) {
  # The na.omit() is because NA values are managed by "missing" and "nomissing"
  all_in <- all(detect_column[[1]] %in% stats::na.omit(x[[names(detect_column)]]))
  uniq_in <- !any(duplicated(na.omit(x[[names(detect_column)]])))
  missing_in <- any(is.na(x[[names(detect_column)]]))
  nomissing_in <- !missing_in
  if ("any" %in% control) {
    # do nothing
  } else {
    if ("all" %in% control & !all_in) {
      stop(
        "`", msg_prefix, "`: ",
        "All rows were are not in the new dataset. Missing rows: ",
        paste(
          setdiff(
            detect_column[[1]],
            x[[names(detect_column)]]
          ),
          collapse=", "
        )
      )
    }
    if ("unique" %in% control & !uniq_in) {
      print(
        unique(x[duplicated(x[[names(detect_column)]]), by_cols, drop=FALSE])
      )
      stop(
        "`", msg_prefix, "`: ",
        "Rows are not unique in the new dataset. Keys for duplicated rows are above."
      )
    }
    if ("missing" %in% control & !missing_in) {
      stop("`", msg_prefix, "`: No rows are missing in the new dataset.")
    }
    if ("nomissing" %in% control & !nomissing_in) {
      print(
        unique(x[is.na(x[[names(detect_column)]]), by_cols, drop=FALSE])
      )
      stop("`", msg_prefix, "`: Rows are missing in the new dataset. Keys for missing rows are above.")
    }
  }
  x
}

#' Convert the fraction or count into an integer number of rows required
#'
#' @param fraction See join_control x_fraction and y_fraction
#' @param count See join_control x_count and y_count
#' @param data The data that the fraction or count are based on
#' @param value_name The name of the parameter from join_control being input
#' @return An integer (including \code{NA_integer_} representing the number of
#'   rows from \code{fraction} or \code{count}.
#' @noRd
join_control_check_count_fraction <- function(fraction, count, data, value_name) {
  count_clean <- as.integer(count)
  fraction_clean <- as.double(fraction)
  if (is.na(count_clean) & !is.na(count)) {
    stop(sprintf("For %s: `count` was not a valid integer", value_name))
  } else if (is.na(fraction_clean) & !is.na(fraction)) {
    stop(sprintf("For %s: `fraction` was not a valid double", value_name))
  } else if (!is.na(count_clean) & !is.na(fraction_clean)) {
    stop(sprintf("For %s: Both `count` and `fraction` cannot be provided", value_name))
  } else if (!is.na(count_clean) && ((count_clean < 1) | (count_clean > nrow(data)))) {
    stop(sprintf("For %s: 'count' must be between 1 and nrow(data) (%d), if not NA.", value_name, nrow(data)))
  } else if (!is.na(fraction_clean) && ((fraction_clean <= 0) | (fraction_clean > 1))) {
    stop(sprintf("For %s: 'fraction' must be >0 and <=1", value_name))
  }
  if (is.na(fraction_clean)) {
    if (is.na(count_clean)) {
      # If neither is required, return 0 (as an integer)
      count_clean <- 0L
    }
    ret <- count_clean
  } else {
    # Make the fraction into a row count
    ret <- as.integer(ceiling(fraction_clean*nrow(data)))
    # It can't have more than the maximum number of rows in the data (to avoid
    # floating point errors with regard to rounding up)
    ret <- min(ret, nrow(data))
  }
  ret
}

#' @describeIn join_control For the common task of many-to-one mapping, the
#'   helper function `join_many_to_one()` works.
#' @importFrom dplyr left_join
#' @family Join control
#' @examples
#' x <- data.frame(A=rep(1:2, 2), B=1:4)
#' y_nomissing <- data.frame(A=1:2, C=1:2)
#' y_missing <- data.frame(A=1, C=1)
#' join_many_to_one(x, y_nomissing)
#' # The next line would give an error and point to the rows that fail
#' \dontrun{
#' join_many_to_one(x, y_missing)
#' }
#' @export
join_many_to_one <- function(x, y, ...) {
  join_control(
    x, y,
    join_fun=dplyr::left_join,
    x_control=c("all", "unique", "nomissing"),
    y_control="nomissing",
    ...
  )
}

#' @describeIn join_control For the common task of many-to-one mapping, the
#'   helper function `join_one_to_many()` works.
#' @importFrom dplyr left_join
#' @family Join control
#' @export
join_one_to_many <- function(x, y, ...) {
  join_control(
    x, y,
    join_fun=dplyr::left_join,
    x_control="nomissing",
    y_control=c("all", "unique", "nomissing"),
    ...
  )
}

#' @describeIn join_control For the common task of one-to-one mapping, the
#'   helper function `join_one_to_one()` works.
#' @importFrom dplyr left_join
#' @family Join control
#' @examples
#' x <- data.frame(A=rep(1:2, 2), B=1:4)
#' y <- data.frame(A=rep(1:2, 2), C=1:4)
#' z <- data.frame(B=1:4, C=5:8)
#' join_one_to_one(x, z)
#' # The next line would give an error and point to the rows that fail
#' \dontrun{
#' join_one_to_one(x, y)
#' }
#' @export
join_one_to_one <- function(x, y, ...) {
  join_control(
    x, y,
    join_fun=dplyr::left_join,
    x_control=c("all", "unique", "nomissing"),
    y_control=c("all", "unique", "nomissing"),
    ...
  )
}
