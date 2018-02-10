#' @title Generate a crosstabulation of two vectors.
#'
#' @description
#' This function is deprecated, use \code{tabyl(dat, var1, var2)} instead.
#'
#' @param vec1 the vector to place on the crosstab column.  If supplying a data.frame, this should be an unquoted column name.
#' @param vec2 the vector to place on the crosstab row.  If supplying a data.frame, this should be an unquoted column name.
#' @param percent which grouping to use for percentages, if desired (defaults to "none", which returns simple counts).  Must be one of "none", "row", "col", or "all".
#' @param show_na a logical value indicating whether counts should be displayed where either variable is \code{NA}.
#' @return Returns a data.frame with the frequencies of the crosstabulated variables.
#' @export

crosstab <- function(...) {
  .Deprecated("tabyl(dat, var1, var2, ...)")
  UseMethod("crosstab")
}

#' @inheritParams crosstab
#' @rdname crosstab
#' @export
crosstab.default <- function(vec1, vec2, percent = "none", show_na = TRUE, ...) {
  if (!mode(vec1) %in% c("logical", "numeric", "character", "list") | is.matrix(vec1)) {
    stop("vec1 must be a vector of type logical, numeric, character, list, or factor")
  }
  if (!mode(vec2) %in% c("logical", "numeric", "character", "list") | is.matrix(vec2)) {
    stop("vec2 must be a vector of type logical, numeric, character, list, or factor")
  }

  if (!percent %in% c("none", "row", "col", "all")) {
    stop("'percent' must be one of 'none', 'row', 'col', or 'all'")
  }

  if (length(vec1) != length(vec2)) {
    stop("the two vectors are not the same length")
  }

  dat <- data.frame(
    vec1 = vec1,
    vec2 = vec2,
    stringsAsFactors = FALSE
  )

  dat_col_names <- names(dat)

  if (is.null(names(vec1))) {
    var_name <- deparse(substitute(vec1))
  } else {
    var_name <- names(vec1)
  }

  # an odd variable name can be deparsed into a vector of length >1, rare but breaks function, see issue #87
  if (length(var_name) > 1) {
    var_name <- paste(var_name, collapse = "")
  }

  if (!show_na) {
    dat <- dat[!is.na(dat[[1]]) & !is.na(dat[[2]]), ]
  }

  # create long data.frame with initial counts
  tabl <- dat %>%
    dplyr::count_(dat_col_names) %>%
    dplyr::ungroup()


  # replace NA with string NA_ in vec2 to avoid invalid col name after spreading
  # if this col is a factor, need to add that level to the factor
  if (is.factor(tabl[[2]])) {
    levels(tabl[[2]]) <- c(levels(tabl[[2]]), "NA_")
  }
  tabl[2][is.na(tabl[2])] <- "NA_"

  # spread to wide, ungroup() for cleanliness of result, and rename 1st col
  result <- tabl %>%
    tidyr::spread_(dat_col_names[[2]], "n", fill = 0) %>%
    dplyr::ungroup()

  if ("NA_" %in% names(result)) {
    result <- result[c(setdiff(names(result), "NA_"), "NA_")]
  } # move NA_ column to end, from http://stackoverflow.com/a/18339562
  # calculate percentages, if specified
  if (percent != "none") {
    result <- adorn_percentages(result, denominator = percent)
  }

  result %>%
    stats::setNames(., c(var_name, names(.)[-1])) %>%
    data.frame(., check.names = FALSE) %>%
    as_tabyl()
}

#' @inheritParams crosstab.default
#' @param .data (optional) a data.frame, in which case \code{vec1} and \code{vec2} should be unquoted column names.
#' @param ... additional arguments, if calling \code{crosstab} on a data.frame.
#' @rdname crosstab
#' @export
crosstab.data.frame <- function(.data, ...) {
  # collect dots
  dots <- as.list(substitute(list(...)))[-1L] #
  n <- length(dots)

  # select columns from .data
  columns <- dots[1:2]
  if (dots[[1]] == dots[[2]]) {
    stop("the same column name is specified for both input variables.  Use tabyl() for tabulating a single variable")
  }

  x <- list()
  x[[deparse(columns[[1]])]] <- .data[, deparse(columns[[1]])]
  x[[deparse(columns[[2]])]] <- .data[, deparse(columns[[2]])]
  x <- data.frame(x,
                  stringsAsFactors = FALSE,
                  # preserve bad input names
                  check.names = FALSE
  )

  # create args list to use with do.call
  arguments <- list()

  if (n > 2) arguments <- dots[3:n]

  arguments$vec1 <- x[1]
  arguments$vec2 <- x[2]

  do.call(crosstab.default,
    args = arguments
  )
}
