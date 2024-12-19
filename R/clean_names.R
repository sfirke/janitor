#' @title Cleans names of an object (usually a data.frame).
#'
#' @description
#' Resulting names are unique and consist only of the `_` character, numbers, and letters.
#' Capitalization preferences can be specified using the `case` parameter.
#'
#' Accented characters are transliterated to ASCII.  For example, an "o" with a
#' German umlaut over it becomes "o", and the Spanish character "enye" becomes
#' "n".
#'
#' This function takes and returns a data.frame, for ease of piping with
#' `%>%`. For the underlying function that works on a character vector
#' of names, see [janitor::make_clean_names()].  `clean_names`
#' relies on the versatile function [snakecase::to_any_case()], which
#' accepts many arguments.  See that function's documentation for ideas on getting
#' the most out of `clean_names`.  A few examples are included below.
#'
#' A common issue is that the micro/mu symbol is replaced by "m" instead of "u".
#' The replacement with "m" is more correct when doing Greek-to-ASCII
#' transliteration but less correct when doing scientific data-to-ASCII
#' transliteration.  A warning will be generated if the "m" replacement occurs.
#' To replace with "u", please add the argument `replace=janitor:::mu_to_u`
#' which is a character vector mapping all known mu or micro Unicode code points
#' (characters) to "u".
#'
#' @param dat The input `data.frame`.
#' @param set_labels If set to `TRUE`, old names are stored as labels in each column of the returned data.frame.
#' @inheritDotParams make_clean_names -string
#' @return A `data.frame` with clean names.
#'
#' @details `clean_names()` is intended to be used on `data.frames`
#'   and `data.frame`-like objects. For this reason there are methods to
#'   support using `clean_names()` on `sf` and `tbl_graph` (from
#'   `tidygraph`) objects as well as on database connections through
#'   `dbplyr`. For cleaning other named objects like named lists
#'   and vectors, use `make_clean_names()`. When `set_labels` is set to `TRUE`, the old names,
#'   stored as column labels, can be restored using `sjlabelled::label_to_colnames()`.
#'
#' @export
#' @family Set names
#' @examples
#'
#' # --- Simple Usage ---
#' x <- data.frame(caseID = 1, DOB = 2, Other = 3)
#' clean_names(x)
#'
#' # or pipe in the input data.frame:
#' x %>%
#'   clean_names()
#'
#' # if you prefer camelCase variable names:
#' x %>%
#'   clean_names(., "lower_camel")
#'
#' # (not run) run clean_names after reading in a spreadsheet:
#' # library(readxl)
#' # read_excel("messy_excel_file.xlsx") %>%
#' #   clean_names()
#'
#' # --- Taking advantage of the underlying snakecase::to_any_case arguments ---
#'
#' # Restore column names to Title Case, e.g., for plotting
#' mtcars %>%
#'   clean_names(case = "title")
#'
#' # Tell clean_names to leave certain abbreviations untouched:
#' x %>%
#'   clean_names(case = "upper_camel", abbreviations = c("ID", "DOB"))
#'
clean_names <- function(dat, ...) {
  UseMethod("clean_names")
}

#' @rdname clean_names
#' @export
clean_names.default <- function(dat, ..., set_labels = FALSE) {
  if (is.null(names(dat)) && is.null(dimnames(dat))) {
    stop(
      "`clean_names()` requires that either names or dimnames be non-null.",
      call. = FALSE
    )
  }
  if (is.null(names(dat))) {
    dimnames(dat) <- lapply(dimnames(dat), make_clean_names, ...)
  } else {
    if (set_labels) {
      old_names <- names(dat)
      for (i in seq_along(old_names)) {
        attr(dat[[i]], "label") <- old_names[[i]]
      }
    }
    names(dat) <- make_clean_names(names(dat), ...)
  }
  dat
}

#' @rdname clean_names
#' @export
clean_names.sf <- function(dat, ..., set_labels = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) { # nocov start
    stop(
      "Package 'sf' needed for this function to work. Please install it.",
      call. = FALSE
    )
  } # nocov end
  # get old names
  sf_names <- names(dat)
  # Clean the names except for the "sf_column" which is used internally by sf
  cols_to_rename <- which(!(sf_names %in% attr(dat, "sf_column")))
  # clean all but last column
  sf_cleaned <- make_clean_names(sf_names[cols_to_rename], ...)
  # rename original df
  names(dat)[cols_to_rename] <- sf_cleaned

  if (set_labels) {
    for (i in seq_along(sf_names[cols_to_rename])) {
      attr(dat[[i]], "label") <- sf_names[[i]]
    }
  }

  dat
}

#' @rdname clean_names
#' @export
clean_names.tbl_graph <- function(dat, ...) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) { # nocov start
    stop(
      "Package 'tidygraph' needed for this function to work. Please install it.",
      call. = FALSE
    )
  } # nocov end

  dplyr::rename_all(dat, .funs = make_clean_names, ...)
}

#' @rdname clean_names
#' @export
clean_names.tbl_lazy <- function(dat, ...) {
  if (!requireNamespace("dbplyr", quietly = TRUE)) { # nocov start
    stop(
      "Package 'dbplyr' needed for this function to work. Please install it.",
      call. = FALSE
    )
  } # nocov end
  dplyr::rename_with(dat, janitor::make_clean_names, .cols = dplyr::everything(), ...)
}


# TODO: According to https://www.compart.com/en/unicode/U+03BC reviewed on
# 2021-07-10, there are some UTF-32 encoding characters that are also mu or
# micro.  This only handles the utf-8 values; to add more characters, just add
# to this character vector.

#' Constant to help map from mu to u
#'
#' This is a character vector with names of all known Unicode code points that
#' look like the Greek mu or the micro symbol and values of "u".  This is
#' intended to simplify mapping from mu or micro in Unicode to the character "u"
#' with `clean_names()` and `make_clean_names()`.
#'
#' See the help in `clean_names()` for how to use this.
#'
#' @family Set names
mu_to_u <-
  # setNames is used instead of setting the names directly because it prevents a
  # warning like "unable to translate '<U+3382>' to native encoding" for several
  # of the items.
  setNames(
    rep("u", 10),
    nm =
      c(
        "\u00b5", "\u03bc", "\u3382", "\u338c", "\u338d",
        "\u3395", "\u339b", "\u33b2", "\u33b6", "\u33bc"
      )
  )
