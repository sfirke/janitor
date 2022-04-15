#' @title Add column name to the top of a two-way tabyl.
#'
#' @description
#' This function adds the column variable name to the top of a \code{tabyl} for a complete display of information.  This makes the tabyl prettier, but renders the data.frame less useful for further manipulation.
#'
#' @param dat a data.frame of class \code{tabyl} or other data.frame with a tabyl-like layout.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).
#' @param placement whether the column name should be added to the top of the tabyl in an otherwise-empty row \code{"top"} or appended to the already-present row name variable (\code{"combined"}).  The formatting in the \code{"top"} option has the look of base R's \code{table()}; it also wipes out the other column names, making it hard to further use the data.frame besides formatting it for reporting.  The \code{"combined"} option is more conservative in this regard.
#' @param row_name (optional) default behavior is to pull the row name from the attributes of the input \code{tabyl} object.  If you wish to override that text, or if your input is not a \code{tabyl}, supply a string here.
#' @param col_name (optional) default behavior is to pull the column_name from the attributes of the input \code{tabyl} object.  If you wish to override that text, or if your input is not a \code{tabyl}, supply a string here.
#' @return the input tabyl, augmented with the column title.  Non-tabyl inputs that are of class \code{tbl_df} are downgraded to basic data.frames so that the title row prints correctly.
#'
#' @export
#' @examples
#'
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_title(placement = "top")
#'
#' # Adding a title to a non-tabyl
#' library(tidyr); library(dplyr)
#' mtcars %>%
#'   group_by(gear, am) %>%
#'   summarise(avg_mpg = mean(mpg), .groups = "drop") %>%
#'   spread(gear, avg_mpg) %>%
#'   adorn_rounding() %>%
#'   adorn_title("top", row_name = "Gears", col_name = "Cylinders")

adorn_title <- function(dat, placement = "top", row_name, col_name) {
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    purrr::map(dat, adorn_title, placement, row_name, col_name)
  } else {
    if (!is.data.frame(dat)) {
      stop("\"dat\" must be a data.frame")
    }
    if (!placement %in% c("top", "combined")) {
      stop("\"placement\" must be one of \"top\" or \"combined\"")
    }
    if ("tabyl" %in% class(dat)) {
      if (attr(dat, "tabyl_type") == "one_way") {
        warning("adorn_title is meant for two-way tabyls, calling it on a one-way tabyl may not yield a meaningful result")
      }
    }
    if (missing(col_name)) {
      if (!"tabyl" %in% class(dat)) {
        stop("When input is not a data.frame of class tabyl, a value must be specified for the col_name argument")
      }
      col_var <- attr(dat, "var_names")$col
    } else {
      if (!is.character(col_name)) {
        stop("col_name must be a string")
      }
      col_var <- col_name
    }

    if (!missing(row_name)) {
      if (!is.character(row_name)) {
        stop("row_name must be a string")
      }
      names(dat)[1] <- row_name
      row_var <- row_name
    } else {
      if ("tabyl" %in% class(dat)) {
        row_var <- attr(dat, "var_names")$row
      } else {
        row_var <- names(dat)[1] # for non-tabyl input, if no row_name supplied, use first existing name
      }
    }


    if (placement == "top") {
      dat[, ] <- lapply(dat[, ], as.character) # to handle factors, problematic in first column and at bind_rows.
      # Can't use mutate_all b/c it strips attributes
      top <- dat[1, ]
      
      top[1, ] <- as.list(names(top))

      out <- dplyr::bind_rows(top, dat)
      out <- stats::setNames(out, c("", col_var, rep("", ncol(out) - 2)))
    }
    if (placement == "combined") {
      out <- dat
      names(out)[1] <- paste(row_var, col_var, sep = "/")
    }
    if ("tbl_df" %in% class(out)) { # "top" text doesn't print if input (and thus the output) is a tibble
      out <- as.data.frame(out) # but this prints row numbers, so don't apply to non-tbl_dfs like tabyls
    }
    out
  }
}
