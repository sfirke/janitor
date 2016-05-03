#' @title Convert common NA string values to true \code{NA} values, throughout a data.frame.
#'
#' @description
#' Converts strings like "#N/A", and any user-specified strings, into \code{NA}.
#'
#' Default values to convert to NA are "NA", "#N/A", "N/A", "n/a", "#NAME?".  You can add your own additional values via a character vector; those values will also be converted to \code{NA}.
#'
#' @param dat data.frame to operate on.
#' @param addl_strings an optional character vector of additional strings to convert.
#' @return Returns a data.frame (or tbl_df if a tbl_df was supplied).
#' @export
#' @examples
#' clean_NA_variants(mtcars, "4") # a silly example;
#' # mtcars has no string NA values, but this will convert 4s to NA


clean_NA_variants <- function(dat, addl_strings = NULL){
  if(!class(addl_strings) %in% c("NULL", "character")){ stop("addl_strings parameter should be a character vector, if specified")}
  custom_na_vals <- c("NA", "#N/A", "N/A", "n/a", "#NAME?", addl_strings)

  # replace character values with NA - use loop instead of apply to retain df class (data.frame or tbl_df)
  for(i in seq_along(dat)){
    dat[[i]] <- clean_NA_vec(dat[[i]], na_vals = custom_na_vals)
  }
  dat
}

#' @title Turn common NA string values in a vector into true \code{NA} values.
#'
#' @description
#' Converts strings like "#N/A", and any user-specified strings, into \code{NA}.  Operates at vector level.
#'
#' Supply a vector \code{na_vals} of strings to convert to \code{NA}.
#'
#' @param vec vector to operate on.
#' @param na_vals a character vector of strings to convert.
#' @return Returns a vector.
#' @keywords internal
#' @examples
#' # not run:
#' # clean_NA_vec(letters, c("b", "d"))
clean_NA_vec <- function(vec, na_vals) {
  vec[vec %in% na_vals] <- NA
  vec
}