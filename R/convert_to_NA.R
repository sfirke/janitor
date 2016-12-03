#' @title Convert string values to true \code{NA} values.
#'
#' @description
#' Converts instances of user-specified strings into \code{NA}.  Can operate on either a single vector or an entire data.frame.
#'
#' @section Warning: Deprecated, do not use in new code. Use \code{dplyr::na_if()} instead.
#' @param dat vector or data.frame to operate on.
#' @param strings character vector of strings to convert.
#' @return Returns a cleaned object.  Can be a vector, data.frame, or \code{tibble::tbl_df} depending on the provided input.
#' @seealso janitor_deprecated
#' @export
# EXCLUDE COVERAGE START
convert_to_NA <- function(dat, strings){
  .Deprecated("dplyr::na_if()")
  
  if(!class(strings) %in% c("character", "numeric", "factor", "integer")){ stop("'strings' parameter should be a vector of class character, numeric, factor, or integer") }

    # helper function: converts instances of user-specified strings into NA
  clean_NA_vec <- function(vec, na_vals) {
    vec[vec %in% na_vals] <- NA
    vec
  }

  if(is.vector(dat) & !is.list(dat)){ # handle vector/list case, otherwise proceed for df
    result <- clean_NA_vec(dat, strings)
    if(identical(dat, result)){ warning("no replacements made") }
    result
  } else if(is.data.frame(dat)){
    # replace character values with NA - use loop instead of apply to retain df class (data.frame or tbl_df)
    result <- dat
    for(i in seq_along(result)){
      result[[i]] <- clean_NA_vec(result[[i]], na_vals = strings)
    }
    if(identical(dat, result)){ warning("no replacements made") }
    result
  } else{ stop("argument 'dat' must be a vector or data.frame") }
}
# EXCLUDE COVERAGE END